use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use neco_rs_parser::{NativeLinkMode, ParsedPackage, ParsedRoot, parse_root};

use crate::cli::{default_output_path, select_binary_from_package};
use crate::codegen::{
    EntryAbi, build_linux_x86_64_program_executable, build_linux_x86_64_program_image,
};
use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, ComparisonKind, CompiledPtxArtifact,
    ConditionExpr, ExitCodeExpr, F32Expr, I32Expr, I64Expr, KernelArgumentRef, LoweredProgram,
    OpenPath, Operation, PathBufSource, U8Expr,
};
use crate::lowering::lower_package_to_program;
use crate::write_compiled_ptx_artifacts;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

fn unique_temp_dir(name: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock before unix epoch")
        .as_nanos();
    std::env::temp_dir().join(format!("neco-rs-test-{name}-{unique}"))
}

fn selected_fixture_package(root: &Path, binary_name: &str) -> ParsedPackage {
    let ParsedRoot::Package(package) = parse_root(root).expect("fixture parses") else {
        panic!("expected package root");
    };
    select_binary_from_package(package, &PathBuf::from(binary_name)).expect("select binary")
}

fn operations_contain_static_data_get(operations: &[Operation]) -> bool {
    operations.iter().any(|operation| match operation {
        Operation::ArraySetU8 {
            value: U8Expr::StaticDataGet { .. },
            ..
        } => true,
        Operation::If {
            then_operations,
            else_operations,
            ..
        } => {
            operations_contain_static_data_get(then_operations)
                || operations_contain_static_data_get(else_operations)
        }
        Operation::Loop { body_operations } => operations_contain_static_data_get(body_operations),
        _ => false,
    })
}

fn parse_inline_binary_package(name: &str, source: &str) -> ParsedPackage {
    let source_path = PathBuf::from("src/main.fe");
    let (tokens, syntax) = neco_rs_parser::parse_source(source).expect("parse source");
    let root_dir = repo_root().join("tests/testcases").join(name);
    ParsedPackage {
        manifest_path: root_dir.join("neco-package.json"),
        root_dir,
        manifest: neco_rs_parser::PackageManifest {
            name: name.to_string(),
            dependencies: Vec::new(),
            felis_lib_entrypoint: None,
            felis_bin_entrypoints: vec![source_path.clone()],
            native_link_mode: NativeLinkMode::KernelStart,
            native_libraries: Vec::new(),
        },
        source_files: vec![neco_rs_parser::ParsedSourceFile {
            path: source_path,
            role: neco_rs_parser::SourceFileRole::BinaryEntrypoint,
            tokens,
            syntax: syntax.expect("source file syntax"),
        }],
    }
}

#[test]
fn lowers_stdin_to_stdout_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/stdin-to-stdout");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 1000,
            element_type: ArrayElementType::U8,
            kind: ArrayKind::Fixed,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::Read {
                fd: I32Expr::Literal(0),
                array_slot: 0,
                len: I32Expr::Literal(1000),
                result_slot: 0,
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 0,
                len: I32Expr::Local(0),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 1);
}

#[test]
fn lowers_cuda_cu_init_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/cuda-cu-init");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::CuInit {
                flags: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(0))),
        ]
    );
    assert_eq!(program.i32_slots, 1);
}

#[test]
fn lowers_cuda_device_ctx_create_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/cuda-cu-device-ctx-create");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::CuInit {
                flags: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::StoreI32 {
                slot: 1,
                value: I32Expr::Literal(0),
            },
            Operation::CuDeviceGet {
                device_slot: 1,
                ordinal: I32Expr::Literal(0),
                result_slot: 2,
            },
            Operation::StoreI64 {
                slot: 0,
                value: I64Expr::Literal(0),
            },
            Operation::CuCtxCreateV2 {
                ctx_slot: 0,
                flags: I32Expr::Literal(0),
                device: I32Expr::Local(1),
                result_slot: 3,
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(3))),
        ]
    );
    assert_eq!(program.i32_slots, 4);
    assert_eq!(program.i64_slots, 1);

    let image = build_linux_x86_64_program_image(&program, EntryAbi::LibcMain);
    assert_eq!(
        image
            .external_calls
            .iter()
            .map(|call| call.symbol)
            .collect::<Vec<_>>(),
        vec!["cuInit", "cuDeviceGet", "cuCtxCreate_v2"]
    );
}

#[test]
fn lowers_cuda_compile_ptx_module_load_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/cuda-compile-ptx-module-load");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(program.data.len(), 2);
    let ptx = String::from_utf8_lossy(&program.data[0]);
    assert!(ptx.contains(".visible .entry empty_kernel("));
    assert!(ptx.contains(".param .u64 arg0"));
    assert!(ptx.contains("ld.global.u32"));
    assert!(ptx.contains("st.global.u32"));
    assert!(program.data[0].ends_with(&[0]));
    assert_eq!(program.data[1], b"empty_kernel\0");
    assert_eq!(
        program.operations,
        vec![
            Operation::CuInit {
                flags: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::StoreI32 {
                slot: 1,
                value: I32Expr::Literal(0),
            },
            Operation::CuDeviceGet {
                device_slot: 1,
                ordinal: I32Expr::Literal(0),
                result_slot: 2,
            },
            Operation::StoreI64 {
                slot: 0,
                value: I64Expr::Literal(0),
            },
            Operation::CuCtxCreateV2 {
                ctx_slot: 0,
                flags: I32Expr::Literal(0),
                device: I32Expr::Local(1),
                result_slot: 3,
            },
            Operation::StoreI64 {
                slot: 1,
                value: I64Expr::Literal(0),
            },
            Operation::CuModuleLoadData {
                module_slot: 1,
                data_index: 0,
                result_slot: 4,
            },
            Operation::StoreI64 {
                slot: 2,
                value: I64Expr::Literal(0),
            },
            Operation::CuModuleGetFunction {
                function_slot: 2,
                module: I64Expr::Local(1),
                name_data_index: 1,
                result_slot: 5,
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: I32Expr::Literal(7),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: I32Expr::Literal(0),
            },
            Operation::CuMemAllocV2 {
                array_slot: 1,
                len: I32Expr::Literal(2),
                result_slot: 6,
            },
            Operation::CuMemcpyHtoDV2 {
                dest_slot: 1,
                source_slot: 0,
                len: I32Expr::Literal(2),
                result_slot: 7,
            },
            Operation::CuLaunchKernel {
                function: I64Expr::Local(2),
                arg: KernelArgumentRef::ArrayPtx(1),
                grid_dim_x: I32Expr::Literal(1),
                grid_dim_y: I32Expr::Literal(1),
                grid_dim_z: I32Expr::Literal(1),
                block_dim_x: I32Expr::Literal(1),
                block_dim_y: I32Expr::Literal(1),
                block_dim_z: I32Expr::Literal(1),
                shared_mem_bytes: I32Expr::Literal(0),
                stream: I64Expr::Literal(0),
                result_slot: 8,
            },
            Operation::CuMemcpyDtoHV2 {
                dest_slot: 0,
                source_slot: 1,
                len: I32Expr::Literal(2),
                result_slot: 9,
            },
            Operation::If {
                condition: ConditionExpr::I32 {
                    kind: ComparisonKind::Eq,
                    lhs: I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I64Expr::Literal(1)),
                    },
                    rhs: I32Expr::Literal(7),
                },
                then_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0)))],
                else_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::ArrayGet {
                    array_slot: 0,
                    index: Box::new(I64Expr::Literal(1)),
                }))],
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
    assert_eq!(program.i32_slots, 10);
    assert_eq!(program.i64_slots, 3);

    let image = build_linux_x86_64_program_image(&program, EntryAbi::LibcMain);
    assert_eq!(
        image
            .external_calls
            .iter()
            .map(|call| call.symbol)
            .collect::<Vec<_>>(),
        vec![
            "cuInit",
            "cuDeviceGet",
            "cuCtxCreate_v2",
            "cuModuleLoadData",
            "cuModuleGetFunction",
            "cuMemAlloc_v2",
            "cuMemcpyHtoD_v2",
            "cuLaunchKernel",
            "cuMemcpyDtoH_v2"
        ]
    );
}

#[test]
fn compiles_i64_ptx_arithmetic() {
    let root = repo_root().join("tests/testcases/compile-ptx-arithmetic");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower package");
    let ptx = String::from_utf8_lossy(&program.data[0]);
    assert!(ptx.contains(".visible .entry i64_kernel("));
    assert!(ptx.contains(".reg .u64 %rd<"));
    assert!(ptx.contains("ld.global.u64"));
    assert!(ptx.contains("add.s64"));
    assert!(ptx.contains("sub.s64"));
    assert!(ptx.contains("mul.lo.s64"));
    assert!(ptx.contains("div.s64"));
    assert!(ptx.contains("st.global.u64"));
}

#[test]
fn compiles_f32_ptx_arithmetic() {
    let root = repo_root().join("tests/testcases/compile-ptx-arithmetic");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower package");
    assert_eq!(
        program.compiled_ptx,
        vec![
            CompiledPtxArtifact {
                data_index: 0,
                function_name: "i64_kernel".to_string(),
            },
            CompiledPtxArtifact {
                data_index: 1,
                function_name: "f32_kernel".to_string(),
            }
        ]
    );
    let ptx = String::from_utf8_lossy(&program.data[1]);
    assert!(ptx.contains(".visible .entry f32_kernel("));
    assert!(ptx.contains(".reg .f32 %f<"));
    assert!(ptx.contains("ld.global.f32"));
    assert!(ptx.contains("add.rn.f32"));
    assert!(ptx.contains("sub.rn.f32"));
    assert!(ptx.contains("mul.rn.f32"));
    assert!(ptx.contains("div.rn.f32"));
    assert!(ptx.contains("st.global.f32"));
}

#[test]
fn compiles_i32_ptx_bitwise_loop_and_local_refs() {
    let package = parse_inline_binary_package(
        "compile-i32-ptx-bitwise-loop",
        r#"
#fn kernel : (array : ArrayVLPTX i32) -> () #with PTX {
    #let acc : i32 = 1i32;
    #let index : i32 = 0i32;
    #letref #excl acc_ref : &^ i32 #borrow acc;
    #letref #excl index_ref : &^ i32 #borrow index;
    #loop {
        #let current : i32 = ref_get i32 index_ref;
        #if i32_gte current 3i32 {
            #break;
        };
        #let shifted : i32 = i32_shl (ref_get i32 acc_ref) 1i32;
        #let mixed : i32 = i32_xor shifted current;
        ref_set i32 acc_ref (i32_shr mixed 1i32);
        ref_set i32 index_ref (i32_add current 1i32);
    };
    ref_set_ptx i32 array 0i32 (ref_get i32 acc_ref);
    ()
}

#compile_ptx kernel #to kernel_ptx;

#entrypoint main;

#fn main : () #with IO {
    ()
}
"#,
    );

    let program = lower_package_to_program(&package).expect("lower package");
    let ptx = String::from_utf8_lossy(&program.data[0]);
    assert!(ptx.contains(".visible .entry kernel("));
    assert!(ptx.contains("shl.b32"));
    assert!(ptx.contains("xor.b32"));
    assert!(ptx.contains("shr.u32"));
    assert!(ptx.contains("setp.ge.s32"));
    assert!(ptx.contains("bra $kernel_loop_"));
    assert!(ptx.contains("mov.u32"));
    assert!(ptx.contains("st.global.u32"));
}

#[test]
fn compiles_ptx_if_with_f32_comparison() {
    let root = repo_root().join("tests/testcases/compile-ptx-if");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower package");
    let ptx = String::from_utf8_lossy(&program.data[0]);
    assert!(ptx.contains(".visible .entry threshold_kernel("));
    assert!(ptx.contains(".reg .pred %p<"));
    assert!(ptx.contains("setp.ge.f32"));
    assert!(ptx.contains("@!%p"));
    assert!(ptx.contains("bra"));
    assert!(ptx.contains("st.global.f32"));
}

#[test]
fn writes_compiled_ptx_artifacts_under_package_neco_directory() {
    let root = unique_temp_dir("ptx-artifacts");
    fs::create_dir_all(&root).expect("create temp package dir");
    let package = ParsedPackage {
        root_dir: root.clone(),
        manifest_path: root.join("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "ptx-artifacts".to_string(),
            dependencies: Vec::new(),
            felis_lib_entrypoint: None,
            felis_bin_entrypoints: vec![PathBuf::from("src/main.fe")],
            native_link_mode: NativeLinkMode::KernelStart,
            native_libraries: Vec::new(),
        },
        source_files: Vec::new(),
    };
    let program = LoweredProgram {
        operations: Vec::new(),
        data: vec![b".version 8.0\n\0".to_vec()],
        compiled_ptx: vec![CompiledPtxArtifact {
            data_index: 0,
            function_name: "kernel".to_string(),
        }],
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };

    write_compiled_ptx_artifacts(&package, &program).expect("write ptx artifacts");

    assert_eq!(
        fs::read(root.join(".neco/kernel.ptx")).expect("read ptx artifact"),
        b".version 8.0\n"
    );
    fs::remove_dir_all(root).expect("cleanup temp package dir");
}

#[test]
fn compiles_ptx_struct_field_access() {
    let root = repo_root().join("tests/testcases/cuda-compile-ptx-struct-module-load");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower package");
    let ptx = String::from_utf8_lossy(&program.data[0]);
    assert!(ptx.contains(".visible .entry vec3_sum_kernel("));
    assert!(ptx.contains("ld.global.u32"));
    assert!(ptx.contains("add.s32"));
    assert!(ptx.contains("st.global.u32 [%rd1+12]"));
}

#[test]
fn compiles_ptx_special_registers() {
    let root = repo_root().join("tests/testcases/cuda-compile-ptx-special-registers-module-load");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower package");
    let ptx = String::from_utf8_lossy(&program.data[0]);
    assert!(ptx.contains(".visible .entry special_registers_kernel("));
    assert!(ptx.contains("mov.u32 %r1, %ctaid.x;"));
    assert!(ptx.contains("mov.u32 %r2, %ctaid.y;"));
    assert!(ptx.contains("mov.u32 %r4, %ctaid.z;"));
    assert!(ptx.contains("mov.u32 %r6, %ntid.x;"));
    assert!(ptx.contains("mov.u32 %r7, %ntid.y;"));
    assert!(ptx.contains("mov.u32 %r9, %ntid.z;"));
    assert!(ptx.contains("mov.u32 %r11, %tid.x;"));
    assert!(ptx.contains("mov.u32 %r12, %tid.y;"));
    assert!(ptx.contains("mov.u32 %r14, %tid.z;"));
    assert!(ptx.contains("st.global.u32 [%rd1+0]"));
}

#[test]
fn rejects_ptx_struct_rc_values() {
    let package = parse_inline_binary_package(
        "reject-ptx-struct-rc-values",
        r#"
#use std_core::io::IO;
#use std_core::ptx::PTX;
#use std_core::primitive::array::ArrayVLPTX;
#use std_core::primitive::f32::f32;
#use std_core::primitive::reference::ref_get_ptx;
#use std_core::primitive::reference::ref_set_ptx;

#struct(rc) Vec3 : Type[0] {
    x : f32,
    y : f32,
    z : f32,
}

#fn kernel : (array : ArrayVLPTX f32) -> () #with PTX {
    #let x : f32 = ref_get_ptx f32 array 0i32;
    #let v : Vec3 = Vec3 { x = x, y = x, z = x };
    ref_set_ptx f32 array 1i32 v.x;
    ()
}

#compile_ptx kernel #to kernel_ptx;

#entrypoint main;

#fn main : () #with IO {
    ()
}
"#,
    );

    let error = lower_package_to_program(&package).expect_err("lowering must reject struct(rc)");
    assert!(error.to_string().contains("struct(rc)"));
}

#[test]
fn rejects_zero_argument_compile_ptx_target() {
    let package = parse_inline_binary_package(
        "reject-zero-argument-compile-ptx",
        r#"
#fn empty_kernel : () #with PTX {
    ()
}

#compile_ptx empty_kernel #to empty_kernel_ptx;

#entrypoint main;

#fn main : () #with IO {
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject zero-argument PTX");
    assert!(
        error
            .to_string()
            .contains("supports only one-argument PTX functions")
    );
}

#[test]
fn lowers_open_read_close_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/open-read-close");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![
            ArrayAllocation {
                slot: 0,
                len: 0,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Dynamic,
            },
            ArrayAllocation {
                slot: 1,
                len: 128,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Dynamic,
            },
        ]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArrayAllocDynamic {
                array_slot: 0,
                len: I32Expr::Literal(32),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(0),
            },
            Operation::PathBufPush {
                path_slot: 0,
                source: PathBufSource::StaticData(0),
            },
            Operation::Open {
                path: OpenPath::PathBuf(0),
                flags: I32Expr::Literal(0),
                mode: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::Read {
                fd: I32Expr::Local(0),
                array_slot: 1,
                len: I32Expr::Literal(128),
                result_slot: 1,
            },
            Operation::Close {
                fd: I32Expr::Local(0),
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 1,
                len: I32Expr::Local(1),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
    assert_eq!(program.data, vec![b"message.txt\0".to_vec()]);
    assert_eq!(program.i32_slots, 2);
}

#[test]
fn lowers_open_write_close_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/open-write-close");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 0,
            element_type: ArrayElementType::U8,
            kind: ArrayKind::Dynamic,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArrayAllocDynamic {
                array_slot: 0,
                len: I32Expr::Literal(32),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(0),
            },
            Operation::PathBufPush {
                path_slot: 0,
                source: PathBufSource::StaticData(0),
            },
            Operation::Open {
                path: OpenPath::PathBuf(0),
                flags: I32Expr::Literal(577),
                mode: I32Expr::Literal(420),
                result_slot: 0,
            },
            Operation::WriteStatic {
                fd: I32Expr::Local(0),
                data_index: 1,
                len: I32Expr::Literal(25),
            },
            Operation::Close {
                fd: I32Expr::Local(0),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
    assert_eq!(
        program.data,
        vec![
            b"created.txt\0".to_vec(),
            b"open/write/close fixture\n\0".to_vec(),
        ]
    );
    assert_eq!(program.i32_slots, 1);
}

#[test]
fn lowers_cli_args_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/cli-args");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::ArrayAllocDynamic {
                array_slot: 0,
                len: I32Expr::Literal(256),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(0),
            },
            Operation::PathBufPush {
                path_slot: 0,
                source: PathBufSource::RuntimeArg(I32Expr::Literal(1)),
            },
            Operation::Open {
                path: OpenPath::PathBuf(0),
                flags: I32Expr::Literal(0),
                mode: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::Read {
                fd: I32Expr::Local(0),
                array_slot: 1,
                len: I32Expr::Literal(64),
                result_slot: 1,
            },
            Operation::Close {
                fd: I32Expr::Local(0),
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 1,
                len: I32Expr::Local(1),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::FromU8(Box::new(U8Expr::RuntimeArgGet {
                    arg_index: Box::new(I32Expr::Literal(2)),
                    index: Box::new(I64Expr::Literal(0)),
                }))),
                Box::new(I32Expr::FromU8(Box::new(U8Expr::RuntimeArgGet {
                    arg_index: Box::new(I32Expr::Literal(2)),
                    index: Box::new(I64Expr::Literal(1)),
                }))),
            ))),
        ]
    );
    assert!(program.data.is_empty());
    assert_eq!(
        program.arrays,
        vec![
            ArrayAllocation {
                slot: 0,
                len: 0,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Dynamic,
            },
            ArrayAllocation {
                slot: 1,
                len: 64,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Fixed,
            },
        ]
    );
    assert_eq!(program.i32_slots, 2);
    assert!(program.requires_argv);
}

#[test]
fn lowers_open_array_path_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/open-array-path");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![
            ArrayAllocation {
                slot: 0,
                len: 12,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Fixed,
            },
            ArrayAllocation {
                slot: 1,
                len: 0,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Dynamic,
            },
            ArrayAllocation {
                slot: 2,
                len: 128,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Fixed,
            },
        ]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(0x6d),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: U8Expr::Literal(0x65),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(2),
                value: U8Expr::Literal(0x73),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(3),
                value: U8Expr::Literal(0x73),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(4),
                value: U8Expr::Literal(0x61),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(5),
                value: U8Expr::Literal(0x67),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(6),
                value: U8Expr::Literal(0x65),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(7),
                value: U8Expr::Literal(0x2e),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(8),
                value: U8Expr::Literal(0x74),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(9),
                value: U8Expr::Literal(0x78),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(10),
                value: U8Expr::Literal(0x74),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(11),
                value: U8Expr::Literal(0x00),
            },
            Operation::ArrayAllocDynamic {
                array_slot: 1,
                len: I32Expr::Literal(32),
            },
            Operation::ArraySetU8 {
                array_slot: 1,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(0),
            },
            Operation::PathBufPush {
                path_slot: 1,
                source: PathBufSource::Array(0),
            },
            Operation::Open {
                path: OpenPath::PathBuf(1),
                flags: I32Expr::Literal(0),
                mode: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::Read {
                fd: I32Expr::Local(0),
                array_slot: 2,
                len: I32Expr::Literal(128),
                result_slot: 1,
            },
            Operation::Close {
                fd: I32Expr::Local(0),
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 2,
                len: I32Expr::Local(1),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
}
