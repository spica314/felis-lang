use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use neco_rs_parser::{NativeLinkMode, ParsedPackage, ParsedRoot, parse_root};

use crate::cli::{default_output_path, select_binary_from_package};
use crate::codegen::{
    EntryAbi, LINUX_X86_64_EXECUTABLE_LAYOUT, build_linux_x86_64_program_executable,
    build_linux_x86_64_program_image,
};
use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, ComparisonKind, CompiledPtxArtifact,
    ConditionExpr, ExitCodeExpr, F32Expr, I32Expr, I64Expr, KernelArgumentRef, LoweredProgram,
    OpenPath, Operation, PathBufSource, U8Expr,
};
use crate::lowering::lower_package_to_program;
use crate::run_cli;
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
            felis_test_entrypoints: Vec::new(),
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
fn lowers_fn_call_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/fn-call");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Mod(
            Box::new(I32Expr::Div(
                Box::new(I32Expr::Mul(
                    Box::new(I32Expr::Sub(
                        Box::new(I32Expr::Add(
                            Box::new(I32Expr::Literal(3)),
                            Box::new(I32Expr::Literal(7)),
                        )),
                        Box::new(I32Expr::Literal(4)),
                    )),
                    Box::new(I32Expr::Literal(61)),
                )),
                Box::new(I32Expr::Literal(3)),
            )),
            Box::new(I32Expr::Literal(80)),
        )))]
    );
    assert!(program.data.is_empty());
    assert!(program.arrays.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn builds_elf_image_with_exit_syscall() {
    let program = LoweredProgram {
        operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))],
        data: Vec::new(),
        compiled_ptx: Vec::new(),
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let elf = build_linux_x86_64_program_executable(&program)
        .to_bytes()
        .expect("serialize ELF");
    assert_eq!(&elf[0..4], b"\x7FELF");
    assert_eq!(&elf[0x1000..0x1005], &[0xb8, 42, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x1005..0x1007], &[0x89, 0xc7]);
    assert_eq!(&elf[0x1007..0x100c], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x100c..0x100e], &[0x0f, 0x05]);
}

#[test]
fn builds_elf_image_with_write_and_implicit_exit() {
    let program = LoweredProgram {
        operations: vec![
            Operation::WriteStatic {
                fd: I32Expr::Literal(1),
                data_index: 0,
                len: I32Expr::Literal(14),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ],
        data: vec![b"Hello, world!\n\0".to_vec()],
        compiled_ptx: Vec::new(),
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let elf = build_linux_x86_64_program_executable(&program)
        .to_bytes()
        .expect("serialize ELF");

    assert_eq!(&elf[0..4], b"\x7FELF");
    assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x1005..0x1007], &[0x89, 0xc7]);
    assert_eq!(&elf[0x1007..0x1009], &[0x48, 0xbe]);
    assert_eq!(
        &elf[0x1009..0x1011],
        &LINUX_X86_64_EXECUTABLE_LAYOUT
            .data_virtual_address
            .to_le_bytes()
    );
    assert_eq!(&elf[0x1011..0x1016], &[0xb8, 14, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x1016..0x1018], &[0x89, 0xc2]);
    assert_eq!(&elf[0x1018..0x101d], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x101d..0x101f], &[0x0f, 0x05]);
    assert_eq!(&elf[0x101f..0x1024], &[0xb8, 0x00, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x1024..0x1026], &[0x89, 0xc7]);
    assert_eq!(&elf[0x1026..0x102b], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x102b..0x102d], &[0x0f, 0x05]);
    assert_eq!(&elf[0x2000..0x200e], b"Hello, world!\n");
}

#[test]
fn builds_elf_image_with_mmap_failure_check() {
    let program = LoweredProgram {
        operations: vec![
            Operation::Mmap {
                len: I32Expr::Literal(4096),
                result_slot: 0,
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ],
        data: Vec::new(),
        compiled_ptx: Vec::new(),
        arrays: Vec::new(),
        heap_slots: 1,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let image = build_linux_x86_64_program_image(&program, EntryAbi::KernelStart);
    assert!(
        image
            .code
            .windows(8)
            .any(|window| window == [0x0f, 0x05, 0x48, 0x85, 0xc0, 0x0f, 0x88, 0x05])
    );
}

#[test]
fn builds_elf_image_with_dynamic_array_mmap_failure_check() {
    let program = LoweredProgram {
        operations: vec![
            Operation::ArrayAllocDynamic {
                array_slot: 0,
                len: I32Expr::Literal(16),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ],
        data: Vec::new(),
        compiled_ptx: Vec::new(),
        arrays: vec![ArrayAllocation {
            slot: 0,
            len: 0,
            element_type: ArrayElementType::U8,
            kind: ArrayKind::Dynamic,
        }],
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let image = build_linux_x86_64_program_image(&program, EntryAbi::KernelStart);
    assert!(
        image
            .code
            .windows(8)
            .any(|window| window == [0x0f, 0x05, 0x48, 0x85, 0xc0, 0x0f, 0x88, 0x05])
    );
}

#[test]
fn builds_elf_image_with_array_get_bounds_check() {
    let program = LoweredProgram {
        operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::ArrayGet {
            array_slot: 0,
            index: Box::new(I64Expr::Literal(0)),
        }))],
        data: Vec::new(),
        compiled_ptx: Vec::new(),
        arrays: vec![ArrayAllocation {
            slot: 0,
            len: 1,
            element_type: ArrayElementType::I32,
            kind: ArrayKind::Fixed,
        }],
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let image = build_linux_x86_64_program_image(&program, EntryAbi::KernelStart);
    assert!(
        image
            .code
            .windows(5)
            .any(|window| window == [0x48, 0x85, 0xc9, 0x0f, 0x88])
    );
}

#[test]
fn builds_elf_image_with_array_set_bounds_check() {
    let program = LoweredProgram {
        operations: vec![
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: I32Expr::Literal(42),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ],
        data: Vec::new(),
        compiled_ptx: Vec::new(),
        arrays: vec![ArrayAllocation {
            slot: 0,
            len: 1,
            element_type: ArrayElementType::I32,
            kind: ArrayKind::Fixed,
        }],
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let image = build_linux_x86_64_program_image(&program, EntryAbi::KernelStart);
    assert!(
        image
            .code
            .windows(5)
            .any(|window| window == [0x48, 0x85, 0xc9, 0x0f, 0x88])
    );
}

#[test]
fn builds_elf_image_with_runtime_i32_ops() {
    let program = LoweredProgram {
        operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Mod(
            Box::new(I32Expr::Div(
                Box::new(I32Expr::Mul(
                    Box::new(I32Expr::Sub(
                        Box::new(I32Expr::Add(
                            Box::new(I32Expr::Literal(3)),
                            Box::new(I32Expr::Literal(7)),
                        )),
                        Box::new(I32Expr::Literal(4)),
                    )),
                    Box::new(I32Expr::Literal(61)),
                )),
                Box::new(I32Expr::Literal(3)),
            )),
            Box::new(I32Expr::Literal(80)),
        )))],
        data: Vec::new(),
        compiled_ptx: Vec::new(),
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let elf = build_linux_x86_64_program_executable(&program)
        .to_bytes()
        .expect("serialize ELF");
    let code = &elf[0x1000..];

    assert!(code.windows(2).any(|window| window == [0x01, 0xc8]));
    assert!(code.windows(2).any(|window| window == [0x29, 0xc8]));
    assert!(code.windows(3).any(|window| window == [0x0f, 0xaf, 0xc1]));
    assert!(code.windows(2).any(|window| window == [0xf7, 0xf9]));
    assert!(code.windows(2).any(|window| window == [0x89, 0xd0]));
    assert!(
        !code
            .windows(5)
            .any(|window| window == [0xbf, 42, 0x00, 0x00, 0x00])
    );
    assert!(code.windows(2).any(|window| window == [0x89, 0xc7]));
}

#[test]
fn defaults_output_into_package_neco_directory() {
    let root = repo_root().join("tests/testcases/workspace-basic/workspace-app");
    let package = selected_fixture_package(&root, "main");
    let output = default_output_path(&package);
    assert_eq!(output, root.join(".neco").join("main"));
}

#[test]
fn selects_longest_matching_binary_name() {
    let root = repo_root().join("tests/testcases/if");
    let package = selected_fixture_package(&root, "if-else-if-true");
    assert_eq!(
        package.manifest.felis_bin_entrypoints,
        vec![PathBuf::from("src/if-else-if-true.fe")]
    );
}

#[test]
fn run_cli_returns_target_exit_status() {
    let root = repo_root().join("tests/testcases/exit-42");
    let status = run_cli(vec![
        "neco-rs".to_string(),
        "run".to_string(),
        root.display().to_string(),
    ])
    .expect("run fixture");

    fs::remove_file(root.join(".neco/exit-42")).expect("cleanup output");
    fs::remove_dir(root.join(".neco")).expect("cleanup output dir");

    assert_eq!(status, 42);
}

#[test]
fn run_cli_selects_binary_by_bin_option() {
    let root = repo_root().join("tests/testcases/multi-bin-private-modules");
    let status = run_cli(vec![
        "neco-rs".to_string(),
        "run".to_string(),
        root.display().to_string(),
        "--bin".to_string(),
        "tool".to_string(),
    ])
    .expect("run fixture");

    fs::remove_file(root.join(".neco/tool")).expect("cleanup output");
    fs::remove_dir(root.join(".neco")).expect("cleanup output dir");

    assert_eq!(status, 7);
}

#[test]
fn test_cli_returns_zero_when_all_test_entrypoints_pass() {
    let root = repo_root().join("tests/testcases/neco-test-success");
    let status = run_cli(vec![
        "neco-rs".to_string(),
        "test".to_string(),
        root.display().to_string(),
    ])
    .expect("run test fixtures");

    fs::remove_dir_all(root.join(".neco")).expect("cleanup outputs");

    assert_eq!(status, 0);
}

#[test]
fn test_cli_returns_first_failing_test_status() {
    let root = repo_root().join("tests/testcases/neco-test-failure");
    let status = run_cli(vec![
        "neco-rs".to_string(),
        "test".to_string(),
        root.display().to_string(),
    ])
    .expect("run test fixtures");

    fs::remove_dir_all(root.join(".neco")).expect("cleanup outputs");

    assert_eq!(status, 42);
}
