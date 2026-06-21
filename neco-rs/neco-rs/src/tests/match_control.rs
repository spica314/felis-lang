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
fn lowers_u8_array_hello_world_fixture_to_runtime_array_operations() {
    let root = repo_root().join("tests/testcases/u8-array-hello-world");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 13,
            element_type: ArrayElementType::U8,
            kind: ArrayKind::Fixed,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(104),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: U8Expr::Literal(101),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(2),
                value: U8Expr::Literal(108),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(3),
                value: U8Expr::Literal(108),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(4),
                value: U8Expr::Literal(111),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(5),
                value: U8Expr::Literal(44),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(6),
                value: U8Expr::Literal(32),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(7),
                value: U8Expr::Literal(119),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(8),
                value: U8Expr::Literal(111),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(9),
                value: U8Expr::Literal(114),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(10),
                value: U8Expr::Literal(108),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(11),
                value: U8Expr::Literal(100),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(12),
                value: U8Expr::Literal(10),
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 0,
                len: I32Expr::Literal(13),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_hex_literals_fixture_to_runtime_array_operations() {
    let root = repo_root().join("tests/testcases/hex-literals");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 2,
            element_type: ArrayElementType::U8,
            kind: ArrayKind::Fixed,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(65),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: U8Expr::Literal(10),
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 0,
                len: I32Expr::Literal(2),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_enum_match_basic_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/enum-match-basic");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))]
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_enum_match_payload_single_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/enum-match-payload");
    let package = selected_fixture_package(&root, "enum-match-payload-single");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))]
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_enum_match_payload_pair_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/enum-match-payload");
    let package = selected_fixture_package(&root, "enum-match-payload-pair");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
            Box::new(I32Expr::Literal(20)),
            Box::new(I32Expr::Literal(22)),
        )))]
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_enum_match_string_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/enum-match-string");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::FromU8(
            Box::new(U8Expr::StaticDataGet {
                data_index: 0,
                index: Box::new(I64Expr::Literal(0)),
            })
        )))]
    );
    assert_eq!(program.data, vec![b"A\0".to_vec()]);
    assert!(program.arrays.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_enum_match_struct_string_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/enum-match-struct-string");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::FromU8(
            Box::new(U8Expr::StaticDataGet {
                data_index: 0,
                index: Box::new(I64Expr::Literal(0)),
            })
        )))]
    );
    assert_eq!(program.data, vec![b"A\0".to_vec()]);
    assert!(program.arrays.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_type_rc_match_single_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let package = selected_fixture_package(&root, "type-rc-match-single");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::Mmap {
                len: I32Expr::Literal(12),
                result_slot: 0,
            },
            Operation::HeapStoreI32 {
                heap_slot: 0,
                byte_offset: 0,
                value: I32Expr::Literal(0),
            },
            Operation::HeapStoreI32 {
                heap_slot: 0,
                byte_offset: 4,
                value: I32Expr::Literal(1),
            },
            Operation::HeapStoreI32 {
                heap_slot: 0,
                byte_offset: 8,
                value: I32Expr::Literal(42),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42))),
        ]
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
    assert_eq!(program.heap_slots, 1);
}

#[test]
fn lowers_type_rc_match_pair_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let package = selected_fixture_package(&root, "type-rc-match-pair");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::Mmap {
                len: I32Expr::Literal(16),
                result_slot: 0,
            },
            Operation::HeapStoreI32 {
                heap_slot: 0,
                byte_offset: 0,
                value: I32Expr::Literal(1),
            },
            Operation::HeapStoreI32 {
                heap_slot: 0,
                byte_offset: 4,
                value: I32Expr::Literal(1),
            },
            Operation::HeapStoreI32 {
                heap_slot: 0,
                byte_offset: 8,
                value: I32Expr::Literal(20),
            },
            Operation::HeapStoreI32 {
                heap_slot: 0,
                byte_offset: 12,
                value: I32Expr::Literal(22),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Literal(20)),
                Box::new(I32Expr::Literal(22)),
            ))),
        ]
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
    assert_eq!(program.heap_slots, 1);
}

#[test]
fn lowers_type_rc_match_list_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let package = selected_fixture_package(&root, "type-rc-match-list");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(program.operations.len(), 16);
    assert!(matches!(
        program.operations.last(),
        Some(Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(_, rhs))))
            if matches!(rhs.as_ref(), I32Expr::Add(_, _))
    ));
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
    assert_eq!(program.heap_slots, 3);
}

#[test]
fn lowers_type_rc_match_reference_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let package = selected_fixture_package(&root, "type-rc-match-reference");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations.last(),
        Some(&Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
            Box::new(I32Expr::Literal(20)),
            Box::new(I32Expr::Literal(22)),
        ))))
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
    assert_eq!(program.heap_slots, 4);
}

#[test]
fn lowers_type_rc_match_mut_reference_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let package = selected_fixture_package(&root, "type-rc-match-mut-reference");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::Literal(0),
            },
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::Literal(42),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(0))),
        ]
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 1);
    assert_eq!(program.heap_slots, 0);
}

#[test]
fn lowers_if_else_true_fixture_to_program() {
    let root = repo_root().join("tests/testcases/if");
    let source_path = root.join("src/if-else-true.fe");
    let source = std::fs::read_to_string(&source_path).expect("read fixture source");
    let (tokens, syntax) = neco_rs_parser::parse_source(&source).expect("parse source");
    let package = ParsedPackage {
        root_dir: root.clone(),
        manifest_path: root.join("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "if".to_string(),
            dependencies: Vec::new(),
            felis_lib_entrypoint: None,
            felis_bin_entrypoints: vec![PathBuf::from("src/if-else-true.fe")],
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
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::If {
                condition: ConditionExpr::I32 {
                    kind: ComparisonKind::Eq,
                    lhs: I32Expr::Literal(3),
                    rhs: I32Expr::Literal(3),
                },
                then_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))],
                else_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(1)))],
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
}

#[test]
fn lowers_if_else_if_fixture_to_program() {
    let root = repo_root().join("tests/testcases/if");
    let source_path = root.join("src/if-else-if-true.fe");
    let source = std::fs::read_to_string(&source_path).expect("read fixture source");
    let (tokens, syntax) = neco_rs_parser::parse_source(&source).expect("parse source");
    let package = ParsedPackage {
        root_dir: root.clone(),
        manifest_path: root.join("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "if".to_string(),
            dependencies: Vec::new(),
            felis_lib_entrypoint: None,
            felis_bin_entrypoints: vec![PathBuf::from("src/if-else-if-true.fe")],
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
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::If {
                condition: ConditionExpr::I32 {
                    kind: ComparisonKind::Eq,
                    lhs: I32Expr::Literal(3),
                    rhs: I32Expr::Literal(4),
                },
                then_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(1)))],
                else_operations: vec![Operation::If {
                    condition: ConditionExpr::I32 {
                        kind: ComparisonKind::Eq,
                        lhs: I32Expr::Literal(5),
                        rhs: I32Expr::Literal(5),
                    },
                    then_operations: vec![Operation::Exit(ExitCodeExpr::I32(
                        I32Expr::Literal(42,)
                    ))],
                    else_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(2,)))],
                }],
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
}

#[test]
fn lowers_loop_fixture_to_runtime_operations() {
    let root = repo_root().join("tests/testcases/loop");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::Literal(1),
            },
            Operation::StoreI32 {
                slot: 1,
                value: I32Expr::Literal(0),
            },
            Operation::Loop {
                body_operations: vec![
                    Operation::StoreI32 {
                        slot: 1,
                        value: I32Expr::Add(
                            Box::new(I32Expr::Local(1)),
                            Box::new(I32Expr::Local(0)),
                        ),
                    },
                    Operation::If {
                        condition: ConditionExpr::I32 {
                            kind: ComparisonKind::Eq,
                            lhs: I32Expr::Local(0),
                            rhs: I32Expr::Literal(10),
                        },
                        then_operations: vec![Operation::Break],
                        else_operations: vec![],
                    },
                    Operation::StoreI32 {
                        slot: 0,
                        value: I32Expr::Add(
                            Box::new(I32Expr::Local(0)),
                            Box::new(I32Expr::Literal(1)),
                        ),
                    },
                ],
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(1))),
        ]
    );
    assert_eq!(program.i32_slots, 2);
}

#[test]
fn lowers_continue_fixture_to_runtime_operations() {
    let root = repo_root().join("tests/testcases/continue");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::Literal(0),
            },
            Operation::StoreI32 {
                slot: 1,
                value: I32Expr::Literal(0),
            },
            Operation::Loop {
                body_operations: vec![
                    Operation::StoreI32 {
                        slot: 0,
                        value: I32Expr::Add(
                            Box::new(I32Expr::Local(0)),
                            Box::new(I32Expr::Literal(1)),
                        ),
                    },
                    Operation::If {
                        condition: ConditionExpr::I32 {
                            kind: ComparisonKind::Eq,
                            lhs: I32Expr::Local(0),
                            rhs: I32Expr::Literal(5),
                        },
                        then_operations: vec![Operation::Continue],
                        else_operations: vec![],
                    },
                    Operation::StoreI32 {
                        slot: 1,
                        value: I32Expr::Add(
                            Box::new(I32Expr::Local(1)),
                            Box::new(I32Expr::Local(0)),
                        ),
                    },
                    Operation::If {
                        condition: ConditionExpr::I32 {
                            kind: ComparisonKind::Eq,
                            lhs: I32Expr::Local(0),
                            rhs: I32Expr::Literal(10),
                        },
                        then_operations: vec![Operation::Break],
                        else_operations: vec![],
                    },
                ],
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(1))),
        ]
    );
    assert_eq!(program.i32_slots, 2);
}
