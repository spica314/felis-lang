use std::path::{Path, PathBuf};

use neco_rs_parser::{ParsedPackage, ParsedRoot, parse_root};

use crate::cli::{default_output_path, select_binary_from_package};
use crate::codegen::build_linux_x86_64_program_executable;
use crate::ir::{
    ArrayAllocation, ArrayElementType, ComparisonKind, ConditionExpr, ExitCodeExpr, I32Expr,
    LoweredProgram, OpenPath, Operation, U8Expr,
};
use crate::lowering::lower_package_to_program;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

fn selected_fixture_package(root: &Path, binary_name: &str) -> ParsedPackage {
    let ParsedRoot::Package(package) = parse_root(root).expect("fixture parses") else {
        panic!("expected package root");
    };
    select_binary_from_package(package, &PathBuf::from(binary_name)).expect("select binary")
}

#[test]
fn lowers_exit_fixture_to_program() {
    let root = repo_root().join("tests/testcases/exit-42");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))]
    );
    assert!(program.data.is_empty());
    assert!(program.arrays.is_empty());
}

#[test]
fn lowers_hello_world_fixture_to_program() {
    let root = repo_root().join("tests/testcases/hello-world");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::WriteStatic {
                fd: I32Expr::Literal(1),
                data_index: 0,
                len: I32Expr::Literal(14),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0)))
        ]
    );
    assert_eq!(program.data, vec![b"Hello, world!\n\0".to_vec()]);
    assert!(program.arrays.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_comments_basic_fixture_to_program() {
    let root = repo_root().join("tests/testcases/comments-basic");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::WriteStatic {
                fd: I32Expr::Literal(1),
                data_index: 0,
                len: I32Expr::Literal(14),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0)))
        ]
    );
    assert_eq!(program.data, vec![b"Hello, world!\n\0".to_vec()]);
    assert!(program.arrays.is_empty());
    assert_eq!(program.i32_slots, 0);
}

#[test]
fn lowers_i32_ops_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/i32-ops");
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
}

#[test]
fn lowers_u8_ops_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/u8-ops");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::U8(U8Expr::Mod(
            Box::new(U8Expr::Div(
                Box::new(U8Expr::Mul(
                    Box::new(U8Expr::Sub(
                        Box::new(U8Expr::Add(
                            Box::new(U8Expr::Literal(3)),
                            Box::new(U8Expr::Literal(7)),
                        )),
                        Box::new(U8Expr::Literal(4)),
                    )),
                    Box::new(U8Expr::Literal(21)),
                )),
                Box::new(U8Expr::Literal(3)),
            )),
            Box::new(U8Expr::Literal(80)),
        )))]
    );
    assert!(program.data.is_empty());
    assert!(program.arrays.is_empty());
}

#[test]
fn lowers_array_basic_fixture_to_runtime_array_operations() {
    let root = repo_root().join("tests/testcases/array-basic");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 4,
            element_type: ArrayElementType::I32,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I32Expr::Literal(0),
                value: I32Expr::Literal(7),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I32Expr::Literal(1),
                value: I32Expr::Literal(14),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I32Expr::Literal(2),
                value: I32Expr::Literal(21),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Add(
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I32Expr::Literal(0)),
                    }),
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I32Expr::Literal(1)),
                    }),
                )),
                Box::new(I32Expr::ArrayGet {
                    array_slot: 0,
                    index: Box::new(I32Expr::Literal(2)),
                }),
            ))),
        ]
    );
}

#[test]
fn lowers_array_type_annotation_fixture_to_runtime_array_operations() {
    let root = repo_root().join("tests/testcases/array-type-annotation");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 4,
            element_type: ArrayElementType::I32,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I32Expr::Literal(0),
                value: I32Expr::Literal(7),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I32Expr::Literal(1),
                value: I32Expr::Literal(14),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I32Expr::Literal(2),
                value: I32Expr::Literal(21),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Add(
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I32Expr::Literal(0)),
                    }),
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I32Expr::Literal(1)),
                    }),
                )),
                Box::new(I32Expr::ArrayGet {
                    array_slot: 0,
                    index: Box::new(I32Expr::Literal(2)),
                }),
            ))),
        ]
    );
}

#[test]
fn lowers_i32_reference_annotation_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/i32-reference-annotation");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::Literal(41),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Local(0)),
                Box::new(I32Expr::Literal(1)),
            ))),
        ]
    );
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
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(0),
                value: U8Expr::Literal(104),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(1),
                value: U8Expr::Literal(101),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(2),
                value: U8Expr::Literal(108),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(3),
                value: U8Expr::Literal(108),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(4),
                value: U8Expr::Literal(111),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(5),
                value: U8Expr::Literal(44),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(6),
                value: U8Expr::Literal(32),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(7),
                value: U8Expr::Literal(119),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(8),
                value: U8Expr::Literal(111),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(9),
                value: U8Expr::Literal(114),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(10),
                value: U8Expr::Literal(108),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(11),
                value: U8Expr::Literal(100),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(12),
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
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(0),
                value: U8Expr::Literal(65),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(1),
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
                value: I32Expr::Literal(0),
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
                value: I32Expr::Literal(0),
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
    assert_eq!(
        program.operations,
        vec![
            Operation::Mmap {
                len: I32Expr::Literal(8),
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
                value: I32Expr::Literal(0),
            },
            Operation::Mmap {
                len: I32Expr::Literal(20),
                result_slot: 1,
            },
            Operation::HeapStoreI32 {
                heap_slot: 1,
                byte_offset: 0,
                value: I32Expr::Literal(1),
            },
            Operation::HeapStoreI32 {
                heap_slot: 1,
                byte_offset: 4,
                value: I32Expr::Literal(0),
            },
            Operation::HeapStoreI32 {
                heap_slot: 1,
                byte_offset: 8,
                value: I32Expr::Literal(22),
            },
            Operation::HeapStorePtr {
                heap_slot: 1,
                byte_offset: 12,
                source_heap_slot: 0,
            },
            Operation::Mmap {
                len: I32Expr::Literal(20),
                result_slot: 2,
            },
            Operation::HeapStoreI32 {
                heap_slot: 2,
                byte_offset: 0,
                value: I32Expr::Literal(1),
            },
            Operation::HeapStoreI32 {
                heap_slot: 2,
                byte_offset: 4,
                value: I32Expr::Literal(0),
            },
            Operation::HeapStoreI32 {
                heap_slot: 2,
                byte_offset: 8,
                value: I32Expr::Literal(20),
            },
            Operation::HeapStorePtr {
                heap_slot: 2,
                byte_offset: 12,
                source_heap_slot: 1,
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Literal(20)),
                Box::new(I32Expr::Add(
                    Box::new(I32Expr::Literal(22)),
                    Box::new(I32Expr::Literal(0)),
                )),
            ))),
        ]
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
    assert_eq!(program.heap_slots, 3);
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
                    then_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(
                        42,
                    )))],
                    else_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(
                        2,
                    )))],
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
fn lowers_open_read_close_fixture_to_runtime_io_operations() {
    let root = repo_root().join("tests/testcases/open-read-close");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 128,
            element_type: ArrayElementType::U8,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::Open {
                path: OpenPath::StaticData(0),
                flags: I32Expr::Literal(0),
                mode: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::Read {
                fd: I32Expr::Local(0),
                array_slot: 0,
                len: I32Expr::Literal(128),
                result_slot: 1,
            },
            Operation::Close {
                fd: I32Expr::Local(0),
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 0,
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
    assert!(program.arrays.is_empty());
    assert_eq!(
        program.operations,
        vec![
            Operation::Open {
                path: OpenPath::StaticData(0),
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
            Operation::Open {
                path: OpenPath::RuntimeArg(I32Expr::Literal(1)),
                flags: I32Expr::Literal(0),
                mode: I32Expr::Literal(0),
                result_slot: 0,
            },
            Operation::Read {
                fd: I32Expr::Local(0),
                array_slot: 0,
                len: I32Expr::Literal(64),
                result_slot: 1,
            },
            Operation::Close {
                fd: I32Expr::Local(0),
            },
            Operation::WriteArray {
                fd: I32Expr::Literal(1),
                array_slot: 0,
                len: I32Expr::Local(1),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::FromU8(Box::new(U8Expr::RuntimeArgGet {
                    arg_index: Box::new(I32Expr::Literal(2)),
                    index: Box::new(I32Expr::Literal(0)),
                }))),
                Box::new(I32Expr::FromU8(Box::new(U8Expr::RuntimeArgGet {
                    arg_index: Box::new(I32Expr::Literal(2)),
                    index: Box::new(I32Expr::Literal(1)),
                }))),
            ))),
        ]
    );
    assert!(program.data.is_empty());
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 64,
            element_type: ArrayElementType::U8,
        }]
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
            },
            ArrayAllocation {
                slot: 1,
                len: 128,
                element_type: ArrayElementType::U8,
            },
        ]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(0),
                value: U8Expr::Literal(0x6d),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(1),
                value: U8Expr::Literal(0x65),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(2),
                value: U8Expr::Literal(0x73),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(3),
                value: U8Expr::Literal(0x73),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(4),
                value: U8Expr::Literal(0x61),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(5),
                value: U8Expr::Literal(0x67),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(6),
                value: U8Expr::Literal(0x65),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(7),
                value: U8Expr::Literal(0x2e),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(8),
                value: U8Expr::Literal(0x74),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(9),
                value: U8Expr::Literal(0x78),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(10),
                value: U8Expr::Literal(0x74),
            },
            Operation::ArraySetU8 {
                array_slot: 0,
                index: I32Expr::Literal(11),
                value: U8Expr::Literal(0x00),
            },
            Operation::Open {
                path: OpenPath::Array(0),
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
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
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
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
        requires_argv: false,
    };
    let elf = build_linux_x86_64_program_executable(&program)
        .to_bytes()
        .expect("serialize ELF");

    assert_eq!(&elf[0..4], b"\x7FELF");
    assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x1005..0x1007], &[0x89, 0xc7]);
    assert_eq!(&elf[0x1007..0x1009], &[0x48, 0xbe]);
    assert_eq!(&elf[0x1009..0x1011], &0x404000_u64.to_le_bytes());
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
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
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
    let root = repo_root().join("tests/testcases/exit-42");
    let output = default_output_path(&root);
    assert_eq!(output, root.join(".neco").join("exit-42"));
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
