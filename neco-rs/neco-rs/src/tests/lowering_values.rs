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
fn rejects_string_literal_as_arrayvl_value() {
    let source_path = PathBuf::from("src/main.fe");
    let source = r#"
#use std_core::io::IO;
#entrypoint main;

#fn main : () #with IO {
    #let message : ArrayVL u8 = "Hello\n";
    ()
}
"#;
    let (tokens, syntax) = neco_rs_parser::parse_source(source).expect("parse source");
    let package = ParsedPackage {
        root_dir: PathBuf::from("."),
        manifest_path: PathBuf::from("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "string-literal-arrayvl-value".to_string(),
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
    };

    let error = lower_package_to_program(&package).expect_err("lowering must reject old form");
    assert!(
        error
            .to_string()
            .contains("expected a value of type `ArrayVL u8`")
    );
}

#[test]
fn rejects_string_literal_as_open_path() {
    let source_path = PathBuf::from("src/main.fe");
    let source = r#"
#use std_core::io::IO;
#use std_core::io::FileDescriptor;
#entrypoint main;

#fn main : () #with IO {
    #let fd : FileDescriptor <- IO::open "message.txt" 0i32 0i32;
    ()
}
"#;
    let (tokens, syntax) = neco_rs_parser::parse_source(source).expect("parse source");
    let package = ParsedPackage {
        root_dir: PathBuf::from("."),
        manifest_path: PathBuf::from("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "open-string-path".to_string(),
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
    };

    let error = lower_package_to_program(&package).expect_err("lowering must reject string path");
    assert!(error.to_string().contains("`IO::open` expects a `PathBuf`"));
}

#[test]
fn rejects_arrayvl_as_open_path() {
    let source_path = PathBuf::from("src/main.fe");
    let source = r#"
#use std_core::io::IO;
#use std_core::io::FileDescriptor;
#entrypoint main;

#fn main : () #with IO {
    #let path : & ArrayVL u8 = "message.txt";
    #let fd : FileDescriptor <- IO::open path 0i32 0i32;
    ()
}
"#;
    let (tokens, syntax) = neco_rs_parser::parse_source(source).expect("parse source");
    let package = ParsedPackage {
        root_dir: PathBuf::from("."),
        manifest_path: PathBuf::from("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "open-arrayvl-path".to_string(),
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
    };

    let error = lower_package_to_program(&package).expect_err("lowering must reject ArrayVL path");
    assert!(error.to_string().contains("`IO::open` expects a `PathBuf`"));
}

#[test]
fn lowers_pathbuf_pop_to_runtime_operation() {
    let source_path = PathBuf::from("src/main.fe");
    let source = r#"
#use std_core::io::IO;
#use std_core::path::PathBuf;
#entrypoint main;

#fn main : () #with IO {
    #let path : PathBuf <- IO::pathbuf_new 64i32;
    #letref #excl path_ref : &^ PathBuf #borrow path;
    #let _ : () <- IO::pathbuf_push path_ref "dir/message.txt";
    #let _ : () <- IO::pathbuf_pop path_ref;
    ()
}
"#;
    let (tokens, syntax) = neco_rs_parser::parse_source(source).expect("parse source");
    let package = ParsedPackage {
        root_dir: PathBuf::from("."),
        manifest_path: PathBuf::from("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "pathbuf-pop".to_string(),
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
    };

    let program = lower_package_to_program(&package).expect("lower pathbuf pop");
    assert_eq!(
        program.operations,
        vec![
            Operation::ArrayAllocDynamic {
                array_slot: 0,
                len: I32Expr::Literal(64),
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
            Operation::PathBufPop { path_slot: 0 },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
        ]
    );
    assert_eq!(program.data, vec![b"dir/message.txt\0".to_vec()]);
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
fn lowers_f32_function_reference_slot_fixture_to_runtime_slot() {
    let root = repo_root().join("tests/testcases/f32-reference-slots");
    let package = selected_fixture_package(&root, "f32-function-reference-slot");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert!(program.operations.iter().any(|operation| matches!(
        operation,
        Operation::StoreF32 {
            slot: 0,
            value: F32Expr::LiteralBits(_)
        }
    )));
    assert_eq!(program.f32_slots, 1);
}

#[test]
fn lowers_f32_block_reference_slot_fixture_to_runtime_slot() {
    let root = repo_root().join("tests/testcases/f32-reference-slots");
    let package = selected_fixture_package(&root, "f32-block-reference-slot");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert!(program.operations.iter().any(|operation| matches!(
        operation,
        Operation::StoreF32 {
            slot: 0,
            value: F32Expr::LiteralBits(_)
        }
    )));
    assert_eq!(program.f32_slots, 1);
}

#[test]
fn lowers_i64_ops_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/i64-ops");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I64(I64Expr::Mod(
            Box::new(I64Expr::Div(
                Box::new(I64Expr::Mul(
                    Box::new(I64Expr::Sub(
                        Box::new(I64Expr::Add(
                            Box::new(I64Expr::Literal(3)),
                            Box::new(I64Expr::Literal(7)),
                        )),
                        Box::new(I64Expr::Literal(4)),
                    )),
                    Box::new(I64Expr::Literal(61)),
                )),
                Box::new(I64Expr::Literal(3)),
            )),
            Box::new(I64Expr::Literal(80)),
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
fn lowers_bool_basic_fixture_to_runtime_conditions() {
    let root = repo_root().join("tests/testcases/bool-basic");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::If {
                condition: ConditionExpr::Literal(false),
                then_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(1)))],
                else_operations: vec![],
            },
            Operation::If {
                condition: ConditionExpr::And(
                    Box::new(ConditionExpr::Or(
                        Box::new(ConditionExpr::Literal(false)),
                        Box::new(ConditionExpr::And(
                            Box::new(ConditionExpr::Literal(true)),
                            Box::new(ConditionExpr::I32 {
                                kind: ComparisonKind::Eq,
                                lhs: I32Expr::Literal(7),
                                rhs: I32Expr::Literal(7),
                            }),
                        )),
                    )),
                    Box::new(ConditionExpr::Not(Box::new(ConditionExpr::Literal(false)))),
                ),
                then_operations: vec![Operation::If {
                    condition: ConditionExpr::Or(
                        Box::new(ConditionExpr::Literal(false)),
                        Box::new(ConditionExpr::Not(Box::new(ConditionExpr::Literal(false)))),
                    ),
                    then_operations: vec![Operation::If {
                        condition: ConditionExpr::Not(Box::new(ConditionExpr::And(
                            Box::new(ConditionExpr::Literal(true)),
                            Box::new(ConditionExpr::Literal(false)),
                        ))),
                        then_operations: vec![Operation::Exit(ExitCodeExpr::I32(
                            I32Expr::Literal(42),
                        ))],
                        else_operations: vec![],
                    }],
                    else_operations: vec![],
                }],
                else_operations: vec![],
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(2))),
        ]
    );
    assert!(program.data.is_empty());
    assert!(program.arrays.is_empty());
}

fn assert_equality_fixture_lowers_to_condition(
    binary_name: &str,
    condition: ConditionExpr,
    then_exit: i32,
    final_exit: i32,
) {
    let root = repo_root().join("tests/testcases/equality-basic");
    let package = selected_fixture_package(&root, binary_name);

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations,
        vec![
            Operation::If {
                condition,
                then_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(
                    then_exit
                )))],
                else_operations: vec![],
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(final_exit))),
        ]
    );
    assert!(program.data.is_empty());
    assert!(program.arrays.is_empty());
}

#[test]
fn lowers_enum_equality_true_fixture_to_bool_condition() {
    assert_equality_fixture_lowers_to_condition(
        "enum-eq-true",
        ConditionExpr::Literal(true),
        42,
        1,
    );
}

#[test]
fn lowers_enum_equality_false_fixture_to_bool_condition() {
    assert_equality_fixture_lowers_to_condition(
        "enum-eq-false",
        ConditionExpr::Literal(false),
        1,
        42,
    );
}

#[test]
fn lowers_enum_payload_equality_true_fixture_to_bool_condition() {
    assert_equality_fixture_lowers_to_condition(
        "enum-payload-eq-true",
        ConditionExpr::I32 {
            kind: ComparisonKind::Eq,
            lhs: I32Expr::Literal(42),
            rhs: I32Expr::Literal(42),
        },
        42,
        1,
    );
}

#[test]
fn lowers_enum_payload_equality_false_fixture_to_bool_condition() {
    assert_equality_fixture_lowers_to_condition(
        "enum-payload-eq-false",
        ConditionExpr::I32 {
            kind: ComparisonKind::Eq,
            lhs: I32Expr::Literal(41),
            rhs: I32Expr::Literal(42),
        },
        1,
        42,
    );
}

#[test]
fn lowers_struct_equality_true_fixture_to_bool_condition() {
    assert_equality_fixture_lowers_to_condition(
        "struct-eq-true",
        ConditionExpr::I32 {
            kind: ComparisonKind::Eq,
            lhs: I32Expr::Literal(42),
            rhs: I32Expr::Literal(42),
        },
        42,
        1,
    );
}

#[test]
fn lowers_struct_equality_false_fixture_to_bool_condition() {
    assert_equality_fixture_lowers_to_condition(
        "struct-eq-false",
        ConditionExpr::I32 {
            kind: ComparisonKind::Eq,
            lhs: I32Expr::Literal(41),
            rhs: I32Expr::Literal(42),
        },
        1,
        42,
    );
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
            kind: ArrayKind::Fixed,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: I32Expr::Literal(7),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: I32Expr::Literal(14),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(2),
                value: I32Expr::Literal(21),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Add(
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I64Expr::Literal(0)),
                    }),
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I64Expr::Literal(1)),
                    }),
                )),
                Box::new(I32Expr::ArrayGet {
                    array_slot: 0,
                    index: Box::new(I64Expr::Literal(2)),
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
            kind: ArrayKind::Fixed,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: I32Expr::Literal(7),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: I32Expr::Literal(14),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(2),
                value: I32Expr::Literal(21),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Add(
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I64Expr::Literal(0)),
                    }),
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I64Expr::Literal(1)),
                    }),
                )),
                Box::new(I32Expr::ArrayGet {
                    array_slot: 0,
                    index: Box::new(I64Expr::Literal(2)),
                }),
            ))),
        ]
    );
}

#[test]
fn lowers_dyn_array_type_annotation_fixture_to_runtime_array_operations() {
    let root = repo_root().join("tests/testcases/dyn-array-type-annotation");
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
            kind: ArrayKind::Dynamic,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: I32Expr::Literal(7),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: I32Expr::Literal(14),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(2),
                value: I32Expr::Literal(21),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Add(
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I64Expr::Literal(0)),
                    }),
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I64Expr::Literal(1)),
                    }),
                )),
                Box::new(I32Expr::ArrayGet {
                    array_slot: 0,
                    index: Box::new(I64Expr::Literal(2)),
                }),
            ))),
        ]
    );
}

#[test]
fn lowers_dyn_array_u8_helpers_fixture_to_runtime_array_operations() {
    let root = repo_root().join("tests/testcases/dyn-array-u8-helpers");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 8,
            element_type: ArrayElementType::U8,
            kind: ArrayKind::Dynamic,
        }]
    );
    assert!(operations_contain_static_data_get(&program.operations));
}

#[test]
fn lowers_arrayvl_len_fixture_to_runtime_array_len() {
    let root = repo_root().join("tests/testcases/arrayvl-len");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 42,
            element_type: ArrayElementType::U8,
            kind: ArrayKind::Dynamic,
        }]
    );
    assert_eq!(
        program.operations,
        vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::ArrayLen {
            array_slot: 0,
        }))]
    );
}

#[test]
fn rejects_direct_arrayvl_new_into_arrayvl_reference() {
    let source_path = PathBuf::from("src/main.fe");
    let source = r#"
#use std_core::io::IO;
#entrypoint main;

#fn main : () #with IO {
    #let array_ref : &^ ArrayVL u8 <- IO::arrayvl_new u8 42i32;
    ()
}
"#;
    let (tokens, syntax) = neco_rs_parser::parse_source(source).expect("parse source");
    let package = ParsedPackage {
        root_dir: PathBuf::from("."),
        manifest_path: PathBuf::from("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "direct-arrayvl-new-reference".to_string(),
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
    };

    let error = lower_package_to_program(&package).expect_err("lowering must reject old form");
    assert!(
        error
            .to_string()
            .contains("`IO::arrayvl_new` returns an `ArrayVL T` value")
    );
}

#[test]
fn rejects_direct_array_new_into_array_reference() {
    let source_path = PathBuf::from("src/main.fe");
    let source = r#"
#use std_core::io::IO;
#entrypoint main;

#fn main : () #with IO {
    #let array_ref : &^ Array i32 4i32 <- IO::array_new i32 4i32;
    ()
}
"#;
    let (tokens, syntax) = neco_rs_parser::parse_source(source).expect("parse source");
    let package = ParsedPackage {
        root_dir: PathBuf::from("."),
        manifest_path: PathBuf::from("neco-package.json"),
        manifest: neco_rs_parser::PackageManifest {
            name: "direct-array-new-reference".to_string(),
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
    };

    let error = lower_package_to_program(&package).expect_err("lowering must reject old form");
    assert!(
        error
            .to_string()
            .contains("`IO::array_new` returns an `Array T len` value")
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
fn lowers_reference_builtin_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/reference-builtins");
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
                slot: 0,
                value: I32Expr::Literal(41),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Local(0)),
                Box::new(I32Expr::Local(0)),
            ))),
        ]
    );
}

#[test]
fn rejects_ref_set_through_shared_primitive_reference() {
    let package = parse_inline_binary_package(
        "shared-primitive-ref-set",
        r#"
#use std_core::io::IO;
#use std_core::primitive::reference::ref_set;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn main : () #with IO {
    #let value : i32 = 1i32;
    #letref value_ref : & i32 #borrow value;
    ref_set i32 value_ref 2i32;
    #let _ : () <- IO::exit 0i32;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package)
        .expect_err("lowering must reject ref_set through a shared primitive reference");
    assert!(
        error
            .to_string()
            .contains("`ref_set` requires an exclusive reference")
    );
}

#[test]
fn rejects_shared_primitive_reference_for_exclusive_parameter() {
    let package = parse_inline_binary_package(
        "shared-primitive-ref-exclusive-parameter",
        r#"
#use std_core::io::IO;
#use std_core::primitive::reference::ref_set;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn write_ref : (value_ref : &^ i32) -> () #with IO {
    ref_set i32 value_ref 2i32;
    ()
}

#fn main : () #with IO {
    #let value : i32 = 1i32;
    #letref value_ref : & i32 #borrow value;
    write_ref value_ref;
    #let _ : () <- IO::exit 0i32;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package)
        .expect_err("lowering must reject a shared primitive reference for an exclusive parameter");
    assert!(
        error
            .to_string()
            .contains("expected a value of type `&^ i32`")
    );
}

#[test]
fn rejects_unknown_effect_on_non_entrypoint_function() {
    let package = parse_inline_binary_package(
        "unknown-function-effect",
        r#"
#entrypoint main;

#fn helper : () #with Disk {
    ()
}

#fn main : () {
    helper;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package)
        .expect_err("lowering must reject unknown non-entrypoint effects");
    assert!(
        error
            .to_string()
            .contains("function `helper` effect must be `IO`")
    );
}
