use std::path::{Path, PathBuf};

use neco_rs_parser::{NativeLinkMode, ParsedPackage, ParsedRoot, parse_root};

use crate::cli::{default_output_path, select_binary_from_package};
use crate::codegen::{
    EntryAbi, build_linux_x86_64_program_executable, build_linux_x86_64_program_image,
};
use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, ComparisonKind, ConditionExpr, ExitCodeExpr,
    F32Expr, I32Expr, I64Expr, LoweredProgram, OpenPath, Operation, PathBufSource, U8Expr,
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
    ParsedPackage {
        root_dir: PathBuf::from("."),
        manifest_path: PathBuf::from("neco-package.json"),
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
fn rejects_reference_method_get() {
    let package = parse_inline_binary_package(
        "reference-method-get",
        r#"
#use std_core::io::IO;
#entrypoint main;

#fn main : () #with IO {
    #let value : i32 = 42i32;
    #letref value_ref : & i32 #borrow value;
    #let code : i32 = value_ref .> get;
    #let _ : () <- IO::exit code;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package).expect_err("lowering must reject `.> get`");
    assert!(error.to_string().contains("unsupported pure expression"));
}

#[test]
fn rejects_reference_method_set() {
    let package = parse_inline_binary_package(
        "reference-method-set",
        r#"
#use std_core::io::IO;
#entrypoint main;

#fn main : () #with IO {
    #let value : i32 = 0i32;
    #letref #excl value_ref : &^ i32 #borrow value;
    value_ref .> set 42i32;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package).expect_err("lowering must reject `.> set`");
    assert!(
        error
            .to_string()
            .contains("unsupported expression statement")
    );
}

#[test]
fn rejects_array_method_get() {
    let package = parse_inline_binary_package(
        "array-method-get",
        r#"
#use std_core::io::IO;
#use std_core::primitive::array::Array;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn main : () #with IO {
    #let array_ref : Array i32 1i32 <- IO::array_new i32 1i32;
    #let code : i32 = array_ref .> get 0i32;
    #let _ : () <- IO::exit code;
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject array `.> get`");
    assert!(error.to_string().contains("unsupported pure expression"));
}

#[test]
fn rejects_array_method_set() {
    let package = parse_inline_binary_package(
        "array-method-set",
        r#"
#use std_core::io::IO;
#use std_core::primitive::array::Array;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn main : () #with IO {
    #let array_ref : Array i32 1i32 <- IO::array_new i32 1i32;
    array_ref .> set 0i32 42i32;
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject array `.> set`");
    assert!(
        error
            .to_string()
            .contains("unsupported expression statement")
    );
}

#[test]
fn rejects_array_method_len() {
    let package = parse_inline_binary_package(
        "array-method-len",
        r#"
#use std_core::io::IO;
#use std_core::primitive::array::ArrayVL;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn main : () #with IO {
    #let arrayvl : ArrayVL i32 <- IO::arrayvl_new i32 1i32;
    #letref #excl arrayvl_ref : &^ ArrayVL i32 #borrow arrayvl;
    #let code : i32 = arrayvl_ref .> len;
    #let _ : () <- IO::exit code;
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject array `.> len`");
    assert!(error.to_string().contains("unsupported pure expression"));
}

#[test]
fn rejects_duplicate_struct_fields() {
    let package = parse_inline_binary_package(
        "duplicate-struct-fields",
        r#"
#use std_core::io::IO;
#use std_core::primitive::i32::i32;
#entrypoint main;

#struct Span : Type[0] {
    start : i32,
    start : i32,
}

#fn main : () #with IO {
    #let _ : () <- IO::exit 0i32;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package)
        .expect_err("lowering must reject duplicate struct fields");
    assert!(
        error
            .to_string()
            .contains("duplicate field `start` in struct `Span`"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_invalid_struct_kind_annotation() {
    let package = parse_inline_binary_package(
        "invalid-struct-kind",
        r#"
#use std_core::primitive::i32::i32;

#struct Point : i32 {
    x : i32,
}

#entrypoint main;

#fn main : () {
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject invalid struct kind");
    assert!(
        error
            .to_string()
            .contains("struct `Point` kind must end in `Type[0]`")
    );
}

#[test]
fn rejects_invalid_type_kind_annotation() {
    let package = parse_inline_binary_package(
        "invalid-type-kind",
        r#"
#use std_core::primitive::bool::bool;
#use std_core::primitive::i32::i32;

#type Value : bool {
    single : i32 -> Value,
}

#entrypoint main;

#fn main : () {
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject invalid type kind");
    assert!(
        error
            .to_string()
            .contains("type `Value` kind must end in `Type[0]`")
    );
}

#[test]
fn rejects_invalid_constructor_result_type() {
    let package = parse_inline_binary_package(
        "invalid-constructor-result-type",
        r#"
#use std_core::primitive::i32::i32;

#type Value : Type[0] {
    single : i32 -> Vaule,
}

#entrypoint main;

#fn main : () {
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject constructor typo");
    assert!(
        error
            .to_string()
            .contains("constructor `single` result type must be `Value`")
    );
}

#[test]
fn rejects_struct_and_type_name_collision() {
    let package = parse_inline_binary_package(
        "type-name-collision",
        r#"
#use std_core::primitive::i32::i32;

#type Token : Type[0] {
    value : i32 -> Token,
}

#struct Token : Type[0] {
    value : i32,
}

#entrypoint main;

#fn main : () {
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject type-name collision");
    assert!(
        error
            .to_string()
            .contains("type name `Token` is already used by an algebraic type")
    );
}

#[test]
fn rejects_duplicate_pure_functions() {
    let package = parse_inline_binary_package(
        "duplicate-pure-functions",
        r#"
#use std_core::io::IO;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn answer : i32 {
    1i32
}

#fn answer : i32 {
    2i32
}

#fn main : () #with IO {
    #let _ : () <- IO::exit 0i32;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package)
        .expect_err("lowering must reject duplicate pure functions");
    assert!(
        error
            .to_string()
            .contains("duplicate function `answer` is not supported"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_duplicate_statement_functions() {
    let package = parse_inline_binary_package(
        "duplicate-statement-functions",
        r#"
#use std_core::io::IO;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn write_once : () #with IO {
    ()
}

#fn write_once : () #with IO {
    ()
}

#fn main : () #with IO {
    #let _ : () <- IO::exit 0i32;
    ()
}
"#,
    );

    let error = lower_package_to_program(&package)
        .expect_err("lowering must reject duplicate statement functions");
    assert!(
        error
            .to_string()
            .contains("duplicate function `write_once` is not supported"),
        "unexpected error: {error}"
    );
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

#[test]
fn rejects_reference_and_array_builtins_without_io_effect() {
    let cases = [
        (
            "ref-get-without-io",
            r#"
#use std_core::io::IO;
#use std_core::primitive::reference::ref_get;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn read_ref : (value_ref : & i32) -> i32 {
    ref_get i32 value_ref
}

#fn main : () #with IO {
    #let value : i32 = 42i32;
    #letref value_ref : & i32 #borrow value;
    #let code : i32 = read_ref value_ref;
    #let _ : () <- IO::exit code;
    ()
}
"#,
            "`ref_get` requires `#with IO`",
        ),
        (
            "ref-set-without-io",
            r#"
#use std_core::io::IO;
#use std_core::primitive::reference::ref_get;
#use std_core::primitive::reference::ref_set;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn write_ref : (value_ref : &^ i32) -> () {
    ref_set i32 value_ref 42i32;
    ()
}

#fn main : () #with IO {
    #let value : i32 = 0i32;
    #letref #excl value_ref : &^ i32 #borrow value;
    write_ref value_ref;
    #let _ : () <- IO::exit ((ref_get i32 value_ref));
    ()
}
"#,
            "`ref_set` requires `#with IO`",
        ),
        (
            "array-get-without-io",
            r#"
#use std_core::io::IO;
#use std_core::primitive::array::Array;
#use std_core::primitive::array::array_get;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn read_array : (array_ref : & Array i32 1i32) -> i32 {
    array_get array_ref 0i32
}

#fn main : () #with IO {
    #let array_ref : Array i32 1i32 <- IO::array_new i32 1i32;
    #let code : i32 = read_array array_ref;
    #let _ : () <- IO::exit code;
    ()
}
"#,
            "`array_get` requires `#with IO`",
        ),
        (
            "array-set-without-io",
            r#"
#use std_core::io::IO;
#use std_core::primitive::array::Array;
#use std_core::primitive::array::array_get;
#use std_core::primitive::array::array_set;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn write_array : (array_ref : &^ Array i32 1i32) -> () {
    array_set array_ref 0i32 42i32;
    ()
}

#fn main : () #with IO {
    #let array_ref : Array i32 1i32 <- IO::array_new i32 1i32;
    write_array array_ref;
    #let _ : () <- IO::exit (array_get array_ref 0i32);
    ()
}
"#,
            "`array_set` requires `#with IO`",
        ),
        (
            "array-len-without-io",
            r#"
#use std_core::io::IO;
#use std_core::primitive::array::ArrayVL;
#use std_core::primitive::array::array_len;
#use std_core::primitive::i32::i32;
#entrypoint main;

#fn read_len : (array_ref : & ArrayVL i32) -> i32 {
    array_len array_ref
}

#fn main : () #with IO {
    #let arrayvl : ArrayVL i32 <- IO::arrayvl_new i32 1i32;
    #letref array_ref : & ArrayVL i32 #borrow arrayvl;
    #let code : i32 = read_len array_ref;
    #let _ : () <- IO::exit code;
    ()
}
"#,
            "`array_len` requires `#with IO`",
        ),
    ];

    for (name, source, expected_message) in cases {
        let package = parse_inline_binary_package(name, source);
        let error = lower_package_to_program(&package).expect_err("lowering must reject builtin");
        assert!(
            error.to_string().contains(expected_message),
            "expected `{expected_message}`, got `{error}`"
        );
    }
}

#[test]
fn rejects_effectful_operations_in_function_arguments() {
    let cases = [
        (
            "nested-ref-get-argument",
            r#"
#use std_core::io::IO;
#use std_core::primitive::reference::ref_get;
#use std_core::primitive::i32::i32;
#use std_core::primitive::i32::i32_add;
#entrypoint main;

#fn main : () #with IO {
    #let value : i32 = 41i32;
    #letref value_ref : & i32 #borrow value;
    #let code : i32 = i32_add (ref_get i32 value_ref) 1i32;
    #let _ : () <- IO::exit code;
    ()
}
"#,
            "`ref_get` is effectful and must be used as a top-level statement",
        ),
        (
            "nested-io-call-argument",
            r#"
#use std_core::io::IO;
#use std_core::io::FileDescriptor;
#entrypoint main;

#fn main : () #with IO {
    #let bytes_ref : & ArrayVL u8 = "x";
    #let _ : () <- IO::write (IO::stdout) bytes_ref 1i32;
    ()
}
"#,
            "`IO::stdout` is effectful and must be used as a top-level statement",
        ),
        (
            "nested-io-function-argument",
            r#"
#use std_core::io::IO;
#use std_core::primitive::reference::ref_get;
#use std_core::primitive::i32::i32;
#use std_core::primitive::i32::i32_add;
#entrypoint main;

#fn read_ref : (value_ref : & i32) -> i32 #with IO {
    ref_get i32 value_ref
}

#fn main : () #with IO {
    #let value : i32 = 41i32;
    #letref value_ref : & i32 #borrow value;
    #let code : i32 = i32_add (read_ref value_ref) 1i32;
    #let _ : () <- IO::exit code;
    ()
}
"#,
            "`read_ref` is effectful and must be used as a top-level statement",
        ),
        (
            "nested-io-operation-in-function-tail",
            r#"
#use std_core::io::IO;
#use std_core::primitive::reference::ref_get;
#use std_core::primitive::i32::i32;
#use std_core::primitive::i32::i32_add;
#entrypoint main;

#fn add1 : (value_ref : & i32) -> i32 #with IO {
    i32_add (ref_get i32 value_ref) 1i32
}

#fn main : () #with IO {
    #let value : i32 = 41i32;
    #letref value_ref : & i32 #borrow value;
    #let code : i32 = add1 value_ref;
    #let _ : () <- IO::exit code;
    ()
}
"#,
            "`ref_get` is effectful and must be used as a top-level statement",
        ),
    ];

    for (name, source, expected_message) in cases {
        let package = parse_inline_binary_package(name, source);
        let error =
            lower_package_to_program(&package).expect_err("lowering must reject nested IO effect");
        assert!(
            error.to_string().contains(expected_message),
            "expected `{expected_message}`, got `{error}`"
        );
    }
}

#[test]
fn lowers_fn_reference_annotation_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/fn-reference-annotation");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.arrays,
        vec![ArrayAllocation {
            slot: 0,
            len: 2,
            element_type: ArrayElementType::I32,
            kind: ArrayKind::Fixed,
        }]
    );
    assert_eq!(
        program.operations,
        vec![
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::Literal(39),
            },
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::Literal(40),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(0),
                value: I32Expr::Literal(1),
            },
            Operation::ArraySetI32 {
                array_slot: 0,
                index: I64Expr::Literal(1),
                value: I32Expr::Literal(2),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Local(0)),
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
            ))),
        ]
    );
}

#[test]
fn lowers_fn_cli_arg_reference_fixture_to_runtime_expression_tree() {
    let root = repo_root().join("tests/testcases/fn-cli-arg-reference");
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
            Operation::StoreI32 {
                slot: 0,
                value: I32Expr::FromU8(Box::new(U8Expr::RuntimeArgGet {
                    arg_index: Box::new(I32Expr::Literal(1)),
                    index: Box::new(I64Expr::Literal(0)),
                })),
            },
            Operation::StoreI32 {
                slot: 1,
                value: I32Expr::FromU8(Box::new(U8Expr::RuntimeArgGet {
                    arg_index: Box::new(I32Expr::Literal(1)),
                    index: Box::new(I64Expr::Literal(1)),
                })),
            },
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Local(0)),
                Box::new(I32Expr::Local(1)),
            ))),
        ]
    );
}

#[test]
fn rejects_io_arg_as_unsized_array_reference() {
    let package = parse_inline_binary_package(
        "io-arg-unsized-array-reference",
        r#"
#use std_core::io::IO;
#use std_core::primitive::array::Array;
#entrypoint main;

#fn main : () #with IO {
    #let arg_ref : & Array u8 <- IO::arg 1i32;
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject IO::arg as Array u8");
    assert!(
        error
            .to_string()
            .contains("expected a value of type `& Array u8`")
    );
}

#[test]
fn rejects_io_arg_as_arrayvl_value() {
    let package = parse_inline_binary_package(
        "io-arg-arrayvl-value",
        r#"
#use std_core::io::IO;
#use std_core::primitive::array::ArrayVL;
#entrypoint main;

#fn main : () #with IO {
    #let arg : ArrayVL u8 <- IO::arg 1i32;
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject IO::arg as ArrayVL");
    assert!(
        error
            .to_string()
            .contains("expected a value of type `ArrayVL u8`")
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
fn lowers_type_rc_match_reference_fixture_to_runtime_exit() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let package = selected_fixture_package(&root, "type-rc-match-reference");

    let program = lower_package_to_program(&package).expect("lower fixture");
    assert_eq!(
        program.operations.last(),
        Some(&Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
            Box::new(I32Expr::Literal(20)),
            Box::new(I32Expr::Add(
                Box::new(I32Expr::Literal(22)),
                Box::new(I32Expr::Literal(0)),
            )),
        ))))
    );
    assert!(program.arrays.is_empty());
    assert!(program.data.is_empty());
    assert_eq!(program.i32_slots, 0);
    assert_eq!(program.heap_slots, 3);
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
    assert_eq!(program.data.len(), 1);
    let ptx = String::from_utf8_lossy(&program.data[0]);
    assert!(ptx.contains(".visible .entry empty_kernel()"));
    assert!(program.data[0].ends_with(&[0]));
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
            Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(4))),
        ]
    );
    assert_eq!(program.i32_slots, 5);
    assert_eq!(program.i64_slots, 2);

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
            "cuModuleLoadData"
        ]
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
        i64_slots: 0,
        f32_slots: 0,
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
        i64_slots: 0,
        f32_slots: 0,
        requires_argv: false,
    };
    let elf = build_linux_x86_64_program_executable(&program)
        .to_bytes()
        .expect("serialize ELF");

    assert_eq!(&elf[0..4], b"\x7FELF");
    assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
    assert_eq!(&elf[0x1005..0x1007], &[0x89, 0xc7]);
    assert_eq!(&elf[0x1007..0x1009], &[0x48, 0xbe]);
    assert_eq!(&elf[0x1009..0x1011], &0x410000_u64.to_le_bytes());
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
        i64_slots: 0,
        f32_slots: 0,
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
