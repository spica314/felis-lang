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
fn rejects_workspace_dependency_without_workspace() {
    let root = repo_root().join("tests/testcases/workspace-dependency-without-workspace");
    let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
        panic!("expected package root");
    };

    let error = lower_package_to_program(&package)
        .expect_err("lowering must reject workspace dependencies without workspace");
    assert!(
        error
            .to_string()
            .contains("`workspace: true` dependencies require a containing workspace"),
        "unexpected error: {error}"
    );
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
fn rejects_unknown_package_qualified_use() {
    let package = parse_inline_binary_package(
        "unknown-package-qualified-use",
        r#"
#use missing-package::default_value;
#entrypoint main;

#fn default_value : i32 {
    42i32
}

#fn main : () {
    ()
}
"#,
    );

    let error =
        lower_package_to_program(&package).expect_err("lowering must reject unknown package use");
    assert!(
        error
            .to_string()
            .contains("unknown package-qualified use `missing-package`"),
        "unexpected error: {error}"
    );
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
