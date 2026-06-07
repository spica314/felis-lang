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
            "`add1` is effectful and must be bound with `<-`",
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
