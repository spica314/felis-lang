use std::path::PathBuf;

use neco_rs_parser::{
    ElseBranch, Item, ParsedRoot, Pattern, SourceFileRole, Statement, Term, Visibility,
    parse_root,
    parse_source,
};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

#[test]
fn parses_single_package_root() {
    let root = repo_root().join("tests/testcases/hello-world");
    let parsed = parse_root(&root).expect("hello-world package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "hello-world");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 3);
    let Item::Function(main_fn) = &syntax.items[2] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 4);
}

#[test]
fn parses_workspace_root_and_member_packages() {
    let root = repo_root().join("tests/testcases/workspace-basic");
    let parsed = parse_root(&root).expect("workspace parses");
    let ParsedRoot::Workspace(workspace) = parsed else {
        panic!("expected workspace root");
    };

    assert_eq!(workspace.manifest.members.len(), 2);
    assert_eq!(workspace.packages.len(), 2);

    let member_names: Vec<_> = workspace
        .packages
        .iter()
        .map(|package| package.manifest.name.as_str())
        .collect();
    assert!(member_names.contains(&"workspace-app"));
    assert!(member_names.contains(&"workspace-lib"));
}

#[test]
fn parses_array_basic_package_root() {
    let root = repo_root().join("tests/testcases/array-basic");
    let parsed = parse_root(&root).expect("array-basic package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "array-basic");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 5);
    let Item::Function(main_fn) = &syntax.items[4] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 10);
}

#[test]
fn parses_array_type_annotation_package_root() {
    let root = repo_root().join("tests/testcases/array-type-annotation");
    let parsed = parse_root(&root).expect("array-type-annotation package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "array-type-annotation");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 7);

    let Item::Function(sum3_fn) = &syntax.items[5] else {
        panic!("expected helper function");
    };
    let Term::Arrow(sum3_ty) = &sum3_fn.ty else {
        panic!("expected function arrow type");
    };
    let neco_rs_parser::ArrowParameter::Binder(array_ref) = &sum3_ty.parameter else {
        panic!("expected named array parameter");
    };
    let Term::Reference {
        referent,
        exclusive,
    } = array_ref.ty.as_ref()
    else {
        panic!("expected `&^ Array i32 4i32` reference type");
    };
    assert!(*exclusive);
    let Term::Application {
        callee,
        arguments,
    } = referent.as_ref()
    else {
        panic!("expected `Array i32 4i32` application");
    };
    let Term::Path(array_path) = callee.as_ref() else {
        panic!("expected `Array` callee path");
    };
    assert_eq!(array_path.segments.len(), 1);
    assert_eq!(array_path.segments[0].name, "Array");
    assert_eq!(arguments.len(), 3);
    let Term::Path(element_type_path) = &arguments[0] else {
        panic!("expected element type path");
    };
    assert_eq!(element_type_path.segments[0].name, "i32");
    let Term::IntegerLiteral(length_literal) = &arguments[1] else {
        panic!("expected length literal");
    };
    assert_eq!(length_literal, "4");
    let Term::Path(length_suffix_path) = &arguments[2] else {
        panic!("expected length suffix path");
    };
    assert_eq!(length_suffix_path.segments[0].name, "i32");

    let Item::Function(main_fn) = &syntax.items[6] else {
        panic!("expected main function");
    };
    assert_eq!(main_fn.body.statements.len(), 6);
}

#[test]
fn parses_i32_reference_annotation_package_root() {
    let root = repo_root().join("tests/testcases/i32-reference-annotation");
    let parsed = parse_root(&root).expect("i32-reference-annotation package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "i32-reference-annotation");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 6);

    let Item::Function(add1_fn) = &syntax.items[4] else {
        panic!("expected helper function");
    };
    let Term::Arrow(add1_ty) = &add1_fn.ty else {
        panic!("expected function arrow type");
    };
    let neco_rs_parser::ArrowParameter::Binder(value_ref) = &add1_ty.parameter else {
        panic!("expected named i32 parameter");
    };
    let Term::Reference {
        referent,
        exclusive,
    } = value_ref.ty.as_ref()
    else {
        panic!("expected `& i32` reference type");
    };
    assert!(!exclusive);
    let Term::Path(i32_path) = referent.as_ref() else {
        panic!("expected i32 path");
    };
    assert_eq!(i32_path.segments[0].name, "i32");

    let Item::Function(main_fn) = &syntax.items[5] else {
        panic!("expected main function");
    };
    assert_eq!(main_fn.body.statements.len(), 3);
}

#[test]
fn parses_proc_reference_annotation_package_root() {
    let root = repo_root().join("tests/testcases/proc-reference-annotation");
    let parsed = parse_root(&root).expect("proc-reference-annotation package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "proc-reference-annotation");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 8);

    let Item::Function(fill_refs_proc) = &syntax.items[5] else {
        panic!("expected helper procedure");
    };
    assert_eq!(fill_refs_proc.kind, neco_rs_parser::FunctionKind::Proc);
    let Term::Arrow(fill_refs_ty) = &fill_refs_proc.ty else {
        panic!("expected procedure arrow type");
    };
    let neco_rs_parser::ArrowParameter::Binder(value_ref) = &fill_refs_ty.parameter else {
        panic!("expected named i32 reference parameter");
    };
    let Term::Reference {
        referent,
        exclusive,
    } = value_ref.ty.as_ref()
    else {
        panic!("expected `&^ i32` reference type");
    };
    assert!(*exclusive);
    let Term::Path(i32_path) = referent.as_ref() else {
        panic!("expected i32 path");
    };
    assert_eq!(i32_path.segments[0].name, "i32");

    let Term::Arrow(array_arrow) = fill_refs_ty.result.as_ref() else {
        panic!("expected second procedure parameter");
    };
    let neco_rs_parser::ArrowParameter::Binder(array_ref) = &array_arrow.parameter else {
        panic!("expected named array parameter");
    };
    let Term::Reference {
        referent,
        exclusive,
    } = array_ref.ty.as_ref()
    else {
        panic!("expected `&^ Array i32 2i32` reference type");
    };
    assert!(*exclusive);
    let Term::Application {
        callee,
        arguments,
    } = referent.as_ref()
    else {
        panic!("expected `Array i32 2i32` application");
    };
    let Term::Path(array_path) = callee.as_ref() else {
        panic!("expected `Array` callee path");
    };
    assert_eq!(array_path.segments[0].name, "Array");
    assert_eq!(arguments.len(), 3);
    assert!(matches!(array_arrow.result.as_ref(), Term::Unit));

    let Item::Function(main_fn) = &syntax.items[7] else {
        panic!("expected main function");
    };
    assert_eq!(main_fn.body.statements.len(), 5);
}

#[test]
fn parses_proc_cli_arg_reference_package_root() {
    let root = repo_root().join("tests/testcases/proc-cli-arg-reference");
    let parsed = parse_root(&root).expect("proc-cli-arg-reference package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "proc-cli-arg-reference");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 8);

    let Item::Function(read_digits_proc) = &syntax.items[6] else {
        panic!("expected helper procedure");
    };
    assert_eq!(read_digits_proc.kind, neco_rs_parser::FunctionKind::Proc);
    let Term::Arrow(digits_arrow) = &read_digits_proc.ty else {
        panic!("expected procedure arrow type");
    };
    let neco_rs_parser::ArrowParameter::Binder(digits_ref) = &digits_arrow.parameter else {
        panic!("expected named byte-sequence parameter");
    };
    let Term::Reference { referent, exclusive } = digits_ref.ty.as_ref() else {
        panic!("expected reference type");
    };
    assert!(*exclusive);
    let Term::Application { callee, arguments } = referent.as_ref() else {
        panic!("expected `Array u8` application");
    };
    let Term::Path(array_path) = callee.as_ref() else {
        panic!("expected `Array` callee path");
    };
    assert_eq!(array_path.segments[0].name, "Array");
    assert_eq!(arguments.len(), 1);
    let Term::Path(element_type_path) = &arguments[0] else {
        panic!("expected element type path");
    };
    assert_eq!(element_type_path.segments[0].name, "u8");
    let Term::Arrow(left_arrow) = digits_arrow.result.as_ref() else {
        panic!("expected left i32 reference parameter");
    };
    let neco_rs_parser::ArrowParameter::Binder(left_ref) = &left_arrow.parameter else {
        panic!("expected named left i32 reference parameter");
    };
    let Term::Reference {
        referent,
        exclusive,
    } = left_ref.ty.as_ref()
    else {
        panic!("expected `&^ i32` reference type");
    };
    assert!(*exclusive);
    let Term::Path(i32_path) = referent.as_ref() else {
        panic!("expected i32 path");
    };
    assert_eq!(i32_path.segments[0].name, "i32");

    let Term::Arrow(right_arrow) = left_arrow.result.as_ref() else {
        panic!("expected right i32 reference parameter");
    };
    let neco_rs_parser::ArrowParameter::Binder(right_ref) = &right_arrow.parameter else {
        panic!("expected named right i32 reference parameter");
    };
    let Term::Reference {
        referent,
        exclusive,
    } = right_ref.ty.as_ref()
    else {
        panic!("expected `&^ i32` reference type");
    };
    assert!(*exclusive);
    let Term::Path(i32_path) = referent.as_ref() else {
        panic!("expected i32 path");
    };
    assert_eq!(i32_path.segments[0].name, "i32");
    assert!(matches!(right_arrow.result.as_ref(), Term::Unit));

    let Item::Function(main_fn) = &syntax.items[7] else {
        panic!("expected main function");
    };
    assert_eq!(main_fn.body.statements.len(), 6);
}

#[test]
fn parses_u8_array_hello_world_package_root() {
    let root = repo_root().join("tests/testcases/u8-array-hello-world");
    let parsed = parse_root(&root).expect("u8-array-hello-world package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "u8-array-hello-world");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 4);
    let Item::Function(main_fn) = &syntax.items[3] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 16);
}

#[test]
fn parses_hex_literals_package_root() {
    let root = repo_root().join("tests/testcases/hex-literals");
    let parsed = parse_root(&root).expect("hex-literals package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "hex-literals");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 4);
    let Item::Function(main_fn) = &syntax.items[3] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 5);
}

#[test]
fn parses_comments_basic_package_root() {
    let root = repo_root().join("tests/testcases/comments-basic");
    let parsed = parse_root(&root).expect("comments-basic package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "comments-basic");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 3);
    let Item::Function(main_fn) = &syntax.items[2] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 4);
}

#[test]
fn parses_ascii_char_literals() {
    let source = r#"
#fn first_char : u8 {
    'A'
}

#fn newline_char : u8 {
    '\n'
}
"#;
    let (_, syntax) = parse_source(source).expect("source parses");
    let syntax = syntax.expect("source file");
    assert_eq!(syntax.items.len(), 2);

    let Item::Function(first_char) = &syntax.items[0] else {
        panic!("expected function");
    };
    let Some(Term::CharLiteral('A')) = first_char.body.tail.as_deref() else {
        panic!("expected `A` char literal");
    };

    let Item::Function(newline_char) = &syntax.items[1] else {
        panic!("expected function");
    };
    let Some(Term::CharLiteral('\n')) = newline_char.body.tail.as_deref() else {
        panic!("expected newline char literal");
    };
}

#[test]
fn rejects_non_ascii_char_literals() {
    let error = parse_source("'あ'").expect_err("source should fail");
    assert_eq!(error.message, "char literal must be ASCII");
}

#[test]
fn parses_enum_match_basic_package_root() {
    let root = repo_root().join("tests/testcases/enum-match-basic");
    let parsed = parse_root(&root).expect("enum-match-basic package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "enum-match-basic");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 6);

    let Item::Type(type_decl) = &syntax.items[2] else {
        panic!("expected type declaration");
    };
    assert_eq!(type_decl.name.name, "Color");
    assert_eq!(type_decl.constructors.len(), 3);

    let Item::Function(function) = &syntax.items[3] else {
        panic!("expected pure function");
    };
    let Some(Term::Match(match_expr)) = function.body.tail.as_deref() else {
        panic!("expected match expression");
    };
    assert_eq!(match_expr.arms.len(), 3);
    for arm in &match_expr.arms {
        let Pattern::Constructor { subpatterns, .. } = &arm.pattern else {
            panic!("expected constructor pattern");
        };
        assert!(subpatterns.is_empty());
    }

    let Item::Function(main_fn) = &syntax.items[5] else {
        panic!("expected main function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 2);
}

#[test]
fn parses_enum_match_payload_package_root() {
    let root = repo_root().join("tests/testcases/enum-match-payload");
    let parsed = parse_root(&root).expect("enum-match-payload package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "enum-match-payload");
    assert_eq!(package.source_files.len(), 2);
    assert_eq!(
        package.manifest.felis_bin_entrypoints,
        vec![
            PathBuf::from("src/enum-match-payload-single.fe"),
            PathBuf::from("src/enum-match-payload-pair.fe"),
        ]
    );

    for source_file in &package.source_files {
        assert_eq!(source_file.role, SourceFileRole::BinaryEntrypoint);
        let syntax = &source_file.syntax;
        assert_eq!(syntax.items.len(), 7);

        let type_decl = syntax
            .items
            .iter()
            .find_map(|item| match item {
                Item::Type(type_decl) => Some(type_decl),
                _ => None,
            })
            .expect("type declaration");
        assert_eq!(type_decl.name.name, "Value");
        assert_eq!(type_decl.constructors.len(), 2);

        let function = syntax
            .items
            .iter()
            .find_map(|item| match item {
                Item::Function(function) if function.name.name == "value_code" => Some(function),
                _ => None,
            })
            .expect("pure function");
        let Some(Term::Match(match_expr)) = function.body.tail.as_deref() else {
            panic!("expected match expression");
        };
        assert_eq!(match_expr.arms.len(), 2);

        let Pattern::Constructor { subpatterns, .. } = &match_expr.arms[0].pattern else {
            panic!("expected constructor pattern");
        };
        assert_eq!(subpatterns.len(), 1);
        assert!(matches!(&subpatterns[0], Pattern::Bind(name) if name == "x"));

        let Pattern::Constructor { subpatterns, .. } = &match_expr.arms[1].pattern else {
            panic!("expected constructor pattern");
        };
        assert_eq!(subpatterns.len(), 2);
        assert!(matches!(&subpatterns[0], Pattern::Bind(name) if name == "x"));
        assert!(matches!(&subpatterns[1], Pattern::Bind(name) if name == "y"));
    }
}

#[test]
fn parses_type_rc_match_package_root() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let parsed = parse_root(&root).expect("type-rc-match package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "type-rc-match");
    assert_eq!(package.source_files.len(), 3);
    assert_eq!(
        package.manifest.felis_bin_entrypoints,
        vec![
            PathBuf::from("src/type-rc-match-single.fe"),
            PathBuf::from("src/type-rc-match-pair.fe"),
            PathBuf::from("src/type-rc-match-list.fe"),
        ]
    );

    for source_file in &package.source_files {
        assert_eq!(source_file.role, SourceFileRole::BinaryEntrypoint);
        let syntax = &source_file.syntax;
        assert_eq!(syntax.items.len(), 7);

        let type_decl = syntax
            .items
            .iter()
            .find_map(|item| match item {
                Item::Type(type_decl) => Some(type_decl),
                _ => None,
            })
            .expect("type declaration");
        assert_eq!(type_decl.modifier.as_deref(), Some("rc"));
        assert_eq!(type_decl.constructors.len(), 2);
        if source_file.path.ends_with("src/type-rc-match-list.fe") {
            assert_eq!(type_decl.name.name, "List");

            let function = syntax
                .items
                .iter()
                .find_map(|item| match item {
                    Item::Function(function) if function.name.name == "list_sum" => Some(function),
                    _ => None,
                })
                .expect("pure function");
            let Some(Term::Match(match_expr)) = function.body.tail.as_deref() else {
                panic!("expected match expression");
            };
            assert_eq!(match_expr.arms.len(), 2);

            let Pattern::Constructor { subpatterns, .. } = &match_expr.arms[0].pattern else {
                panic!("expected constructor pattern");
            };
            assert!(subpatterns.is_empty());

            let Pattern::Constructor { subpatterns, .. } = &match_expr.arms[1].pattern else {
                panic!("expected constructor pattern");
            };
            assert_eq!(subpatterns.len(), 2);
            assert!(matches!(&subpatterns[0], Pattern::Bind(name) if name == "x"));
            assert!(matches!(&subpatterns[1], Pattern::Bind(name) if name == "xs"));

            assert!(matches!(match_expr.arms[1].result.as_ref(), Term::Block(_)));
        } else {
            assert_eq!(type_decl.name.name, "Value");

            let function = syntax
                .items
                .iter()
                .find_map(|item| match item {
                    Item::Function(function) if function.name.name == "value_code" => {
                        Some(function)
                    }
                    _ => None,
                })
                .expect("pure function");
            let Some(Term::Match(match_expr)) = function.body.tail.as_deref() else {
                panic!("expected match expression");
            };
            assert_eq!(match_expr.arms.len(), 2);

            let Pattern::Constructor { subpatterns, .. } = &match_expr.arms[0].pattern else {
                panic!("expected constructor pattern");
            };
            assert_eq!(subpatterns.len(), 1);
            assert!(matches!(&subpatterns[0], Pattern::Bind(name) if name == "x"));

            let Pattern::Constructor { subpatterns, .. } = &match_expr.arms[1].pattern else {
                panic!("expected constructor pattern");
            };
            assert_eq!(subpatterns.len(), 2);
            assert!(matches!(&subpatterns[0], Pattern::Bind(name) if name == "x"));
            assert!(matches!(&subpatterns[1], Pattern::Bind(name) if name == "y"));

            let Term::Path(path) = match_expr.arms[0].result.as_ref() else {
                panic!("expected bound value in first arm");
            };
            assert_eq!(path.segments.len(), 1);
            assert_eq!(path.segments[0].name, "x");

            let Term::Application { arguments, .. } = match_expr.arms[1].result.as_ref() else {
                panic!("expected i32_add application in second arm");
            };
            assert_eq!(arguments.len(), 2);
            assert!(
                matches!(&arguments[0], Term::Path(path) if path.segments.len() == 1 && path.segments[0].name == "x")
            );
            assert!(
                matches!(&arguments[1], Term::Path(path) if path.segments.len() == 1 && path.segments[0].name == "y")
            );
        }
    }
}

#[test]
fn parses_stdin_to_stdout_package_root() {
    let root = repo_root().join("tests/testcases/stdin-to-stdout");
    let parsed = parse_root(&root).expect("stdin-to-stdout package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "stdin-to-stdout");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 4);
    let Item::Function(main_fn) = &syntax.items[3] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 5);
}

#[test]
fn parses_open_read_close_package_root() {
    let root = repo_root().join("tests/testcases/open-read-close");
    let parsed = parse_root(&root).expect("open-read-close package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "open-read-close");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 4);
    let Item::Function(main_fn) = &syntax.items[3] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 8);
}

#[test]
fn parses_open_write_close_package_root() {
    let root = repo_root().join("tests/testcases/open-write-close");
    let parsed = parse_root(&root).expect("open-write-close package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "open-write-close");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 3);
    let Item::Function(main_fn) = &syntax.items[2] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 7);
}

#[test]
fn parses_fn_call_package_root() {
    let root = repo_root().join("tests/testcases/fn-call");
    let parsed = parse_root(&root).expect("fn-call package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "fn-call");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 10);

    let Item::Function(function) = &syntax.items[8] else {
        panic!("expected helper function");
    };
    assert!(function.effect.is_none());
    assert_eq!(function.body.statements.len(), 10);

    let Item::Function(main_fn) = &syntax.items[9] else {
        panic!("expected entrypoint function");
    };
    assert!(main_fn.effect.is_some());
    let Statement::Let(let_stmt) = &main_fn.body.statements[0] else {
        panic!("expected let statement");
    };
    match let_stmt.value.as_ref() {
        Term::Application { callee, arguments } => {
            match callee.as_ref() {
                Term::Path(path) => assert_eq!(path.segments[0].name, "f"),
                other => panic!("expected path callee, got {other:?}"),
            }
            assert_eq!(arguments.len(), 2);
        }
        other => panic!("expected function application, got {other:?}"),
    }
}

#[test]
fn parses_if_package_root_with_else_branches() {
    let root = repo_root().join("tests/testcases/if");
    let parsed = parse_root(&root).expect("if package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "if");
    assert_eq!(package.source_files.len(), 6);

    let else_true = package
        .source_files
        .iter()
        .find(|file| {
            file.path
                .ends_with("tests/testcases/if/src/if-else-true.fe")
        })
        .expect("if-else-true file");
    let Item::Function(main_fn) = &else_true.syntax.items[3] else {
        panic!("expected function");
    };
    let Statement::If(if_stmt) = &main_fn.body.statements[0] else {
        panic!("expected if statement");
    };
    assert!(matches!(if_stmt.else_branch, Some(ElseBranch::Block(_))));

    let else_if = package
        .source_files
        .iter()
        .find(|file| {
            file.path
                .ends_with("tests/testcases/if/src/if-else-if-true.fe")
        })
        .expect("if-else-if-true file");
    let Item::Function(main_fn) = &else_if.syntax.items[3] else {
        panic!("expected function");
    };
    let Statement::If(if_stmt) = &main_fn.body.statements[0] else {
        panic!("expected if statement");
    };
    let Some(ElseBranch::If(else_if_stmt)) = &if_stmt.else_branch else {
        panic!("expected else-if branch");
    };
    assert!(matches!(else_if_stmt.else_branch, Some(ElseBranch::Block(_))));
}

#[test]
fn parses_loop_package_root() {
    let root = repo_root().join("tests/testcases/loop");
    let parsed = parse_root(&root).expect("loop package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "loop");
    assert_eq!(package.source_files.len(), 1);
    let syntax = &package.source_files[0].syntax;
    let Item::Function(main_fn) = &syntax.items[4] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.body.statements.len(), 4);

    let Statement::Loop(loop_stmt) = &main_fn.body.statements[2] else {
        panic!("expected loop statement");
    };
    assert_eq!(loop_stmt.body.statements.len(), 5);
    assert!(matches!(loop_stmt.body.statements[2], Statement::If(_)));
}

#[test]
fn parses_continue_package_root() {
    let root = repo_root().join("tests/testcases/continue");
    let parsed = parse_root(&root).expect("continue package parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert_eq!(package.manifest.name, "continue");
    assert_eq!(package.source_files.len(), 1);
    let syntax = &package.source_files[0].syntax;
    let Item::Function(main_fn) = &syntax.items[4] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.body.statements.len(), 4);

    let Statement::Loop(loop_stmt) = &main_fn.body.statements[2] else {
        panic!("expected loop statement");
    };
    assert_eq!(loop_stmt.body.statements.len(), 6);
    assert!(matches!(loop_stmt.body.statements[0], Statement::Let(_)));
    assert!(matches!(
        loop_stmt.body.statements[1],
        Statement::Expression(_)
    ));
    assert!(matches!(loop_stmt.body.statements[2], Statement::If(_)));
    assert!(matches!(loop_stmt.body.statements[3], Statement::Let(_)));
    assert!(matches!(
        loop_stmt.body.statements[4],
        Statement::Expression(_)
    ));
    assert!(matches!(loop_stmt.body.statements[5], Statement::If(_)));
}

#[test]
fn parses_std_workspace_packages_with_nested_modules_and_theorems() {
    let root = repo_root().join("std");
    let parsed = parse_root(&root).expect("std parses");
    let ParsedRoot::Workspace(workspace) = parsed else {
        panic!("expected workspace root");
    };

    assert_eq!(workspace.packages.len(), 2);

    let core = workspace
        .packages
        .iter()
        .find(|package| package.manifest.name == "std_core")
        .expect("std_core package");
    assert_eq!(
        core.manifest.felis_lib_entrypoint,
        Some(PathBuf::from("src/lib.fe"))
    );

    let math = workspace
        .packages
        .iter()
        .find(|package| package.manifest.name == "std_math")
        .expect("std_math package");

    assert!(math.source_files.len() >= 5);
    let nat_add_file = math
        .source_files
        .iter()
        .find(|file| file.path.ends_with("std/std_math/src/math/nat/nat_add.fe"))
        .expect("nat_add file");

    let theorem = nat_add_file
        .syntax
        .items
        .iter()
        .find_map(|item| match item {
            Item::Theorem(theorem) if theorem.name.name == "nat_add_x_zero_eq_x" => Some(theorem),
            _ => None,
        })
        .expect("theorem");

    assert_eq!(theorem.visibility, Visibility::Public);
    assert_eq!(theorem.body.statements.len(), 1);

    let Statement::Item(proof_item) = &theorem.body.statements[0] else {
        panic!("expected nested proof item");
    };
    let Item::Function(proof_fn) = proof_item.as_ref() else {
        panic!("expected nested function");
    };
    let Some(match_stmt) = proof_fn.body.tail.as_deref() else {
        panic!("expected match expression");
    };
    let Term::Match(match_expr) = match_stmt else {
        panic!("expected match expression");
    };
    let Pattern::Constructor { subpatterns, .. } = &match_expr.arms[1].pattern else {
        panic!("expected constructor pattern");
    };
    assert_eq!(subpatterns.len(), 1);

    let Term::Block(second_arm_block) = match_expr.arms[1].result.as_ref() else {
        panic!("expected block in second arm");
    };
    let Statement::Let(let_stmt) = &second_arm_block.statements[0] else {
        panic!("expected let statement in second arm block");
    };
    match let_stmt.value.as_ref() {
        Term::Application { .. } => {}
        other => panic!("expected application term, got {other:?}"),
    }
}
