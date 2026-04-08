use std::path::PathBuf;

use neco_rs_parser::{
    Item, ParsedRoot, Pattern, SourceFileRole, Statement, Term, Visibility, parse_root,
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
fn parses_neco_felis_workspace_root() {
    let root = repo_root().join("neco-felis");
    let parsed = parse_root(&root).expect("neco-felis workspace parses");
    let ParsedRoot::Workspace(workspace) = parsed else {
        panic!("expected workspace root");
    };

    assert_eq!(workspace.manifest.members.len(), 1);
    assert_eq!(workspace.packages.len(), 1);

    let package = &workspace.packages[0];
    assert_eq!(package.manifest.name, "neco-felis");
    assert_eq!(package.source_files.len(), 1);
    assert_eq!(
        package.source_files[0].role,
        SourceFileRole::BinaryEntrypoint
    );

    let syntax = &package.source_files[0].syntax;
    assert_eq!(syntax.items.len(), 16);
    let Item::Function(main_fn) = &syntax.items[15] else {
        panic!("expected function");
    };
    assert_eq!(main_fn.visibility, Visibility::Private);
    assert!(main_fn.effect.is_some());
    assert_eq!(main_fn.body.statements.len(), 53);
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
    assert_eq!(package.source_files.len(), 4);

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
    assert!(if_stmt.else_block.is_some());
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
