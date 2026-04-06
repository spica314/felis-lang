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
fn parses_std_package_with_nested_modules_and_theorems() {
    let root = repo_root().join("std");
    let parsed = parse_root(&root).expect("std parses");
    let ParsedRoot::Package(package) = parsed else {
        panic!("expected package root");
    };

    assert!(package.source_files.len() >= 6);
    let nat_add_file = package
        .source_files
        .iter()
        .find(|file| file.path.ends_with("std/src/math/nat/nat_add.fe"))
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
