use std::path::PathBuf;

use neco_rs_parser::{ParsedRoot, parse_root, parse_source};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

#[test]
fn parses_package_fixtures() {
    let root = repo_root().join("tests/testcases");
    let entries = std::fs::read_dir(&root).expect("testcases dir");

    for entry in entries {
        let path = entry.expect("testcase entry").path();
        if !path.join("neco-package.json").exists() {
            continue;
        }

        parse_root(&path)
            .unwrap_or_else(|error| panic!("fixture `{}` should parse: {error}", path.display()));
    }
}

#[test]
fn parses_std_workspace() {
    let parsed = parse_root(repo_root().join("std")).expect("std workspace parses");
    let ParsedRoot::Workspace(workspace) = parsed else {
        panic!("expected workspace root");
    };
    assert!(!workspace.packages.is_empty());
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
}

#[test]
fn rejects_non_ascii_char_literals() {
    let source = r#"
#fn non_ascii_char : u8 {
    'あ'
}
"#;
    let error = parse_source(source).expect_err("source should fail");
    assert!(
        error.to_string().contains("char literal must be ASCII"),
        "unexpected error: {error}"
    );
}
