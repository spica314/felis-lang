use super::DependencySource;
use super::model::Manifest;
use super::parse::parse_manifest;
use std::path::{Path, PathBuf};

#[test]
fn parses_package_manifest_without_serde() {
    let manifest = parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "felis-bin-entrypoints": ["src/main.fe"],
                "dependencies": {
                    "std": { "workspace": true }
                }
            }"#,
    )
    .expect("manifest parses");

    let Manifest::Package(package) = manifest else {
        panic!("expected package");
    };

    assert_eq!(package.name, "hello-world");
    assert_eq!(
        package.felis_bin_entrypoints,
        vec![PathBuf::from("src/main.fe")]
    );
    assert_eq!(package.dependencies.len(), 1);
    assert_eq!(package.dependencies[0].source, DependencySource::Workspace);
}

#[test]
fn rejects_non_workspace_dependency_entries() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "dependencies": {
                    "std": { "workspace": false }
                }
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject non-workspace dependency entries"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("dependency entry only supports `workspace: true`"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn parses_workspace_manifest_without_serde() {
    let manifest = parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "workspace": {
                    "members": ["app", "lib"]
                }
            }"#,
    )
    .expect("manifest parses");

    let Manifest::Workspace(workspace) = manifest else {
        panic!("expected workspace");
    };

    assert_eq!(
        workspace.members,
        vec![PathBuf::from("app"), PathBuf::from("lib")]
    );
}
