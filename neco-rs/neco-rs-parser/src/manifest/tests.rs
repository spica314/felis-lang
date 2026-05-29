use super::DependencySource;
use super::model::{Manifest, NativeLinkMode};
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
    assert_eq!(package.native_link_mode, NativeLinkMode::KernelStart);
    assert!(package.native_libraries.is_empty());
}

#[test]
fn parses_native_link_fields() {
    let manifest = parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "felis-bin-entrypoints": ["src/main.fe"],
                "native-link-mode": "libc-start",
                "native-libraries": ["m", "pthread"]
            }"#,
    )
    .expect("manifest parses");

    let Manifest::Package(package) = manifest else {
        panic!("expected package");
    };

    assert_eq!(package.native_link_mode, NativeLinkMode::LibcStart);
    assert_eq!(package.native_libraries, vec!["m", "pthread"]);
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
fn reports_multiline_json_error_positions() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
  "name": @
}"#,
    ) {
        Ok(_) => panic!("manifest should reject invalid JSON"),
        Err(error) => error,
    };

    assert!(
        error.to_string().contains("neco-package.json:1:10..1:11"),
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

#[test]
fn rejects_empty_workspace_member_path() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "workspace": {
                    "members": [""]
                }
            }"#,
    ) {
        Ok(_) => panic!("workspace member path should not be empty"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("workspace member path must not be empty"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn rejects_empty_library_entrypoint_path() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "lib",
                "felis-lib-entrypoint": ""
            }"#,
    ) {
        Ok(_) => panic!("library entrypoint path should not be empty"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("library entrypoint path must not be empty"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn rejects_empty_binary_entrypoint_path() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "bin",
                "felis-bin-entrypoints": [""]
            }"#,
    ) {
        Ok(_) => panic!("binary entrypoint path should not be empty"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("binary entrypoint path must not be empty"),
        "unexpected error: {error:?}"
    );
}
