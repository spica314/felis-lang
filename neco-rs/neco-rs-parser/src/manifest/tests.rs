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
                "felis-test-entrypoints": ["src/main-test.fe"],
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
    assert_eq!(
        package.felis_test_entrypoints,
        vec![PathBuf::from("src/main-test.fe")]
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
fn rejects_native_libraries_without_libc_start() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "felis-bin-entrypoints": ["src/main.fe"],
                "native-libraries": ["m"]
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject native libraries without libc-start"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("`native-libraries` requires `native-link-mode` to be `libc-start`"),
        "unexpected error: {error:?}"
    );
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
fn rejects_duplicate_dependency_entries() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "dependencies": {
                    "std": { "workspace": true },
                    "std": { "workspace": true }
                }
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject duplicate dependency entries"),
        Err(error) => error,
    };

    assert!(
        error.to_string().contains("duplicate dependency `std`"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn rejects_duplicate_binary_entrypoint_paths() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "felis-bin-entrypoints": ["src/main.fe", "./src/main.fe"]
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject duplicate binary entrypoint paths"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("duplicate binary entrypoint path `src/main.fe`"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn rejects_duplicate_test_entrypoint_paths() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "felis-test-entrypoints": ["src/main-test.fe", "./src/main-test.fe"]
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject duplicate test entrypoint paths"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("duplicate test entrypoint path `src/main-test.fe`"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn rejects_unknown_package_fields() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "felis-bin-entrypoint": "src/main.fe"
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject unknown package fields"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("manifest contains unknown field `felis-bin-entrypoint`"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn rejects_unknown_workspace_fields() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "workspace": {
                    "members": ["app"],
                    "member": ["lib"]
                }
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject unknown workspace fields"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("`workspace` contains unknown field `member`"),
        "unexpected error: {error:?}"
    );
}

#[test]
fn rejects_unknown_dependency_fields() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "name": "hello-world",
                "dependencies": {
                    "std": { "workspacce": true }
                }
            }"#,
    ) {
        Ok(_) => panic!("manifest should reject unknown dependency fields"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("dependency entry contains unknown field `workspacce`"),
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
fn rejects_empty_workspace_members_list() {
    let error = match parse_manifest(
        Path::new("neco-package.json"),
        r#"{
                "workspace": {
                    "members": []
                }
            }"#,
    ) {
        Ok(_) => panic!("workspace members list should not be empty"),
        Err(error) => error,
    };

    assert!(
        error
            .to_string()
            .contains("`workspace.members` must contain at least one path"),
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
