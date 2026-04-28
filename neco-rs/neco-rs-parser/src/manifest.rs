use std::path::{Path, PathBuf};

use neco_rs_json::{JsonEntry, JsonValue};

use crate::{Error, FilePos, Result, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageManifest {
    pub name: String,
    pub dependencies: Vec<Dependency>,
    pub felis_lib_entrypoint: Option<PathBuf>,
    pub felis_bin_entrypoints: Vec<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub name: String,
    pub source: DependencySource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencySource {
    Workspace,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceManifest {
    pub members: Vec<PathBuf>,
}

pub(crate) enum Manifest {
    Package(PackageManifest),
    Workspace(WorkspaceManifest),
}

pub(crate) fn parse_manifest(path: &Path, raw: &str) -> Result<Manifest> {
    let (_, value) = neco_rs_json::parse(raw).map_err(|error| convert_json_error(path, error))?;
    let object = expect_object(path, &value, "manifest")?;

    let workspace = optional_field(object, "workspace")?;
    let name = optional_string_field(object, "name")?;
    let dependencies = optional_field(object, "dependencies")?;
    let felis_lib_entrypoint = optional_string_field(object, "felis-lib-entrypoint")?;
    let felis_bin_entrypoints = optional_string_array_field(object, "felis-bin-entrypoints")?;

    match (workspace, name) {
        (Some(workspace), None) => {
            if felis_lib_entrypoint.is_some()
                || !felis_bin_entrypoints.is_empty()
                || dependencies.is_some()
            {
                return Err(
                    Error::new("workspace manifest cannot contain package-only fields")
                        .with_path(path.to_path_buf()),
                );
            }

            let workspace = expect_object(path, workspace, "`workspace`")?;
            let members = required_string_array_field(path, workspace, "members")?
                .into_iter()
                .map(PathBuf::from)
                .collect();

            Ok(Manifest::Workspace(WorkspaceManifest { members }))
        }
        (None, Some(name)) => Ok(Manifest::Package(PackageManifest {
            name,
            dependencies: parse_dependencies(path, dependencies)?,
            felis_lib_entrypoint: felis_lib_entrypoint.map(PathBuf::from),
            felis_bin_entrypoints: felis_bin_entrypoints
                .into_iter()
                .map(PathBuf::from)
                .collect(),
        })),
        (Some(_), Some(_)) => Err(
            Error::new("manifest cannot be both a workspace and a package")
                .with_path(path.to_path_buf()),
        ),
        (None, None) => Err(
            Error::new("manifest must define either `workspace` or `name`")
                .with_path(path.to_path_buf()),
        ),
    }
}

fn parse_dependencies(path: &Path, value: Option<&JsonValue>) -> Result<Vec<Dependency>> {
    let Some(value) = value else {
        return Ok(Vec::new());
    };
    let object = expect_object(path, value, "`dependencies`")?;
    object
        .iter()
        .map(|entry| {
            let dependency = expect_object(path, &entry.value, "dependency entry")?;
            match optional_bool_field(dependency, "workspace")? {
                Some(true) => {}
                Some(false) => {
                    return Err(
                        Error::new("dependency entry only supports `workspace: true`")
                            .with_path(path.to_path_buf()),
                    );
                }
                None => {
                    return Err(Error::new("dependency entry must define `workspace: true`")
                        .with_path(path.to_path_buf()));
                }
            }
            Ok(Dependency {
                name: entry.key.clone(),
                source: DependencySource::Workspace,
            })
        })
        .collect()
}

fn optional_field<'a>(entries: &'a [JsonEntry], key: &str) -> Result<Option<&'a JsonValue>> {
    let mut found = None;
    for entry in entries {
        if entry.key == key {
            if found.is_some() {
                return Err(Error::new(format!("duplicate field `{key}`")));
            }
            found = Some(&entry.value);
        }
    }
    Ok(found)
}

fn optional_string_field(entries: &[JsonEntry], key: &str) -> Result<Option<String>> {
    let Some(value) = optional_field(entries, key)? else {
        return Ok(None);
    };
    expect_string(value, &format!("`{key}`")).map(Some)
}

fn optional_bool_field(entries: &[JsonEntry], key: &str) -> Result<Option<bool>> {
    let Some(value) = optional_field(entries, key)? else {
        return Ok(None);
    };
    expect_bool(value, &format!("`{key}`")).map(Some)
}

fn optional_string_array_field(entries: &[JsonEntry], key: &str) -> Result<Vec<String>> {
    let Some(value) = optional_field(entries, key)? else {
        return Ok(Vec::new());
    };
    expect_string_array(value, &format!("`{key}`"))
}

fn required_string_array_field(
    path: &Path,
    entries: &[JsonEntry],
    key: &str,
) -> Result<Vec<String>> {
    optional_field(entries, key)?
        .ok_or_else(|| {
            Error::new(format!("missing required field `{key}`")).with_path(path.to_path_buf())
        })
        .and_then(|value| expect_string_array(value, &format!("`{key}`")))
}

fn expect_object<'a>(path: &Path, value: &'a JsonValue, context: &str) -> Result<&'a [JsonEntry]> {
    match value {
        JsonValue::Object(entries) => Ok(entries),
        _ => {
            Err(Error::new(format!("{context} must be a JSON object"))
                .with_path(path.to_path_buf()))
        }
    }
}

fn expect_string(value: &JsonValue, context: &str) -> Result<String> {
    match value {
        JsonValue::String(value) => Ok(value.clone()),
        _ => Err(Error::new(format!("{context} must be a string"))),
    }
}

fn expect_bool(value: &JsonValue, context: &str) -> Result<bool> {
    match value {
        JsonValue::Boolean(value) => Ok(*value),
        _ => Err(Error::new(format!("{context} must be a boolean"))),
    }
}

fn expect_string_array(value: &JsonValue, context: &str) -> Result<Vec<String>> {
    let JsonValue::Array(values) = value else {
        return Err(Error::new(format!("{context} must be an array")));
    };
    values
        .iter()
        .map(|value| expect_string(value, context))
        .collect()
}

fn convert_json_error(path: &Path, error: neco_rs_json::Error) -> Error {
    let mut converted = Error::new(error.message).with_path(path.to_path_buf());
    if let Some(span) = error.span {
        converted = converted.with_span(Span {
            start: FilePos {
                r: 0,
                c: span.start,
            },
            end: FilePos { r: 0, c: span.end },
        });
    }
    converted
}

#[cfg(test)]
mod tests {
    use super::{DependencySource, Manifest, parse_manifest};
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
}
