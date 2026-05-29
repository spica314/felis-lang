use std::path::{Component, Path, PathBuf};

use neco_rs_json::{JsonEntry, JsonValue};

use crate::{Error, FilePos, Result, Span};

use super::model::{
    Dependency, DependencySource, Manifest, NativeLinkMode, PackageManifest, WorkspaceManifest,
};

pub(crate) fn parse_manifest(path: &Path, raw: &str) -> Result<Manifest> {
    let (_, value) =
        neco_rs_json::parse(raw).map_err(|error| convert_json_error(path, raw, error))?;
    let object = expect_object(path, &value, "manifest")?;

    let workspace = optional_field(object, "workspace")?;
    let name = optional_string_field(object, "name")?;
    let dependencies = optional_field(object, "dependencies")?;
    let felis_lib_entrypoint = optional_string_field(object, "felis-lib-entrypoint")?;
    let felis_bin_entrypoints = optional_string_array_field(object, "felis-bin-entrypoints")?;
    let native_link_mode = optional_string_field(object, "native-link-mode")?;
    let native_libraries = optional_string_array_field(object, "native-libraries")?;

    match (workspace, name) {
        (Some(workspace), None) => {
            if felis_lib_entrypoint.is_some()
                || !felis_bin_entrypoints.is_empty()
                || dependencies.is_some()
                || native_link_mode.is_some()
                || !native_libraries.is_empty()
            {
                return Err(
                    Error::new("workspace manifest cannot contain package-only fields")
                        .with_path(path.to_path_buf()),
                );
            }

            let workspace = expect_object(path, workspace, "`workspace`")?;
            let member_paths = required_string_array_field(path, workspace, "members")?;
            if member_paths.is_empty() {
                return Err(
                    Error::new("`workspace.members` must contain at least one path")
                        .with_path(path.to_path_buf()),
                );
            }
            let members = member_paths
                .into_iter()
                .map(|member| parse_manifest_path(path, &member, "workspace member"))
                .collect::<Result<Vec<_>>>()?;

            Ok(Manifest::Workspace(WorkspaceManifest { members }))
        }
        (None, Some(name)) => Ok(Manifest::Package(PackageManifest {
            name,
            dependencies: parse_dependencies(path, dependencies)?,
            felis_lib_entrypoint: felis_lib_entrypoint
                .map(|entrypoint| parse_manifest_path(path, &entrypoint, "library entrypoint"))
                .transpose()?,
            felis_bin_entrypoints: felis_bin_entrypoints
                .into_iter()
                .map(|entrypoint| parse_manifest_path(path, &entrypoint, "binary entrypoint"))
                .collect::<Result<Vec<_>>>()?,
            native_link_mode: parse_native_link_mode(path, native_link_mode)?,
            native_libraries: parse_native_libraries(path, native_libraries)?,
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

fn parse_native_link_mode(path: &Path, value: Option<String>) -> Result<NativeLinkMode> {
    match value.as_deref() {
        None | Some("kernel-start") => Ok(NativeLinkMode::KernelStart),
        Some("libc-start") => Ok(NativeLinkMode::LibcStart),
        Some(value) => Err(Error::new(format!(
            "`native-link-mode` must be `kernel-start` or `libc-start`, found `{value}`"
        ))
        .with_path(path.to_path_buf())),
    }
}

fn parse_native_libraries(path: &Path, libraries: Vec<String>) -> Result<Vec<String>> {
    for library in &libraries {
        if library.is_empty()
            || library.starts_with('-')
            || library.contains('/')
            || library.contains('\\')
            || library.contains(char::is_whitespace)
        {
            return Err(Error::new(format!(
                "native library `{library}` must be a bare linker library name"
            ))
            .with_path(path.to_path_buf()));
        }
    }
    Ok(libraries)
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

fn parse_manifest_path(path: &Path, raw_path: &str, context: &str) -> Result<PathBuf> {
    if raw_path.is_empty() {
        return Err(
            Error::new(format!("{context} path must not be empty")).with_path(path.to_path_buf())
        );
    }
    let parsed = PathBuf::from(raw_path);
    let escapes_root = parsed.components().any(|component| {
        matches!(
            component,
            Component::Prefix(_) | Component::RootDir | Component::ParentDir
        )
    });
    if escapes_root {
        return Err(Error::new(format!(
            "{context} path `{raw_path}` must stay within the manifest root"
        ))
        .with_path(path.to_path_buf()));
    }
    Ok(parsed)
}

fn convert_json_error(path: &Path, raw: &str, error: neco_rs_json::Error) -> Error {
    let mut converted = Error::new(error.message).with_path(path.to_path_buf());
    if let Some(span) = error.span {
        converted = converted.with_span(Span {
            start: offset_to_file_pos(raw, span.start),
            end: offset_to_file_pos(raw, span.end),
        });
    }
    converted
}

fn offset_to_file_pos(raw: &str, offset: usize) -> FilePos {
    let mut r = 0;
    let mut c = 0;
    for ch in raw.chars().take(offset) {
        if ch == '\n' {
            r += 1;
            c = 0;
        } else {
            c += 1;
        }
    }
    FilePos { r, c }
}
