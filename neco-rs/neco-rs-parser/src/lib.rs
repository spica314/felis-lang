mod lexer;
mod parser;
mod syntax;

use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use neco_rs_json::{JsonEntry, JsonValue};

pub use lexer::{Keyword, Span, Token, TokenKind};
pub use parser::{Parse, Parser};
pub use syntax::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub path: Option<PathBuf>,
    pub span: Option<Span>,
    pub message: String,
}

impl Error {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self {
            path: None,
            span: None,
            message: message.into(),
        }
    }

    pub(crate) fn with_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub(crate) fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.path, self.span) {
            (Some(path), Some(span)) => write!(
                f,
                "{}:{}..{}: {}",
                path.display(),
                span.start,
                span.end,
                self.message
            ),
            (Some(path), None) => write!(f, "{}: {}", path.display(), self.message),
            (None, Some(span)) => write!(f, "{}..{}: {}", span.start, span.end, self.message),
            (None, None) => write!(f, "{}", self.message),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedRoot {
    Package(ParsedPackage),
    Workspace(ParsedWorkspace),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedWorkspace {
    pub root_dir: PathBuf,
    pub manifest_path: PathBuf,
    pub manifest: WorkspaceManifest,
    pub packages: Vec<ParsedPackage>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedPackage {
    pub root_dir: PathBuf,
    pub manifest_path: PathBuf,
    pub manifest: PackageManifest,
    pub source_files: Vec<ParsedSourceFile>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedSourceFile {
    pub path: PathBuf,
    pub role: SourceFileRole,
    pub tokens: Vec<Token>,
    pub syntax: SourceFile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceFileRole {
    LibraryEntrypoint,
    BinaryEntrypoint,
    Module,
}

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

pub fn parse_root(path: impl AsRef<Path>) -> Result<ParsedRoot> {
    let path = path.as_ref();
    let manifest_path = manifest_path_from_root(path)?;
    let raw = fs::read_to_string(&manifest_path)
        .map_err(|error| Error::new(error.to_string()).with_path(manifest_path.clone()))?;
    let manifest = parse_manifest(&manifest_path, &raw)?;

    match manifest {
        Manifest::Package(manifest) => {
            let root_dir = manifest_path
                .parent()
                .ok_or_else(|| Error::new("package manifest has no parent directory"))?
                .to_path_buf();
            let package = parse_package(root_dir, manifest_path, manifest)?;
            Ok(ParsedRoot::Package(package))
        }
        Manifest::Workspace(manifest) => {
            let root_dir = manifest_path
                .parent()
                .ok_or_else(|| Error::new("workspace manifest has no parent directory"))?
                .to_path_buf();
            let mut packages = Vec::new();
            for member in &manifest.members {
                let member_root = root_dir.join(member);
                let member_manifest = member_root.join("neco-package.json");
                let raw = fs::read_to_string(&member_manifest).map_err(|error| {
                    Error::new(error.to_string()).with_path(member_manifest.clone())
                })?;
                let member_manifest_data = match parse_manifest(&member_manifest, &raw)? {
                    Manifest::Package(package) => package,
                    Manifest::Workspace(_) => {
                        return Err(Error::new("workspace members must be packages")
                            .with_path(member_manifest));
                    }
                };
                packages.push(parse_package(
                    member_root,
                    member_manifest,
                    member_manifest_data,
                )?);
            }
            Ok(ParsedRoot::Workspace(ParsedWorkspace {
                root_dir,
                manifest_path,
                manifest,
                packages,
            }))
        }
    }
}

pub fn parse_source(source: &str) -> Result<(Vec<Token>, SourceFile)> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.lex()?;
    let mut parser = Parser::new(tokens.clone());
    let syntax = SourceFile::parse(&mut parser)?;
    Ok((tokens, syntax))
}

fn parse_package(
    root_dir: PathBuf,
    manifest_path: PathBuf,
    manifest: PackageManifest,
) -> Result<ParsedPackage> {
    let mut source_files = Vec::new();
    let mut visited = HashSet::new();

    if let Some(path) = &manifest.felis_lib_entrypoint {
        let entrypoint = root_dir.join(path);
        collect_source_file(
            &manifest_path,
            &entrypoint,
            SourceFileRole::LibraryEntrypoint,
            &mut visited,
            &mut source_files,
        )?;
    }

    for path in &manifest.felis_bin_entrypoints {
        let entrypoint = root_dir.join(path);
        collect_source_file(
            &manifest_path,
            &entrypoint,
            SourceFileRole::BinaryEntrypoint,
            &mut visited,
            &mut source_files,
        )?;
    }

    Ok(ParsedPackage {
        root_dir,
        manifest_path,
        manifest,
        source_files,
    })
}

fn collect_source_file(
    manifest_path: &Path,
    file_path: &Path,
    role: SourceFileRole,
    visited: &mut HashSet<PathBuf>,
    source_files: &mut Vec<ParsedSourceFile>,
) -> Result<()> {
    let canonical = file_path.to_path_buf();
    if !visited.insert(canonical.clone()) {
        return Ok(());
    }

    let source = fs::read_to_string(&canonical)
        .map_err(|error| Error::new(error.to_string()).with_path(canonical.clone()))?;
    let (tokens, syntax) = parse_source(&source).map_err(|error| Error {
        path: Some(canonical.clone()),
        span: error.span,
        message: error.message,
    })?;

    let module_names: Vec<String> = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Mod(decl) => Some(decl.name.clone()),
            _ => None,
        })
        .collect();

    source_files.push(ParsedSourceFile {
        path: canonical.clone(),
        role,
        tokens,
        syntax,
    });

    for module_name in module_names {
        let child_path = module_source_path(&canonical, &module_name);
        if !child_path.exists() {
            return Err(Error::new(format!(
                "module `{}` declared in `{}` does not resolve to `{}`",
                module_name,
                canonical.display(),
                child_path.display()
            ))
            .with_path(manifest_path.to_path_buf()));
        }
        collect_source_file(
            manifest_path,
            &child_path,
            SourceFileRole::Module,
            visited,
            source_files,
        )?;
    }

    Ok(())
}

fn module_source_path(current_file: &Path, module_name: &str) -> PathBuf {
    let parent = current_file.parent().unwrap_or_else(|| Path::new("."));
    let stem = current_file
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("");
    let module_dir = match stem {
        "lib" | "main" => parent.to_path_buf(),
        _ => parent.join(stem),
    };
    module_dir.join(format!("{module_name}.fe"))
}

fn manifest_path_from_root(path: &Path) -> Result<PathBuf> {
    if path.is_dir() {
        Ok(path.join("neco-package.json"))
    } else if path
        .file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name == "neco-package.json")
    {
        Ok(path.to_path_buf())
    } else {
        Err(Error::new(
            "expected a workspace root, package root, or neco-package.json path",
        ))
    }
}

enum Manifest {
    Package(PackageManifest),
    Workspace(WorkspaceManifest),
}

fn parse_manifest(path: &Path, raw: &str) -> Result<Manifest> {
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
            let workspace = optional_bool_field(dependency, "workspace")?.unwrap_or(false);
            Ok(Dependency {
                name: entry.key.clone(),
                source: if workspace {
                    DependencySource::Workspace
                } else {
                    DependencySource::Workspace
                },
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
            start: span.start,
            end: span.end,
        });
    }
    converted
}

#[cfg(test)]
mod tests {
    use super::{Manifest, parse_manifest};
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
