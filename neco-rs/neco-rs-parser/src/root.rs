use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use crate::{
    Error, Item, PackageManifest, Result, SourceFile, Token, WorkspaceManifest,
    manifest::{Manifest, parse_manifest},
    source::parse_source,
};

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
    let syntax = syntax.unwrap();

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
