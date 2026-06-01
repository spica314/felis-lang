use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use neco_rs_parser::{Item, ParsedPackage, ParsedRoot, ParsedSourceFile, ParsedWorkspace};

use crate::{Error, Result};

pub(crate) fn select_package_for_build(parsed: ParsedRoot, output: &Path) -> Result<ParsedPackage> {
    match parsed {
        ParsedRoot::Package(package) => select_binary_from_package(package, output),
        ParsedRoot::Workspace(workspace) => {
            let mut binary_packages = workspace_binary_packages(workspace);

            match binary_packages.len() {
                0 => Err(Error::Unsupported(
                    "workspace root does not contain a binary package".to_string(),
                )),
                1 => select_binary_from_package(binary_packages.remove(0), output),
                _ => {
                    let output_name = output_selection_name(output);
                    let matches = binary_packages
                        .into_iter()
                        .filter_map(|package| {
                            output_name_match_score(&output_name, &package.manifest.name)
                                .map(|score| (score, package))
                        })
                        .collect::<Vec<_>>();
                    if let Some(package) = select_unique_best_match(matches) {
                        return select_binary_from_package(package, output);
                    }

                    Err(Error::Unsupported(
                        "workspace root resolves to multiple binary packages; choose an output path that identifies the target package".to_string(),
                    ))
                }
            }
        }
    }
}

pub(crate) fn select_package_for_default_build(parsed: ParsedRoot) -> Result<ParsedPackage> {
    match parsed {
        ParsedRoot::Package(package) => select_default_binary_from_package(package),
        ParsedRoot::Workspace(workspace) => {
            let mut binary_packages = workspace_binary_packages(workspace);

            match binary_packages.len() {
                0 => Err(Error::Unsupported(
                    "workspace root does not contain a binary package".to_string(),
                )),
                1 => select_default_binary_from_package(binary_packages.remove(0)),
                _ => Err(Error::Unsupported(
                    "workspace root resolves to multiple binary packages; choose an output path that identifies the target package".to_string(),
                )),
            }
        }
    }
}

fn workspace_binary_packages(workspace: ParsedWorkspace) -> Vec<ParsedPackage> {
    workspace
        .packages
        .into_iter()
        .filter(|package| !package.manifest.felis_bin_entrypoints.is_empty())
        .collect()
}

pub(crate) fn select_binary_from_package(
    package: ParsedPackage,
    output: &Path,
) -> Result<ParsedPackage> {
    match package.manifest.felis_bin_entrypoints.len() {
        0 | 1 => Ok(package),
        _ => {
            let output_name = output_selection_name(output);
            let matches = package
                .manifest
                .felis_bin_entrypoints
                .iter()
                .filter_map(|path| {
                    let name = binary_name(path);
                    output_name_match_score(&output_name, &name).map(|score| (score, path.clone()))
                })
                .collect::<Vec<_>>();
            let Some(selected) = select_unique_best_match(matches) else {
                return Err(Error::Unsupported(
                    "package contains multiple binary entrypoints; choose an output path that identifies the target binary".to_string(),
                ));
            };
            let selected_path = package.root_dir.join(&selected);
            let reachable_paths =
                reachable_binary_source_paths(&package.source_files, &selected_path);
            let source_files = package
                .source_files
                .into_iter()
                .filter(|file| reachable_paths.contains(&file.path))
                .collect();
            let mut manifest = package.manifest;
            manifest.felis_bin_entrypoints = vec![selected];

            Ok(ParsedPackage {
                root_dir: package.root_dir,
                manifest_path: package.manifest_path,
                manifest,
                source_files,
            })
        }
    }
}

fn reachable_binary_source_paths(
    source_files: &[ParsedSourceFile],
    selected_binary_path: &Path,
) -> std::collections::HashSet<PathBuf> {
    let mut reachable = std::collections::HashSet::new();
    for file in source_files {
        if file.role == neco_rs_parser::SourceFileRole::LibraryEntrypoint
            || file.path == selected_binary_path
        {
            collect_reachable_source_paths(source_files, &file.path, &mut reachable);
        }
    }
    reachable
}

fn collect_reachable_source_paths(
    source_files: &[ParsedSourceFile],
    path: &Path,
    reachable: &mut std::collections::HashSet<PathBuf>,
) {
    if !reachable.insert(path.to_path_buf()) {
        return;
    }
    let Some(file) = source_files.iter().find(|file| file.path == path) else {
        return;
    };
    for module_name in file.syntax.items.iter().filter_map(|item| match item {
        Item::Mod(decl) => Some(decl.name.as_str()),
        _ => None,
    }) {
        let child_path = module_source_path(path, module_name);
        collect_reachable_source_paths(source_files, &child_path, reachable);
    }
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

fn select_default_binary_from_package(package: ParsedPackage) -> Result<ParsedPackage> {
    match package.manifest.felis_bin_entrypoints.len() {
        0 | 1 => Ok(package),
        _ => Err(Error::Unsupported(
            "package contains multiple binary entrypoints; choose an output path that identifies the target binary".to_string(),
        )),
    }
}

fn binary_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or_default()
        .to_string()
}

fn output_selection_name(output: &Path) -> String {
    output
        .file_name()
        .or_else(|| output.file_stem())
        .and_then(|name| name.to_str())
        .unwrap_or_default()
        .to_string()
}

fn output_name_match_score(output_name: &str, candidate: &str) -> Option<usize> {
    (output_name == candidate || output_name.contains(candidate)).then_some(candidate.len())
}

fn select_unique_best_match<T>(matches: Vec<(usize, T)>) -> Option<T> {
    let best_score = matches.iter().map(|(score, _)| *score).max()?;
    let mut best = matches
        .into_iter()
        .filter(|(score, _)| *score == best_score)
        .map(|(_, value)| value);
    let selected = best.next()?;
    if best.next().is_some() {
        None
    } else {
        Some(selected)
    }
}

pub(crate) fn set_output_executable(output: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        let mut permissions = fs::metadata(output)
            .map_err(|source| Error::Io {
                path: Some(output.to_path_buf()),
                source,
            })?
            .permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(output, permissions).map_err(|source| Error::Io {
            path: Some(output.to_path_buf()),
            source,
        })?;
    }

    Ok(())
}

pub(crate) struct CliOptions {
    pub(crate) input: PathBuf,
    pub(crate) output: Option<PathBuf>,
}

impl CliOptions {
    pub(crate) fn parse<I>(args: I) -> Result<Self>
    where
        I: IntoIterator<Item = String>,
    {
        let mut args = args.into_iter();
        let _program = args.next();

        let Some(subcommand) = args.next() else {
            return Err(Error::Usage(
                "usage: neco-rs build <package-root> [-o <output-elf>]".to_string(),
            ));
        };
        match subcommand.as_str() {
            "build" => {}
            "--help" | "-h" => {
                return Err(Error::Usage(
                    "usage: neco-rs build <package-root> [-o <output-elf>]".to_string(),
                ));
            }
            _ if subcommand.starts_with('-') => {
                return Err(Error::Usage(format!("unknown option `{subcommand}`")));
            }
            _ => {
                return Err(Error::Usage(format!(
                    "expected `build` subcommand, found `{subcommand}`"
                )));
            }
        }

        let mut input = None;
        let mut output = None;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-o" | "--output" => {
                    let Some(path) = args.next() else {
                        return Err(Error::Usage("missing value for --output".to_string()));
                    };
                    output = Some(PathBuf::from(path));
                }
                "--help" | "-h" => {
                    return Err(Error::Usage(
                        "usage: neco-rs build <package-root> [-o <output-elf>]".to_string(),
                    ));
                }
                _ if arg.starts_with('-') => {
                    return Err(Error::Usage(format!("unknown option `{arg}`")));
                }
                _ => {
                    if input.is_some() {
                        return Err(Error::Usage(
                            "expected a single package root path".to_string(),
                        ));
                    }
                    input = Some(PathBuf::from(arg));
                }
            }
        }

        let input = input.ok_or_else(|| Error::Usage("missing package root path".to_string()))?;
        Ok(Self { input, output })
    }
}

pub(crate) fn default_output_path(package: &ParsedPackage) -> PathBuf {
    let name = package
        .manifest
        .felis_bin_entrypoints
        .first()
        .and_then(|path| path.file_stem())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| std::ffi::OsStr::new("a.out"));

    package.root_dir.join(".neco").join(name)
}

#[cfg(test)]
mod tests {
    use super::CliOptions;

    fn parse(args: &[&str]) -> String {
        CliOptions::parse(args.iter().map(|arg| arg.to_string()))
            .map(|options| {
                format!(
                    "input={},output={}",
                    options.input.display(),
                    options
                        .output
                        .as_ref()
                        .map(|path| path.display().to_string())
                        .unwrap_or_default()
                )
            })
            .unwrap_or_else(|error| error.to_string())
    }

    #[test]
    fn parses_build_subcommand() {
        assert_eq!(parse(&["neco-rs", "build", "pkg"]), "input=pkg,output=");
        assert_eq!(
            parse(&["neco-rs", "build", "pkg", "-o", "out"]),
            "input=pkg,output=out"
        );
    }

    #[test]
    fn allows_build_as_package_root_name() {
        assert_eq!(parse(&["neco-rs", "build", "build"]), "input=build,output=");
    }

    #[test]
    fn rejects_missing_or_misplaced_build_subcommand() {
        assert!(parse(&["neco-rs"]).contains("usage: neco-rs build <package-root>"));
        assert!(parse(&["neco-rs", "pkg"]).contains("expected `build` subcommand"));
        assert!(parse(&["neco-rs", "pkg", "build"]).contains("expected `build` subcommand"));
    }

    #[test]
    fn rejects_extra_build_token_after_package_root() {
        assert!(
            parse(&["neco-rs", "build", "pkg", "build"])
                .contains("expected a single package root path")
        );
    }
}
