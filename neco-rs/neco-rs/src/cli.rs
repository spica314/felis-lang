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

pub(crate) fn select_package_for_run(
    parsed: ParsedRoot,
    binary_name: Option<&str>,
) -> Result<ParsedPackage> {
    match binary_name {
        Some(binary_name) => select_package_for_named_binary(parsed, binary_name),
        None => select_package_for_default_build(parsed),
    }
}

pub(crate) fn select_packages_for_test(parsed: ParsedRoot) -> Vec<ParsedPackage> {
    match parsed {
        ParsedRoot::Package(package) => select_test_entrypoints_from_package(package),
        ParsedRoot::Workspace(workspace) => workspace
            .packages
            .into_iter()
            .flat_map(select_test_entrypoints_from_package)
            .collect(),
    }
}

fn select_test_entrypoints_from_package(package: ParsedPackage) -> Vec<ParsedPackage> {
    package
        .manifest
        .felis_test_entrypoints
        .clone()
        .into_iter()
        .filter_map(|selected| select_binary_entrypoint(package.clone(), selected).ok())
        .collect()
}

fn select_package_for_named_binary(parsed: ParsedRoot, binary_name: &str) -> Result<ParsedPackage> {
    match parsed {
        ParsedRoot::Package(package) => select_named_binary_from_package(package, binary_name),
        ParsedRoot::Workspace(workspace) => {
            let matches = workspace_binary_packages(workspace)
                .into_iter()
                .filter_map(|package| select_named_binary_from_package(package, binary_name).ok())
                .collect::<Vec<_>>();

            match matches.len() {
                0 => Err(Error::Unsupported(format!(
                    "workspace does not contain binary `{binary_name}`"
                ))),
                1 => Ok(matches.into_iter().next().expect("one match")),
                _ => Err(Error::Unsupported(format!(
                    "workspace contains multiple binaries named `{binary_name}`"
                ))),
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

fn select_named_binary_from_package(
    package: ParsedPackage,
    binary_name: &str,
) -> Result<ParsedPackage> {
    let selected = package
        .manifest
        .felis_bin_entrypoints
        .iter()
        .find(|path| binary_name_from_path(path) == binary_name)
        .cloned()
        .ok_or_else(|| {
            Error::Unsupported(format!("package does not contain binary `{binary_name}`"))
        })?;

    select_binary_entrypoint(package, selected)
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
                    let name = binary_name_from_path(path);
                    output_name_match_score(&output_name, &name).map(|score| (score, path.clone()))
                })
                .collect::<Vec<_>>();
            let Some(selected) = select_unique_best_match(matches) else {
                return Err(Error::Unsupported(
                    "package contains multiple binary entrypoints; choose an output path that identifies the target binary".to_string(),
                ));
            };
            select_binary_entrypoint(package, selected)
        }
    }
}

fn select_binary_entrypoint(package: ParsedPackage, selected: PathBuf) -> Result<ParsedPackage> {
    let selected_path = package.root_dir.join(&selected);
    let reachable_paths = reachable_binary_source_paths(&package.source_files, &selected_path);
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

fn binary_name_from_path(path: &Path) -> String {
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
    pub(crate) command: CliCommand,
    pub(crate) input: PathBuf,
    pub(crate) output: Option<PathBuf>,
    pub(crate) binary_name: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CliCommand {
    Build,
    Run,
    Test,
}

impl CliOptions {
    pub(crate) fn parse<I>(args: I) -> Result<Self>
    where
        I: IntoIterator<Item = String>,
    {
        let mut args = args.into_iter();
        let _program = args.next();

        let Some(subcommand) = args.next() else {
            return Err(Error::Usage(usage().to_string()));
        };
        let command = match subcommand.as_str() {
            "build" => CliCommand::Build,
            "run" => CliCommand::Run,
            "test" => CliCommand::Test,
            "--help" | "-h" => {
                return Err(Error::Usage(usage().to_string()));
            }
            _ if subcommand.starts_with('-') => {
                return Err(Error::Usage(format!("unknown option `{subcommand}`")));
            }
            _ => {
                return Err(Error::Usage(format!(
                    "expected `build`, `run`, or `test` subcommand, found `{subcommand}`"
                )));
            }
        };

        let mut input = None;
        let mut output = None;
        let mut binary_name = None;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-o" | "--output" => {
                    if command != CliCommand::Build {
                        return Err(Error::Usage(
                            "--output is only supported by `build`".to_string(),
                        ));
                    }
                    let Some(path) = args.next() else {
                        return Err(Error::Usage("missing value for --output".to_string()));
                    };
                    output = Some(PathBuf::from(path));
                }
                "--bin" => {
                    if command != CliCommand::Run {
                        return Err(Error::Usage("--bin is only supported by `run`".to_string()));
                    }
                    let Some(name) = args.next() else {
                        return Err(Error::Usage("missing value for --bin".to_string()));
                    };
                    if binary_name.replace(name).is_some() {
                        return Err(Error::Usage("expected a single --bin option".to_string()));
                    }
                }
                "--help" | "-h" => {
                    return Err(Error::Usage(usage().to_string()));
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
        Ok(Self {
            command,
            input,
            output,
            binary_name,
        })
    }
}

fn usage() -> &'static str {
    "usage: neco-rs build <package-root> [-o <output-elf>]\n       neco-rs run <package-root> [--bin <binary-name>]\n       neco-rs test <package-root>"
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

pub(crate) fn test_output_path(package: &ParsedPackage) -> PathBuf {
    let name = package
        .manifest
        .felis_bin_entrypoints
        .first()
        .and_then(|path| path.file_stem())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| std::ffi::OsStr::new("test"));

    package.root_dir.join(".neco").join("test").join(name)
}

#[cfg(test)]
mod tests {
    use super::{CliCommand, CliOptions};

    fn parse(args: &[&str]) -> String {
        CliOptions::parse(args.iter().map(|arg| arg.to_string()))
            .map(|options| {
                let command = match options.command {
                    CliCommand::Build => "build",
                    CliCommand::Run => "run",
                    CliCommand::Test => "test",
                };
                format!(
                    "command={},input={},output={},bin={}",
                    command,
                    options.input.display(),
                    options
                        .output
                        .as_ref()
                        .map(|path| path.display().to_string())
                        .unwrap_or_default(),
                    options.binary_name.as_deref().unwrap_or_default()
                )
            })
            .unwrap_or_else(|error| error.to_string())
    }

    #[test]
    fn parses_build_subcommand() {
        assert_eq!(
            parse(&["neco-rs", "build", "pkg"]),
            "command=build,input=pkg,output=,bin="
        );
        assert_eq!(
            parse(&["neco-rs", "build", "pkg", "-o", "out"]),
            "command=build,input=pkg,output=out,bin="
        );
    }

    #[test]
    fn parses_run_subcommand() {
        assert_eq!(
            parse(&["neco-rs", "run", "pkg"]),
            "command=run,input=pkg,output=,bin="
        );
        assert_eq!(
            parse(&["neco-rs", "run", "pkg", "--bin", "tool"]),
            "command=run,input=pkg,output=,bin=tool"
        );
    }

    #[test]
    fn parses_test_subcommand() {
        assert_eq!(
            parse(&["neco-rs", "test", "pkg"]),
            "command=test,input=pkg,output=,bin="
        );
    }

    #[test]
    fn allows_build_as_package_root_name() {
        assert_eq!(
            parse(&["neco-rs", "build", "build"]),
            "command=build,input=build,output=,bin="
        );
    }

    #[test]
    fn rejects_missing_or_misplaced_build_subcommand() {
        assert!(parse(&["neco-rs"]).contains("usage: neco-rs build <package-root>"));
        assert!(
            parse(&["neco-rs", "pkg"]).contains("expected `build`, `run`, or `test` subcommand")
        );
        assert!(
            parse(&["neco-rs", "pkg", "build"])
                .contains("expected `build`, `run`, or `test` subcommand")
        );
    }

    #[test]
    fn rejects_extra_build_token_after_package_root() {
        assert!(
            parse(&["neco-rs", "build", "pkg", "build"])
                .contains("expected a single package root path")
        );
    }

    #[test]
    fn rejects_subcommand_specific_options() {
        assert!(
            parse(&["neco-rs", "build", "pkg", "--bin", "tool"])
                .contains("--bin is only supported by `run`")
        );
        assert!(
            parse(&["neco-rs", "run", "pkg", "-o", "out"])
                .contains("--output is only supported by `build`")
        );
    }
}
