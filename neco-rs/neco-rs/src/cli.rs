use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use neco_rs_parser::{ParsedPackage, ParsedRoot};

use crate::{Error, Result};

pub(crate) fn select_package_for_build(parsed: ParsedRoot, output: &Path) -> Result<ParsedPackage> {
    match parsed {
        ParsedRoot::Package(package) => select_binary_from_package(package, output),
        ParsedRoot::Workspace(workspace) => {
            let binary_packages: Vec<_> = workspace
                .packages
                .into_iter()
                .filter(|package| !package.manifest.felis_bin_entrypoints.is_empty())
                .collect();

            match binary_packages.len() {
                0 => Err(Error::Unsupported(
                    "workspace root does not contain a binary package".to_string(),
                )),
                1 => {
                    select_binary_from_package(binary_packages.into_iter().next().unwrap(), output)
                }
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
            let source_files = package
                .source_files
                .into_iter()
                .filter(|file| {
                    file.role != neco_rs_parser::SourceFileRole::BinaryEntrypoint
                        || file.path == selected_path
                })
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
    pub(crate) output: PathBuf,
}

impl CliOptions {
    pub(crate) fn parse<I>(args: I) -> Result<Self>
    where
        I: IntoIterator<Item = String>,
    {
        let mut args = args.into_iter();
        let _program = args.next();

        let mut input = None;
        let mut output = None;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "build" => {}
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
        let output = output.unwrap_or_else(|| default_output_path(&input));

        Ok(Self { input, output })
    }
}

pub(crate) fn default_output_path(input: &Path) -> PathBuf {
    let package_root = if input
        .file_name()
        .is_some_and(|name| name == std::ffi::OsStr::new("neco-package.json"))
    {
        input.parent().unwrap_or(input)
    } else {
        input
    };

    let name = package_root
        .file_name()
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| std::ffi::OsStr::new("a.out"));

    package_root.join(".neco").join(name)
}
