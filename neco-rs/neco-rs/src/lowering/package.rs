use std::path::Path;

use neco_rs_parser::{Item, ParsedPackage, ParsedRoot, ParsedWorkspace, parse_root};

use crate::ir::LoweredProgram;
use crate::{Error, Result};

use super::declarations::{PureFunction, pure_function_from_decl};
use super::pure::lower_pure_block_value;
use super::{LoweringState, validate_value_against_type};

pub(super) fn initialize_zero_arg_use_bindings(
    package: &ParsedPackage,
    available_packages: &[ParsedPackage],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Use(use_decl) = item else {
            continue;
        };

        let Some(function) =
            resolve_zero_arg_imported_pure_function(package, available_packages, &use_decl.path)?
        else {
            continue;
        };

        let imported_name = use_decl
            .path
            .segments
            .last()
            .expect("use path always has at least one segment")
            .lexeme
            .clone();
        let value = lower_pure_block_value(&function.body, state, program)?;
        validate_value_against_type(&value, &function.result_ty, program)?;
        state.environment.insert(imported_name, value);
    }

    Ok(())
}

fn resolve_zero_arg_imported_pure_function(
    package: &ParsedPackage,
    available_packages: &[ParsedPackage],
    path: &neco_rs_parser::PathExpression,
) -> Result<Option<PureFunction>> {
    let Some(last_segment) = path.segments.last() else {
        return Ok(None);
    };
    let target_package = if path.token_keyword_package.is_some() || path.segments.len() == 1 {
        package
    } else {
        let package_name = &path.segments[0].lexeme;
        match available_packages
            .iter()
            .find(|candidate| candidate.manifest.name == *package_name)
        {
            Some(package) => package,
            None => {
                if package_has_zero_arg_pure_function(package, &last_segment.lexeme)? {
                    return Err(Error::Unsupported(format!(
                        "unknown package-qualified use `{package_name}`"
                    )));
                }
                return Ok(None);
            }
        }
    };

    for item in target_package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Function(function) = item else {
            continue;
        };
        if function.effect.is_some() {
            continue;
        }
        if function.name.name != last_segment.lexeme {
            continue;
        }

        let function = pure_function_from_decl(function)?;
        if function.parameters.is_empty() {
            return Ok(Some(function));
        }
        return Ok(None);
    }

    Ok(None)
}

fn package_has_zero_arg_pure_function(package: &ParsedPackage, name: &str) -> Result<bool> {
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Function(function) = item else {
            continue;
        };
        if function.effect.is_some() || function.name.name != name {
            continue;
        }
        return Ok(pure_function_from_decl(function)?.parameters.is_empty());
    }
    Ok(false)
}

pub(super) fn collect_callable_packages(package: &ParsedPackage) -> Result<Vec<ParsedPackage>> {
    let mut packages = resolve_workspace_dependency_packages(package)?;
    if let Some(std_core) = find_std_core_package(package)? {
        packages.push(std_core);
    }
    packages.push(package.clone());
    Ok(packages)
}

fn resolve_workspace_dependency_packages(package: &ParsedPackage) -> Result<Vec<ParsedPackage>> {
    if package.manifest.dependencies.is_empty() {
        return Ok(Vec::new());
    }

    let Some(workspace) = find_workspace_for_package(package)? else {
        return Err(Error::Unsupported(
            "`workspace: true` dependencies require a containing workspace".to_string(),
        ));
    };

    let mut packages = Vec::new();
    for dependency in &package.manifest.dependencies {
        let dependency_package = workspace
            .packages
            .iter()
            .find(|candidate| candidate.manifest.name == dependency.name)
            .cloned()
            .ok_or_else(|| {
                Error::Unsupported(format!(
                    "workspace dependency `{}` could not be resolved for lowering",
                    dependency.name
                ))
            })?;
        packages.push(dependency_package);
    }
    Ok(packages)
}

fn find_workspace_for_package(package: &ParsedPackage) -> Result<Option<ParsedWorkspace>> {
    let mut current = package.root_dir.parent();
    while let Some(candidate) = current {
        let manifest_path = candidate.join("neco-package.json");
        if manifest_path.exists()
            && let Ok(ParsedRoot::Workspace(workspace)) = parse_root(candidate)
            && workspace
                .packages
                .iter()
                .any(|member| paths_equal(&member.root_dir, &package.root_dir))
        {
            return Ok(Some(workspace));
        }
        current = candidate.parent();
    }
    Ok(None)
}

fn find_std_core_package(package: &ParsedPackage) -> Result<Option<ParsedPackage>> {
    for ancestor in package.root_dir.ancestors() {
        let candidate = ancestor.join("std/std_core");
        if candidate.join("neco-package.json").exists()
            && let Ok(ParsedRoot::Package(std_core)) = parse_root(&candidate)
        {
            return Ok(Some(std_core));
        }
    }
    Ok(None)
}

fn paths_equal(lhs: &Path, rhs: &Path) -> bool {
    lhs == rhs
}
