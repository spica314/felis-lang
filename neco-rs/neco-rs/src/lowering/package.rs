use std::path::Path;

use neco_rs_parser::{Item, ParsedPackage, ParsedRoot, ParsedWorkspace, parse_root};

use crate::ir::LoweredProgram;
use crate::{Error, Result};

use super::declarations::{PureFunction, pure_function_from_decl};
use super::pure::lower_pure_block_value;
use super::symbol::SymbolTable;
use super::{LoweringState, validate_value_against_type};

pub(super) fn initialize_zero_arg_use_bindings(
    package: &ParsedPackage,
    symbols: &SymbolTable<'_>,
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
            resolve_zero_arg_imported_pure_function(package, symbols, &use_decl.path)?
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
    symbols: &SymbolTable<'_>,
    path: &neco_rs_parser::PathExpression,
) -> Result<Option<PureFunction>> {
    let Some(last_segment) = path.segments.last() else {
        return Ok(None);
    };
    let target_package_name = if path.token_keyword_package.is_some() || path.segments.len() == 1 {
        package.manifest.name.as_str()
    } else {
        let package_name = &path.segments[0].lexeme;
        if symbols.package_exists(package_name) {
            package_name.as_str()
        } else {
            if package_has_zero_arg_pure_function(
                symbols,
                &package.manifest.name,
                &last_segment.lexeme,
            )? {
                return Err(Error::Unsupported(format!(
                    "unknown package-qualified use `{package_name}`"
                )));
            }
            return Ok(None);
        }
    };

    if let Some(function) =
        symbols.find_pure_function_in_package(target_package_name, &last_segment.lexeme)
    {
        let function = pure_function_from_decl(function)?;
        if function.parameters.is_empty() {
            return Ok(Some(function));
        }
        return Ok(None);
    }

    Ok(None)
}

fn package_has_zero_arg_pure_function(
    symbols: &SymbolTable<'_>,
    package_name: &str,
    name: &str,
) -> Result<bool> {
    if let Some(function) = symbols.find_pure_function_in_package(package_name, name) {
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
