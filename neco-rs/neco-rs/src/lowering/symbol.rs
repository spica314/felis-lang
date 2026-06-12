use std::path::Path;

use neco_rs_parser::{
    BindBuiltinDeclaration, FunctionDeclaration, Item, ParsedPackage, ParsedSourceFile,
    SourceFileRole, StructDeclaration, TypeDeclaration,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum DeclarationKind {
    BindBuiltin,
    Function,
    Struct,
    Type,
}

#[derive(Debug, Clone, Copy)]
pub(super) enum DeclarationRef<'a> {
    BindBuiltin(&'a BindBuiltinDeclaration),
    Function(&'a FunctionDeclaration),
    Struct(&'a StructDeclaration),
    Type(&'a TypeDeclaration),
}

#[derive(Debug)]
pub(super) struct SymbolEntry<'a> {
    package_name: &'a str,
    module_path: Vec<String>,
    source_role: SourceFileRole,
    kind: DeclarationKind,
    declaration: DeclarationRef<'a>,
}

#[derive(Debug)]
pub(super) struct SymbolTable<'a> {
    package_names: Vec<&'a str>,
    entries: Vec<SymbolEntry<'a>>,
}

impl<'a> SymbolTable<'a> {
    pub(super) fn build(packages: &'a [ParsedPackage]) -> Self {
        let package_names = packages
            .iter()
            .map(|package| package.manifest.name.as_str())
            .collect();
        let mut entries = Vec::new();
        for package in packages {
            for source_file in &package.source_files {
                let module_path = module_path(package, source_file);
                for item in &source_file.syntax.items {
                    let Some((kind, declaration)) = declaration_from_item(item) else {
                        continue;
                    };
                    entries.push(SymbolEntry {
                        package_name: &package.manifest.name,
                        module_path: module_path.clone(),
                        source_role: source_file.role.clone(),
                        kind,
                        declaration,
                    });
                }
            }
        }
        validate_entry_metadata(&entries);
        Self {
            package_names,
            entries,
        }
    }

    pub(super) fn bind_builtin_declarations(
        &self,
    ) -> impl Iterator<Item = &'a BindBuiltinDeclaration> + '_ {
        self.entries
            .iter()
            .filter_map(|entry| match entry.declaration {
                DeclarationRef::BindBuiltin(declaration) => Some(declaration),
                _ => None,
            })
    }

    pub(super) fn function_declarations(
        &self,
    ) -> impl Iterator<Item = &'a FunctionDeclaration> + '_ {
        self.entries
            .iter()
            .filter_map(|entry| match entry.declaration {
                DeclarationRef::Function(declaration) => Some(declaration),
                _ => None,
            })
    }

    pub(super) fn struct_declarations(&self) -> impl Iterator<Item = &'a StructDeclaration> + '_ {
        self.entries
            .iter()
            .filter_map(|entry| match entry.declaration {
                DeclarationRef::Struct(declaration) => Some(declaration),
                _ => None,
            })
    }

    pub(super) fn type_declarations(&self) -> impl Iterator<Item = &'a TypeDeclaration> + '_ {
        self.entries
            .iter()
            .filter_map(|entry| match entry.declaration {
                DeclarationRef::Type(declaration) => Some(declaration),
                _ => None,
            })
    }

    pub(super) fn package_exists(&self, package_name: &str) -> bool {
        self.package_names.contains(&package_name)
    }

    pub(super) fn find_function_in_package(
        &self,
        package_name: &str,
        function_name: &str,
    ) -> Option<&'a FunctionDeclaration> {
        self.entries.iter().find_map(|entry| {
            let DeclarationRef::Function(function) = entry.declaration else {
                return None;
            };
            if entry.package_name == package_name && function.name.name == function_name {
                Some(function)
            } else {
                None
            }
        })
    }

    pub(super) fn find_pure_function_in_package(
        &self,
        package_name: &str,
        function_name: &str,
    ) -> Option<&'a FunctionDeclaration> {
        self.entries.iter().find_map(|entry| {
            let DeclarationRef::Function(function) = entry.declaration else {
                return None;
            };
            if entry.package_name == package_name
                && function.effect.is_none()
                && function.name.name == function_name
            {
                Some(function)
            } else {
                None
            }
        })
    }
}

fn validate_entry_metadata(entries: &[SymbolEntry<'_>]) {
    for entry in entries {
        let _ = (entry.module_path.as_slice(), &entry.source_role, entry.kind);
    }
}

fn declaration_from_item(item: &Item) -> Option<(DeclarationKind, DeclarationRef<'_>)> {
    match item {
        Item::BindBuiltin(declaration) => Some((
            DeclarationKind::BindBuiltin,
            DeclarationRef::BindBuiltin(declaration),
        )),
        Item::Function(declaration) => Some((
            DeclarationKind::Function,
            DeclarationRef::Function(declaration),
        )),
        Item::Struct(declaration) => {
            Some((DeclarationKind::Struct, DeclarationRef::Struct(declaration)))
        }
        Item::Type(declaration) => Some((DeclarationKind::Type, DeclarationRef::Type(declaration))),
        _ => None,
    }
}

fn module_path(package: &ParsedPackage, source_file: &ParsedSourceFile) -> Vec<String> {
    if !matches!(source_file.role, SourceFileRole::Module) {
        return Vec::new();
    }

    let relative_path = source_file
        .path
        .strip_prefix(&package.root_dir)
        .unwrap_or(source_file.path.as_path());
    let relative_source_path = relative_path
        .strip_prefix(Path::new("src"))
        .unwrap_or(relative_path);
    relative_source_path
        .with_extension("")
        .components()
        .filter_map(|component| component.as_os_str().to_str().map(str::to_string))
        .collect()
}
