use std::collections::HashMap;

use neco_rs_parser::{
    ArrowParameter, Block, ConstructorDeclaration, DeclaredName, FunctionDeclaration, Item,
    ParsedPackage, StructDeclaration, Term,
};

use crate::{Error, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct PureFunction {
    pub(super) parameters: Vec<PureFunctionParameter>,
    pub(super) result_ty: Term,
    pub(super) body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct PureFunctionParameter {
    pub(super) name: String,
    pub(super) ty: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct StatementFunction {
    pub(super) parameters: Vec<StatementFunctionParameter>,
    pub(super) result_ty: Term,
    pub(super) effect: Option<String>,
    pub(super) body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct StatementFunctionParameter {
    pub(super) name: String,
    pub(super) ty: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ConstructorSignature {
    pub(super) type_name: String,
    pub(super) constructor_name: String,
    pub(super) parameters: Vec<ConstructorParameter>,
    pub(super) is_rc: bool,
    pub(super) tag: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ConstructorParameter {
    pub(super) name: Option<String>,
    pub(super) ty: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct StructSignature {
    pub(super) type_name: String,
    pub(super) is_rc: bool,
    pub(super) fields: Vec<StructFieldSignature>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct StructFieldSignature {
    pub(super) name: String,
    pub(super) ty: Term,
}

pub(super) fn collect_pure_functions(
    packages: &[ParsedPackage],
) -> Result<HashMap<String, PureFunction>> {
    let mut functions = HashMap::new();
    for package in packages {
        for item in package
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
            functions.insert(
                function.name.name.clone(),
                pure_function_from_decl(function)?,
            );
        }
    }
    Ok(functions)
}

pub(super) fn collect_statement_functions(
    packages: &[ParsedPackage],
) -> Result<HashMap<String, StatementFunction>> {
    let mut functions = HashMap::new();
    for package in packages {
        for item in package
            .source_files
            .iter()
            .flat_map(|file| file.syntax.items.iter())
        {
            let Item::Function(function) = item else {
                continue;
            };
            functions.insert(
                function.name.name.clone(),
                statement_function_from_decl(function)?,
            );
        }
    }
    Ok(functions)
}

pub(super) fn collect_constructors(
    packages: &[ParsedPackage],
) -> Result<HashMap<String, ConstructorSignature>> {
    let mut constructors = HashMap::new();
    for package in packages {
        for item in package
            .source_files
            .iter()
            .flat_map(|file| file.syntax.items.iter())
        {
            let Item::Type(type_decl) = item else {
                continue;
            };

            for (tag, constructor) in type_decl.constructors.iter().enumerate() {
                let Some(parameters) = constructor_parameters(constructor, &type_decl.name) else {
                    continue;
                };

                let key = constructor_key(&type_decl.name.name, &constructor.name.name);
                let value = ConstructorSignature {
                    type_name: type_decl.name.name.clone(),
                    constructor_name: constructor.name.name.clone(),
                    parameters,
                    is_rc: type_decl.modifier.as_deref() == Some("rc"),
                    tag: tag as i32,
                };
                if constructors.insert(key.clone(), value).is_some() {
                    return Err(Error::Unsupported(format!(
                        "duplicate constructor `{key}` is not supported"
                    )));
                }
            }
        }
    }
    Ok(constructors)
}

pub(super) fn collect_structs(
    packages: &[ParsedPackage],
) -> Result<HashMap<String, StructSignature>> {
    let mut structs = HashMap::new();
    for package in packages {
        for item in package
            .source_files
            .iter()
            .flat_map(|file| file.syntax.items.iter())
        {
            let Item::Struct(struct_decl) = item else {
                continue;
            };
            let signature = struct_signature_from_decl(struct_decl);
            if structs
                .insert(signature.type_name.clone(), signature)
                .is_some()
            {
                return Err(Error::Unsupported(format!(
                    "duplicate struct `{}` is not supported",
                    struct_decl.name.name
                )));
            }
        }
    }
    Ok(structs)
}

fn struct_signature_from_decl(struct_decl: &StructDeclaration) -> StructSignature {
    StructSignature {
        type_name: struct_decl.name.name.clone(),
        is_rc: struct_decl.modifier.as_deref() == Some("rc"),
        fields: struct_decl
            .fields
            .iter()
            .map(|field| StructFieldSignature {
                name: field.token_ident.lexeme.clone(),
                ty: field.ty.clone(),
            })
            .collect(),
    }
}

fn constructor_parameters(
    constructor: &ConstructorDeclaration,
    type_name: &DeclaredName,
) -> Option<Vec<ConstructorParameter>> {
    let mut parameters = Vec::new();
    let mut current = &constructor.ty;
    while let Term::Arrow(arrow) = current {
        let (name, ty) = match &arrow.parameter {
            ArrowParameter::Binder(binder) => {
                (Some(binder.name.clone()), binder.ty.as_ref().clone())
            }
            ArrowParameter::Domain(domain) => (None, domain.as_ref().clone()),
        };
        parameters.push(ConstructorParameter { name, ty });
        current = arrow.result.as_ref();
    }

    let path = match current {
        Term::Path(path) => path,
        Term::Application { callee, .. } => {
            let Term::Path(path) = callee.as_ref() else {
                return None;
            };
            path
        }
        _ => return None,
    };
    if path.token_keyword_package.is_none()
        && path.segments.len() == 1
        && path.segments[0].lexeme == type_name.name
    {
        Some(parameters)
    } else {
        None
    }
}

pub(super) fn constructor_key(type_name: &str, constructor_name: &str) -> String {
    format!("{type_name}::{constructor_name}")
}

pub(super) fn pure_function_from_decl(function: &FunctionDeclaration) -> Result<PureFunction> {
    let mut parameters = Vec::new();
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        let ArrowParameter::Binder(binder) = &arrow.parameter else {
            return Err(Error::Unsupported(format!(
                "pure function `{}` must use named parameters",
                function.name.name
            )));
        };
        parameters.push(PureFunctionParameter {
            name: binder.name.clone(),
            ty: binder.ty.as_ref().clone(),
        });
        current = arrow.result.as_ref();
    }
    Ok(PureFunction {
        parameters,
        result_ty: current.clone(),
        body: function.body.clone(),
    })
}

fn statement_function_from_decl(function: &FunctionDeclaration) -> Result<StatementFunction> {
    let mut parameters = Vec::new();
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        let ArrowParameter::Binder(binder) = &arrow.parameter else {
            return Err(Error::Unsupported(format!(
                "function `{}` must use named parameters",
                function.name.name
            )));
        };
        parameters.push(StatementFunctionParameter {
            name: binder.name.clone(),
            ty: binder.ty.as_ref().clone(),
        });
        current = arrow.result.as_ref();
    }
    if function.body.tail.is_none() {
        return Err(Error::Unsupported(format!(
            "function `{}` body must end with a value expression",
            function.name.name
        )));
    }
    Ok(StatementFunction {
        parameters,
        result_ty: current.clone(),
        effect: function.effect.as_ref().map(|effect| effect.lexeme.clone()),
        body: function.body.clone(),
    })
}
