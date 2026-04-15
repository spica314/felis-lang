use std::collections::HashMap;

use neco_rs_parser::{
    ArrowParameter, Block, ConstructorDeclaration, DeclaredName, FunctionDeclaration, FunctionKind,
    Item, ParsedPackage, Term,
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
pub(super) struct Procedure {
    pub(super) parameters: Vec<ProcedureParameter>,
    pub(super) body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ProcedureParameter {
    pub(super) name: String,
    pub(super) ty: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct ConstructorSignature {
    pub(super) type_name: String,
    pub(super) constructor_name: String,
    pub(super) arity: usize,
    pub(super) is_rc: bool,
    pub(super) tag: i32,
}

pub(super) fn collect_pure_functions(
    package: &ParsedPackage,
) -> Result<HashMap<String, PureFunction>> {
    let mut functions = HashMap::new();
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Function(function) = item else {
            continue;
        };
        if function.kind != FunctionKind::Fn || function.effect.is_some() {
            continue;
        }
        functions.insert(
            function.name.name.clone(),
            pure_function_from_decl(function)?,
        );
    }
    Ok(functions)
}

pub(super) fn collect_procedures(packages: &[ParsedPackage]) -> Result<HashMap<String, Procedure>> {
    let mut procedures = HashMap::new();
    for package in packages {
        for item in package
            .source_files
            .iter()
            .flat_map(|file| file.syntax.items.iter())
        {
            let Item::Function(function) = item else {
                continue;
            };
            if function.kind != FunctionKind::Proc {
                continue;
            }
            procedures.insert(function.name.name.clone(), procedure_from_decl(function)?);
        }
    }
    Ok(procedures)
}

pub(super) fn collect_constructors(
    package: &ParsedPackage,
) -> Result<HashMap<String, ConstructorSignature>> {
    let mut constructors = HashMap::new();
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Type(type_decl) = item else {
            continue;
        };

        for constructor in &type_decl.constructors {
            let Some(arity) = constructor_arity(constructor, &type_decl.name) else {
                continue;
            };

            let key = constructor_key(&type_decl.name.name, &constructor.name.name);
            let value = ConstructorSignature {
                type_name: type_decl.name.name.clone(),
                constructor_name: constructor.name.name.clone(),
                arity,
                is_rc: type_decl.modifier.as_deref() == Some("rc"),
                tag: constructors.len() as i32,
            };
            if constructors.insert(key.clone(), value).is_some() {
                return Err(Error::Unsupported(format!(
                    "duplicate constructor `{key}` is not supported"
                )));
            }
        }
    }
    Ok(constructors)
}

fn constructor_arity(
    constructor: &ConstructorDeclaration,
    type_name: &DeclaredName,
) -> Option<usize> {
    let mut arity = 0usize;
    let mut current = &constructor.ty;
    while let Term::Arrow(arrow) = current {
        arity += 1;
        current = arrow.result.as_ref();
    }

    let Term::Path(path) = current else {
        return None;
    };
    if !path.starts_with_package
        && path.segments.len() == 1
        && path.segments[0].name == type_name.name
        && path.segments[0].suffixes == type_name.suffixes
    {
        Some(arity)
    } else {
        None
    }
}

pub(super) fn constructor_key(type_name: &str, constructor_name: &str) -> String {
    format!("{type_name}::{constructor_name}")
}

fn pure_function_from_decl(function: &FunctionDeclaration) -> Result<PureFunction> {
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

fn procedure_from_decl(function: &FunctionDeclaration) -> Result<Procedure> {
    let mut parameters = Vec::new();
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        let ArrowParameter::Binder(binder) = &arrow.parameter else {
            return Err(Error::Unsupported(format!(
                "procedure `{}` must use named parameters",
                function.name.name
            )));
        };
        parameters.push(ProcedureParameter {
            name: binder.name.clone(),
            ty: binder.ty.as_ref().clone(),
        });
        current = arrow.result.as_ref();
    }
    if !matches!(current, Term::Unit) {
        return Err(Error::Unsupported(format!(
            "procedure `{}` must return `()`",
            function.name.name
        )));
    }
    if !matches!(function.body.tail.as_deref(), Some(Term::Unit)) {
        return Err(Error::Unsupported(format!(
            "procedure `{}` body must end with `()`",
            function.name.name
        )));
    }
    Ok(Procedure {
        parameters,
        body: function.body.clone(),
    })
}
