use std::collections::{HashMap, HashSet};

use neco_rs_parser::{
    ArrowParameter, Block, ConstructorDeclaration, DeclaredName, FunctionDeclaration,
    StructDeclaration, Term,
};

use crate::{Error, Result};

use super::symbol::SymbolTable;

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
    symbols: &SymbolTable<'_>,
) -> Result<HashMap<String, PureFunction>> {
    let mut functions = HashMap::new();
    for function in symbols.function_declarations() {
        if function.effect.is_some() {
            continue;
        }
        let name = function.name.name.clone();
        let value = pure_function_from_decl(function)?;
        if functions.insert(name.clone(), value).is_some() {
            return Err(Error::Unsupported(format!(
                "duplicate function `{name}` is not supported"
            )));
        }
    }
    Ok(functions)
}

pub(super) fn collect_statement_functions(
    symbols: &SymbolTable<'_>,
) -> Result<HashMap<String, StatementFunction>> {
    let mut functions = HashMap::new();
    for function in symbols.function_declarations() {
        if function
            .effect
            .as_ref()
            .is_some_and(|effect| effect.lexeme == "PTX")
        {
            continue;
        }
        let name = function.name.name.clone();
        let value = statement_function_from_decl(function)?;
        if functions.insert(name.clone(), value).is_some() {
            return Err(Error::Unsupported(format!(
                "duplicate function `{name}` is not supported"
            )));
        }
    }
    Ok(functions)
}

pub(super) fn collect_constructors(
    symbols: &SymbolTable<'_>,
) -> Result<HashMap<String, ConstructorSignature>> {
    let mut constructors = HashMap::new();
    for type_decl in symbols.type_declarations() {
        validate_modifier("type", &type_decl.name.name, type_decl.modifier.as_deref())?;
        validate_type_kind_annotation("type", &type_decl.name, &type_decl.ty)?;

        for (tag, constructor) in type_decl.constructors.iter().enumerate() {
            let parameters = constructor_parameters(constructor, &type_decl.name)?;

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
    Ok(constructors)
}

pub(super) fn collect_structs(
    symbols: &SymbolTable<'_>,
) -> Result<HashMap<String, StructSignature>> {
    let mut structs = HashMap::new();
    let type_names = collect_type_names(symbols)?;
    for struct_decl in symbols.struct_declarations() {
        let signature = struct_signature_from_decl(struct_decl)?;
        if structs
            .insert(signature.type_name.clone(), signature)
            .is_some()
        {
            return Err(Error::Unsupported(format!(
                "duplicate struct `{}` is not supported",
                struct_decl.name.name
            )));
        }
        if type_names.contains(&struct_decl.name.name) {
            return Err(Error::Unsupported(format!(
                "type name `{}` is already used by an algebraic type",
                struct_decl.name.name
            )));
        }
    }
    Ok(structs)
}

pub(super) fn collect_builtin_aliases(
    symbols: &SymbolTable<'_>,
) -> Result<HashMap<String, String>> {
    let mut aliases = HashMap::new();
    for bind_builtin in symbols.bind_builtin_declarations() {
        if aliases
            .insert(
                bind_builtin.alias.clone(),
                bind_builtin.builtin_name.clone(),
            )
            .is_some()
        {
            return Err(Error::Unsupported(format!(
                "duplicate builtin alias `{}` is not supported",
                bind_builtin.alias
            )));
        }
    }
    Ok(aliases)
}

fn collect_type_names(symbols: &SymbolTable<'_>) -> Result<HashSet<String>> {
    let mut type_names = HashSet::new();
    for type_decl in symbols.type_declarations() {
        if !type_names.insert(type_decl.name.name.clone()) {
            return Err(Error::Unsupported(format!(
                "duplicate type `{}` is not supported",
                type_decl.name.name
            )));
        }
    }
    Ok(type_names)
}

fn struct_signature_from_decl(struct_decl: &StructDeclaration) -> Result<StructSignature> {
    validate_modifier(
        "struct",
        &struct_decl.name.name,
        struct_decl.modifier.as_deref(),
    )?;
    validate_type_kind_annotation("struct", &struct_decl.name, &struct_decl.ty)?;

    let mut field_names = HashSet::new();
    let mut fields = Vec::new();
    for field in &struct_decl.fields {
        let name = field.token_ident.lexeme.clone();
        if !field_names.insert(name.clone()) {
            return Err(Error::Unsupported(format!(
                "duplicate field `{name}` in struct `{}`",
                struct_decl.name.name
            )));
        }
        fields.push(StructFieldSignature {
            name,
            ty: field.ty.clone(),
        });
    }

    Ok(StructSignature {
        type_name: struct_decl.name.name.clone(),
        is_rc: struct_decl.modifier.as_deref() == Some("rc"),
        fields,
    })
}

fn validate_modifier(decl_kind: &str, name: &str, modifier: Option<&str>) -> Result<()> {
    match modifier {
        Some("rc") | None => Ok(()),
        Some(modifier) => Err(Error::Unsupported(format!(
            "unknown {decl_kind} modifier `{modifier}` on `{name}`"
        ))),
    }
}

fn validate_type_kind_annotation(decl_kind: &str, name: &DeclaredName, ty: &Term) -> Result<()> {
    let mut current = ty;
    while let Term::Arrow(arrow) = current {
        let parameter_ty = match &arrow.parameter {
            ArrowParameter::Binder(binder) => binder.ty.as_ref(),
            ArrowParameter::Domain(domain) => domain.as_ref(),
        };
        if !is_type_zero_annotation(parameter_ty) {
            return Err(Error::Unsupported(format!(
                "{decl_kind} `{}` kind parameters must have kind `Type[0]`",
                name.name
            )));
        }
        current = arrow.result.as_ref();
    }

    if is_type_zero_annotation(current) {
        Ok(())
    } else {
        Err(Error::Unsupported(format!(
            "{decl_kind} `{}` kind must end in `Type[0]`",
            name.name
        )))
    }
}

fn is_type_zero_annotation(ty: &Term) -> bool {
    let Term::Application { callee, arguments } = ty else {
        return false;
    };
    let Term::Path(path) = callee.as_ref() else {
        return false;
    };
    path.token_keyword_package.is_none()
        && path.segments.len() == 1
        && path.segments[0].lexeme == "Type"
        && matches!(arguments.as_slice(), [Term::IntegerLiteral(level)] if level == "0")
}

fn constructor_parameters(
    constructor: &ConstructorDeclaration,
    type_name: &DeclaredName,
) -> Result<Vec<ConstructorParameter>> {
    let mut parameter_names = HashSet::new();
    let mut parameters = Vec::new();
    let mut current = &constructor.ty;
    while let Term::Arrow(arrow) = current {
        let (name, ty) = match &arrow.parameter {
            ArrowParameter::Binder(binder) => {
                validate_parameter_name(
                    &mut parameter_names,
                    &binder.name,
                    "constructor",
                    &constructor.name.name,
                )?;
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
                return Err(invalid_constructor_result_error(constructor, type_name));
            };
            path
        }
        _ => return Err(invalid_constructor_result_error(constructor, type_name)),
    };
    if path.token_keyword_package.is_none()
        && path.segments.len() == 1
        && path.segments[0].lexeme == type_name.name
    {
        Ok(parameters)
    } else {
        Err(invalid_constructor_result_error(constructor, type_name))
    }
}

fn invalid_constructor_result_error(
    constructor: &ConstructorDeclaration,
    type_name: &DeclaredName,
) -> Error {
    Error::Unsupported(format!(
        "constructor `{}` result type must be `{}`",
        constructor.name.name, type_name.name
    ))
}

pub(super) fn constructor_key(type_name: &str, constructor_name: &str) -> String {
    format!("{type_name}::{constructor_name}")
}

pub(super) fn pure_function_from_decl(function: &FunctionDeclaration) -> Result<PureFunction> {
    let mut parameter_names = HashSet::new();
    let mut parameters = Vec::new();
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        let ArrowParameter::Binder(binder) = &arrow.parameter else {
            return Err(Error::Unsupported(format!(
                "pure function `{}` must use named parameters",
                function.name.name
            )));
        };
        validate_parameter_name(
            &mut parameter_names,
            &binder.name,
            "function",
            &function.name.name,
        )?;
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
    validate_function_effect(function)?;

    let mut parameter_names = HashSet::new();
    let mut parameters = Vec::new();
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        let ArrowParameter::Binder(binder) = &arrow.parameter else {
            return Err(Error::Unsupported(format!(
                "function `{}` must use named parameters",
                function.name.name
            )));
        };
        validate_parameter_name(
            &mut parameter_names,
            &binder.name,
            "function",
            &function.name.name,
        )?;
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

fn validate_parameter_name(
    parameter_names: &mut HashSet<String>,
    name: &str,
    decl_kind: &str,
    decl_name: &str,
) -> Result<()> {
    if parameter_names.insert(name.to_string()) {
        return Ok(());
    }
    Err(Error::Unsupported(format!(
        "duplicate parameter `{name}` in {decl_kind} `{decl_name}`"
    )))
}

fn validate_function_effect(function: &FunctionDeclaration) -> Result<()> {
    if function
        .effect
        .as_ref()
        .is_some_and(|effect| !matches!(effect.lexeme.as_str(), "IO" | "PTX"))
    {
        return Err(Error::Unsupported(format!(
            "function `{}` effect must be `IO` or `PTX`",
            function.name.name
        )));
    }
    Ok(())
}
