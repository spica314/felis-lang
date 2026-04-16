mod io;

use std::collections::HashMap;

use neco_rs_parser::{BindingPattern, PathExpression, Term};

use crate::ir::{ArrayElementType, ArrayKind, ConstructorValue, I32Expr, LoweredProgram, U8Expr};
use crate::lowering::LoweringState;
use crate::{Error, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Unit,
    Constructor(ConstructorValue),
    FileDescriptor(I32Expr),
    ByteString(usize),
    RuntimeArg(I32Expr),
    I32(I32Expr),
    I32Reference(usize),
    U8(U8Expr),
    Array {
        slot: usize,
        element_type: ArrayElementType,
        kind: ArrayKind,
    },
}

pub(crate) fn lower_effect(
    binder: &BindingPattern,
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    match term {
        Term::Path(path) => {
            let segments = path_segments(path)?;
            if let Some(result) = io::lower_io_reference(binder, &segments, &mut state.environment)
            {
                return result;
            }

            Err(Error::Unsupported(format!(
                "unsupported effectful reference `{}`",
                segments.join("::")
            )))
        }
        Term::Application { callee, arguments } => {
            let Term::Path(path) = callee.as_ref() else {
                return Err(Error::Unsupported(
                    "effectful entrypoint calls must use a path callee".to_string(),
                ));
            };

            let segments = path_segments(path)?;
            if let Some(result) = io::lower_io_call(binder, &segments, arguments, state, program) {
                return result;
            }
            if let Some(result) =
                lower_dyn_array_new_call(binder, &segments, arguments, state, program)
            {
                return result;
            }

            Err(Error::Unsupported(format!(
                "unsupported effectful call `{}`",
                segments.join("::")
            )))
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported effectful expression in entrypoint body: {term:?}"
        ))),
    }
}

pub(crate) fn bind_pattern(
    pattern: &BindingPattern,
    value: Value,
    environment: &mut HashMap<String, Value>,
) {
    match pattern {
        BindingPattern::Name(name) => {
            environment.insert(name.clone(), value);
        }
        BindingPattern::Wildcard => {}
        BindingPattern::ValueAndReference {
            value: inner,
            reference,
            exclusive: _,
        } => {
            bind_pattern(inner, value.clone(), environment);
            environment.insert(reference.clone(), value);
        }
    }
}

pub(crate) fn resolve_value(term: &Term, environment: &HashMap<String, Value>) -> Result<Value> {
    let Term::Path(path) = term else {
        return Err(Error::Unsupported(format!(
            "expected a variable reference, got {term:?}"
        )));
    };

    let segments = path_segments(path)?;
    let [name] = segments.as_slice() else {
        return Err(Error::Unsupported(
            "only simple local variable references are supported here".to_string(),
        ));
    };

    environment
        .get(*name)
        .cloned()
        .ok_or_else(|| Error::Unsupported(format!("unknown entrypoint local `{name}`")))
}

fn lower_dyn_array_new_call(
    binder: &BindingPattern,
    path: &[&str],
    arguments: &[Term],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Option<Result<bool>> {
    if path != ["dyn_array_new"] {
        return None;
    }

    let (element_type, length) = match parse_dynamic_array_new_arguments(arguments) {
        Ok(value) => value,
        Err(error) => return Some(Err(error)),
    };
    let array_slot = state.allocate_array(element_type, ArrayKind::Dynamic, length, program);
    let len = I32Expr::Literal(length as i32);
    bind_pattern(
        binder,
        Value::Constructor(ConstructorValue {
            type_name: "DynArray".to_string(),
            constructor_name: "dyn_array".to_string(),
            heap_slot: None,
            fields: vec![
                Value::Array {
                    slot: array_slot,
                    element_type,
                    kind: ArrayKind::Dynamic,
                },
                Value::I32(len.clone()),
                Value::I32(len),
            ],
        }),
        &mut state.environment,
    );
    Some(Ok(false))
}

fn parse_dynamic_array_new_arguments(arguments: &[Term]) -> Result<(ArrayElementType, usize)> {
    let normalized = crate::effect::io::normalize_i32_literal_arguments(arguments);
    let [element_type, length] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`dyn_array_new` must receive an element type and a constant i32 length".to_string(),
        ));
    };

    let Term::Path(path) = element_type else {
        return Err(Error::Unsupported(
            "`dyn_array_new` element type must be a simple path".to_string(),
        ));
    };
    let [segment] = path.segments.as_slice() else {
        return Err(Error::Unsupported(
            "`dyn_array_new` element type must be a simple path".to_string(),
        ));
    };
    let element_type = match segment.name.as_str() {
        "i32" => ArrayElementType::I32,
        "u8" => ArrayElementType::U8,
        _ => {
            return Err(Error::Unsupported(
                "`dyn_array_new` currently supports only `i32` and `u8` arrays".to_string(),
            ));
        }
    };

    let length = crate::effect::io::parse_i32_literal_term(length, "dyn_array_new")?;
    let length = usize::try_from(length)
        .map_err(|_| Error::Unsupported("`dyn_array_new` length must be non-negative".to_string()))?;
    Ok((element_type, length))
}

fn path_segments(path: &PathExpression) -> Result<Vec<&str>> {
    if path.starts_with_package {
        return Err(Error::Unsupported(
            "package-qualified paths are not supported in entrypoint lowering".to_string(),
        ));
    }

    if path
        .segments
        .iter()
        .any(|segment| !segment.suffixes.is_empty())
    {
        return Err(Error::Unsupported(
            "path suffixes are not supported in entrypoint lowering".to_string(),
        ));
    }

    Ok(path
        .segments
        .iter()
        .map(|segment| segment.name.as_str())
        .collect())
}
