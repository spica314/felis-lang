mod io;

use std::collections::HashMap;

use neco_rs_parser::{BindingPattern, PathExpression, Term};

use crate::ir::{
    ArrayElementType, ArrayKind, ConditionExpr, ConstructorValue, I32Expr, I64Expr, LoweredProgram,
    StructValue, U8Expr,
};
use crate::lowering::LoweringState;
use crate::{Error, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Unit,
    Constructor(ConstructorValue),
    Struct(StructValue),
    FileDescriptor(I32Expr),
    StaticSlice {
        data_index: usize,
        len: i32,
    },
    RuntimeArg(I32Expr),
    I32(I32Expr),
    I32Reference(usize),
    I64(I64Expr),
    I64Reference(usize),
    U8(U8Expr),
    Bool(ConditionExpr),
    Array {
        slot: usize,
        element_type: ArrayElementType,
        kind: ArrayKind,
    },
}

pub(crate) fn lower_effect(
    binder: &BindingPattern,
    ty: &Term,
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    match term {
        Term::Path(path) => {
            let segments = path_segments(path)?;
            if let Some(result) = io::lower_io_reference(binder, ty, &segments, state, program) {
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
            if let Some(result) =
                io::lower_io_call(binder, ty, &segments, arguments, state, program)
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
    }
}

pub(crate) fn resolve_value(term: &Term, environment: &HashMap<String, Value>) -> Result<Value> {
    match term {
        Term::Group(inner) => return resolve_value(inner, environment),
        Term::FieldAccess { receiver, field } => {
            let value = resolve_value(receiver, environment)?;
            let Value::Struct(struct_value) = value else {
                return Err(Error::Unsupported(format!(
                    "`.{field}` expects a struct value, got {value:?}"
                )));
            };
            return struct_value
                .fields
                .into_iter()
                .find(|candidate| candidate.name == *field)
                .map(|candidate| candidate.value)
                .ok_or_else(|| {
                    Error::Unsupported(format!(
                        "struct `{}` has no field `{field}`",
                        struct_value.type_name
                    ))
                });
        }
        Term::Path(path) => {
            let segments = path_segments(path)?;
            let [name] = segments.as_slice() else {
                return Err(Error::Unsupported(
                    "only simple local variable references are supported here".to_string(),
                ));
            };

            return environment
                .get(*name)
                .cloned()
                .ok_or_else(|| Error::Unsupported(format!("unknown entrypoint local `{name}`")));
        }
        _ => {}
    }

    Err(Error::Unsupported(format!(
        "expected a variable reference, got {term:?}"
    )))
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
