mod io;

use std::collections::HashMap;

use neco_rs_parser::{BindingPattern, PathExpression, Term};

use crate::{Error, I32Expr, LoweredProgram, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Unit,
    FileDescriptor(u32),
    ByteString(usize),
    I32(I32Expr),
}

pub(crate) fn lower_effect(
    binder: &BindingPattern,
    term: &Term,
    environment: &mut HashMap<String, Value>,
    program: &mut LoweredProgram,
) -> Result<bool> {
    match term {
        Term::Path(path) => {
            let segments = path_segments(path)?;
            if let Some(result) = io::lower_io_reference(binder, &segments, environment) {
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
                io::lower_io_call(binder, &segments, arguments, environment, program)
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
