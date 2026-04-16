use neco_rs_parser::Term;

use crate::effect::{Value, resolve_value};
use crate::ir::{ArrayElementType, ArrayKind, ComparisonKind, ConditionExpr, I32Expr, U8Expr};
use crate::{Error, Result};

use super::LoweringState;
use super::typecheck::{
    is_i32_suffix_term, is_u8_suffix_term, parse_bare_i32_literal, parse_bare_u8_literal,
    parse_suffixed_i32_literal, parse_suffixed_u8_literal,
};

pub(crate) fn lower_i32_expr(term: &Term, state: &LoweringState) -> Result<I32Expr> {
    match term {
        Term::Group(inner) => lower_i32_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_i32_literal(literal),
        Term::MethodCall { .. } => lower_i32_method_call(term, state),
        Term::Path(_) => match resolve_value(term, &state.environment)? {
            Value::I32(expr) => Ok(expr),
            other => Err(Error::Unsupported(format!(
                "expected an `i32` value, got {other:?}"
            ))),
        },
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_i32_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_len_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_i32_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_i32_reference_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_array_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            lower_i32_primitive_call(callee, arguments, state)
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `i32` expression in entrypoint body: {term:?}"
        ))),
    }
}

fn lower_i32_method_call(term: &Term, state: &LoweringState) -> Result<I32Expr> {
    let Term::MethodCall { receiver, method } = term else {
        unreachable!();
    };
    match method.as_str() {
        "get" => match resolve_value(receiver.as_ref(), &state.environment)? {
            Value::I32Reference(slot) => Ok(I32Expr::Local(slot)),
            other => Err(Error::Unsupported(format!(
                "`get` expects an `i32` reference, got {other:?}"
            ))),
        },
        "len" => match resolve_value(receiver.as_ref(), &state.environment)? {
            Value::Array {
                slot,
                kind: ArrayKind::Dynamic,
                ..
            } => Ok(I32Expr::ArrayLen { array_slot: slot }),
            other => Err(Error::Unsupported(format!(
                "`len` expects a dynamic array reference, got {other:?}"
            ))),
        },
        _ => Err(Error::Unsupported(format!(
            "unsupported `i32` expression in entrypoint body: {term:?}"
        ))),
    }
}

pub(crate) fn lower_u8_expr(term: &Term, state: &LoweringState) -> Result<U8Expr> {
    match term {
        Term::Group(inner) => lower_u8_expr(inner, state),
        Term::CharLiteral(value) => Ok(U8Expr::Literal(*value as u8)),
        Term::IntegerLiteral(literal) => parse_suffixed_u8_literal(literal),
        Term::Path(_) => match resolve_value(term, &state.environment)? {
            Value::U8(expr) => Ok(expr),
            other => Err(Error::Unsupported(format!(
                "expected a `u8` value, got {other:?}"
            ))),
        },
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_u8_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_u8_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_runtime_arg_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_u8_array_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            lower_u8_primitive_call(callee, arguments, state)
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `u8` expression in entrypoint body: {term:?}"
        ))),
    }
}

pub(super) fn lower_condition_expr(term: &Term, state: &LoweringState) -> Result<ConditionExpr> {
    let Term::Application { callee, arguments } = term else {
        return Err(Error::Unsupported(
            "`#if` condition must be a comparison call".to_string(),
        ));
    };
    let Term::Path(path) = callee.as_ref() else {
        return Err(Error::Unsupported(
            "`#if` condition must use a path callee".to_string(),
        ));
    };

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`#if` condition must receive exactly two arguments".to_string(),
        ));
    };

    let primitive = path
        .segments
        .last()
        .map(|segment| segment.name.as_str())
        .unwrap_or_default();

    if let Some(kind) = i32_comparison_kind(primitive) {
        return Ok(ConditionExpr::I32 {
            kind,
            lhs: lower_i32_expr(lhs, state)?,
            rhs: lower_i32_expr(rhs, state)?,
        });
    }
    if let Some(kind) = u8_comparison_kind(primitive) {
        return Ok(ConditionExpr::U8 {
            kind,
            lhs: lower_u8_expr(lhs, state)?,
            rhs: lower_u8_expr(rhs, state)?,
        });
    }

    Err(Error::Unsupported(format!(
        "unsupported `#if` condition `{primitive}`"
    )))
}

fn lower_i32_literal_application(callee: &Term, arguments: &[Term]) -> Result<Option<I32Expr>> {
    let [suffix] = arguments else {
        return Ok(None);
    };
    let Term::IntegerLiteral(literal) = callee else {
        return Ok(None);
    };
    if !is_i32_suffix_term(suffix) {
        return Ok(None);
    }
    Ok(Some(parse_bare_i32_literal(literal)?))
}

fn lower_dyn_array_len_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.starts_with_package || path.segments.len() != 1 || !path.segments[0].suffixes.is_empty()
    {
        return Ok(None);
    }
    match path.segments[0].name.as_str() {
        "dyn_array_len_u8" | "dyn_array_len_i32" => {}
        _ => return Ok(None),
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [array] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{}` must receive exactly one array argument",
            path.segments[0].name
        )));
    };
    let Value::Constructor(constructor) = resolve_value(array, &state.environment)? else {
        return Err(Error::Unsupported(format!(
            "`{}` expects a `DynArray` value",
            path.segments[0].name
        )));
    };
    let [Value::Array { slot, .. }, Value::I32(_), Value::I32(_)] = constructor.fields.as_slice()
    else {
        return Err(Error::Unsupported(
            "`DynArray` value must contain raw array, capacity, and logical length".to_string(),
        ));
    };
    Ok(Some(I32Expr::ArrayLen { array_slot: *slot }))
}

fn lower_u8_literal_application(callee: &Term, arguments: &[Term]) -> Result<Option<U8Expr>> {
    let [suffix] = arguments else {
        return Ok(None);
    };
    let Term::IntegerLiteral(literal) = callee else {
        return Ok(None);
    };
    if !is_u8_suffix_term(suffix) {
        return Ok(None);
    }
    Ok(Some(parse_bare_u8_literal(literal)?))
}

fn lower_dyn_array_u8_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<U8Expr>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.starts_with_package || path.segments.len() != 1 || !path.segments[0].suffixes.is_empty()
    {
        return Ok(None);
    }
    if path.segments[0].name != "dyn_array_get_u8" {
        return Ok(None);
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [array, index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`dyn_array_get_u8` must receive an array and an index".to_string(),
        ));
    };
    let Value::Constructor(constructor) = resolve_value(array, &state.environment)? else {
        return Err(Error::Unsupported(
            "`dyn_array_get_u8` expects a `DynArray` value".to_string(),
        ));
    };
    let [Value::Array {
        slot,
        element_type: ArrayElementType::U8,
        ..
    }, Value::I32(_), Value::I32(_)] = constructor.fields.as_slice()
    else {
        return Err(Error::Unsupported(
            "`DynArray` value must contain a `u8` raw array and two `i32` lengths".to_string(),
        ));
    };
    Ok(Some(U8Expr::ArrayGet {
        array_slot: *slot,
        index: Box::new(lower_i32_expr(index, state)?),
    }))
}

fn i32_comparison_kind(name: &str) -> Option<ComparisonKind> {
    match name {
        "i32_eq" => Some(ComparisonKind::Eq),
        "i32_lte" => Some(ComparisonKind::Lte),
        "i32_lt" => Some(ComparisonKind::Lt),
        "i32_gte" => Some(ComparisonKind::Gte),
        "i32_gt" => Some(ComparisonKind::Gt),
        _ => None,
    }
}

fn u8_comparison_kind(name: &str) -> Option<ComparisonKind> {
    match name {
        "u8_eq" => Some(ComparisonKind::Eq),
        "u8_lte" => Some(ComparisonKind::Lte),
        "u8_lt" => Some(ComparisonKind::Lt),
        "u8_gte" => Some(ComparisonKind::Gte),
        "u8_gt" => Some(ComparisonKind::Gt),
        _ => None,
    }
}

fn lower_i32_primitive_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<I32Expr> {
    let Term::Path(path) = callee else {
        return Err(Error::Unsupported(
            "unsupported `i32` callee in entrypoint body".to_string(),
        ));
    };

    let segments: Vec<_> = path
        .segments
        .iter()
        .map(|segment| segment.name.as_str())
        .collect();
    let normalized = normalize_numeric_literal_arguments(arguments);
    let primitive = segments.last().copied().unwrap_or_default();
    if primitive == "i32_from_u8" {
        let [value] = normalized.as_slice() else {
            return Err(Error::Unsupported(format!(
                "`{}` must receive exactly one argument",
                segments.join("::")
            )));
        };
        return Ok(I32Expr::FromU8(Box::new(lower_u8_expr(value, state)?)));
    }
    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{}` must receive exactly two arguments",
            segments.join("::")
        )));
    };
    let lhs = Box::new(lower_i32_expr(lhs, state)?);
    let rhs = Box::new(lower_i32_expr(rhs, state)?);

    match primitive {
        "i32_add" => Ok(I32Expr::Add(lhs, rhs)),
        "i32_sub" => Ok(I32Expr::Sub(lhs, rhs)),
        "i32_mul" => Ok(I32Expr::Mul(lhs, rhs)),
        "i32_div" => Ok(I32Expr::Div(lhs, rhs)),
        "i32_mod" => Ok(I32Expr::Mod(lhs, rhs)),
        _ => Err(Error::Unsupported(format!(
            "unsupported `i32` primitive call `{}`",
            segments.join("::")
        ))),
    }
}

fn lower_u8_primitive_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<U8Expr> {
    let Term::Path(path) = callee else {
        return Err(Error::Unsupported(
            "unsupported `u8` callee in entrypoint body".to_string(),
        ));
    };

    let segments: Vec<_> = path
        .segments
        .iter()
        .map(|segment| segment.name.as_str())
        .collect();
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{}` must receive exactly two arguments",
            segments.join("::")
        )));
    };
    let lhs = Box::new(lower_u8_expr(lhs, state)?);
    let rhs = Box::new(lower_u8_expr(rhs, state)?);

    let primitive = segments.last().copied().unwrap_or_default();

    match primitive {
        "u8_add" => Ok(U8Expr::Add(lhs, rhs)),
        "u8_sub" => Ok(U8Expr::Sub(lhs, rhs)),
        "u8_mul" => Ok(U8Expr::Mul(lhs, rhs)),
        "u8_div" => Ok(U8Expr::Div(lhs, rhs)),
        "u8_mod" => Ok(U8Expr::Mod(lhs, rhs)),
        _ => Err(Error::Unsupported(format!(
            "unsupported `u8` primitive call `{}`",
            segments.join("::")
        ))),
    }
}

fn lower_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`get` must receive exactly one index argument".to_string(),
        ));
    };

    let array_slot = match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::Array {
            slot,
            element_type: ArrayElementType::I32,
            ..
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`get` expects an `i32` array reference, got {other:?}"
            )));
        }
    };

    Ok(Some(I32Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_i32_expr(index, state)?),
    }))
}

fn lower_dyn_array_i32_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.starts_with_package || path.segments.len() != 1 || !path.segments[0].suffixes.is_empty()
    {
        return Ok(None);
    }
    if path.segments[0].name != "dyn_array_get_i32" {
        return Ok(None);
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [array, index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`dyn_array_get_i32` must receive an array and an index".to_string(),
        ));
    };
    let Value::Constructor(constructor) = resolve_value(array, &state.environment)? else {
        return Err(Error::Unsupported(
            "`dyn_array_get_i32` expects a `DynArray` value".to_string(),
        ));
    };
    let [Value::Array {
        slot,
        element_type: ArrayElementType::I32,
        ..
    }, Value::I32(_), Value::I32(_)] = constructor.fields.as_slice()
    else {
        return Err(Error::Unsupported(
            "`DynArray` value must contain an `i32` raw array and two `i32` lengths".to_string(),
        ));
    };
    Ok(Some(I32Expr::ArrayGet {
        array_slot: *slot,
        index: Box::new(lower_i32_expr(index, state)?),
    }))
}

fn lower_i32_reference_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" || !arguments.is_empty() {
        return Ok(None);
    }

    match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::I32Reference(slot) => Ok(Some(I32Expr::Local(slot))),
        _ => Ok(None),
    }
}

fn lower_u8_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<U8Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`get` must receive exactly one index argument".to_string(),
        ));
    };

    let array_slot = match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
            ..
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`get` expects a `u8` array reference, got {other:?}"
            )));
        }
    };

    Ok(Some(U8Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_i32_expr(index, state)?),
    }))
}

fn lower_runtime_arg_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<U8Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`get` must receive exactly one index argument".to_string(),
        ));
    };

    match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::RuntimeArg(arg_index) => Ok(Some(U8Expr::RuntimeArgGet {
            arg_index: Box::new(arg_index),
            index: Box::new(lower_i32_expr(index, state)?),
        })),
        Value::ByteString(data_index) => Ok(Some(U8Expr::StaticDataGet {
            data_index,
            index: Box::new(lower_i32_expr(index, state)?),
        })),
        _ => Ok(None),
    }
}

pub(crate) fn normalize_numeric_literal_arguments(arguments: &[Term]) -> Vec<Term> {
    let mut normalized = Vec::with_capacity(arguments.len());
    let mut index = 0;
    while index < arguments.len() {
        if index + 1 < arguments.len()
            && matches!(arguments[index], Term::IntegerLiteral(_))
            && (is_i32_suffix_term(&arguments[index + 1])
                || is_u8_suffix_term(&arguments[index + 1]))
        {
            normalized.push(Term::Application {
                callee: Box::new(arguments[index].clone()),
                arguments: vec![arguments[index + 1].clone()],
            });
            index += 2;
            continue;
        }
        normalized.push(arguments[index].clone());
        index += 1;
    }
    normalized
}
