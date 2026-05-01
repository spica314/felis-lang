use neco_rs_parser::Term;

use crate::effect::{Value, resolve_value};
use crate::ir::{
    ArrayElementType, ArrayKind, ComparisonKind, ConditionExpr, I32Expr, I64Expr, LoweredProgram,
    U8Expr,
};
use crate::{Error, Result};

use super::LoweringState;
use super::pure::lower_pure_value;
use super::typecheck::{
    is_i32_suffix_term, is_i64_suffix_term, is_u8_suffix_term, parse_bare_i32_literal,
    parse_bare_i64_literal, parse_bare_u8_literal, parse_suffixed_i32_literal,
    parse_suffixed_i64_literal, parse_suffixed_u8_literal,
};

pub(crate) fn lower_i32_expr(term: &Term, state: &LoweringState) -> Result<I32Expr> {
    match term {
        Term::Group(inner) => lower_i32_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_i32_literal(literal),
        Term::MethodCall { .. } => lower_i32_method_call(term, state),
        Term::Path(_) | Term::FieldAccess { .. } => {
            match resolve_value(term, &state.environment)? {
                Value::I32(expr) => Ok(expr),
                other => Err(Error::Unsupported(format!(
                    "expected an `i32` value, got {other:?}"
                ))),
            }
        }
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_i32_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_get_i32_call(callee, arguments, state)? {
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
            Value::Reference { value, .. } => match value.as_ref() {
                Value::I32(expr) => Ok(expr.clone()),
                other => Err(Error::Unsupported(format!(
                    "`get` expects an `i32` reference, got {other:?}"
                ))),
            },
            other => Err(Error::Unsupported(format!(
                "`get` expects an `i32` reference, got {other:?}"
            ))),
        },
        "len" => match resolve_value(receiver.as_ref(), &state.environment)? {
            Value::StaticSlice { len, .. } => Ok(I32Expr::Literal(len)),
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

pub(crate) fn lower_i64_expr(term: &Term, state: &LoweringState) -> Result<I64Expr> {
    match term {
        Term::Group(inner) => lower_i64_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_i64_literal(literal),
        Term::MethodCall { .. } => lower_i64_method_call(term, state),
        Term::Path(_) | Term::FieldAccess { .. } => {
            match resolve_value(term, &state.environment)? {
                Value::I64(expr) => Ok(expr),
                other => Err(Error::Unsupported(format!(
                    "expected an `i64` value, got {other:?}"
                ))),
            }
        }
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_i64_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_get_i64_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_i64_reference_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_i64_array_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            lower_i64_primitive_call(callee, arguments, state)
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `i64` expression in entrypoint body: {term:?}"
        ))),
    }
}

fn lower_i64_method_call(term: &Term, state: &LoweringState) -> Result<I64Expr> {
    let Term::MethodCall { receiver, method } = term else {
        unreachable!();
    };
    match method.as_str() {
        "get" => match resolve_value(receiver.as_ref(), &state.environment)? {
            Value::I64Reference(slot) => Ok(I64Expr::Local(slot)),
            Value::Reference { value, .. } => match value.as_ref() {
                Value::I64(expr) => Ok(expr.clone()),
                other => Err(Error::Unsupported(format!(
                    "`get` expects an `i64` reference, got {other:?}"
                ))),
            },
            other => Err(Error::Unsupported(format!(
                "`get` expects an `i64` reference, got {other:?}"
            ))),
        },
        "len" => match resolve_value(receiver.as_ref(), &state.environment)? {
            Value::Array {
                slot,
                kind: ArrayKind::Dynamic,
                ..
            } => Ok(I64Expr::ArrayLen { array_slot: slot }),
            other => Err(Error::Unsupported(format!(
                "`len` expects a dynamic array reference, got {other:?}"
            ))),
        },
        _ => Err(Error::Unsupported(format!(
            "unsupported `i64` expression in entrypoint body: {term:?}"
        ))),
    }
}

pub(crate) fn lower_u8_expr(term: &Term, state: &LoweringState) -> Result<U8Expr> {
    match term {
        Term::Group(inner) => lower_u8_expr(inner, state),
        Term::CharLiteral(value) => Ok(U8Expr::Literal(*value as u8)),
        Term::IntegerLiteral(literal) => parse_suffixed_u8_literal(literal),
        Term::Path(_) | Term::FieldAccess { .. } => {
            match resolve_value(term, &state.environment)? {
                Value::U8(expr) => Ok(expr),
                other => Err(Error::Unsupported(format!(
                    "expected a `u8` value, got {other:?}"
                ))),
            }
        }
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_u8_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_get_u8_call(callee, arguments, state)? {
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

pub(super) fn lower_condition_expr(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<ConditionExpr> {
    lower_condition_bool_expr(term, state, program)
}

fn lower_condition_bool_expr(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<ConditionExpr> {
    if let Ok(condition) = lower_condition_bool_expr_inner(term, state, program) {
        return Ok(condition);
    }

    match lower_pure_value(term, state, program) {
        Ok(Value::Bool(condition)) => Ok(condition),
        Ok(other) => Err(Error::Unsupported(format!(
            "`#if` condition must be a `bool` expression, got {other:?}"
        ))),
        Err(_) => Err(Error::Unsupported(
            "`#if` condition must be a `bool` expression".to_string(),
        )),
    }
}

fn lower_condition_bool_expr_inner(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<ConditionExpr> {
    match term {
        Term::Group(inner) => lower_condition_bool_expr(inner, state, program),
        Term::Path(_) | Term::FieldAccess { .. } => lower_bool_expr(term, state),
        Term::Application { .. } => lower_condition_bool_application_expr(term, state, program),
        _ => lower_bool_expr(term, state),
    }
}

fn lower_condition_bool_application_expr(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<ConditionExpr> {
    let Term::Application { callee, arguments } = term else {
        return Err(Error::Unsupported(
            "`bool` expression must be a primitive or comparison call".to_string(),
        ));
    };
    let Term::Path(path) = callee.as_ref() else {
        return Err(Error::Unsupported(
            "`bool` expression must use a path callee".to_string(),
        ));
    };

    let primitive = path
        .segments
        .last()
        .map(|segment| segment.lexeme.as_str())
        .unwrap_or_default();

    match primitive {
        "bool_and" => {
            let [lhs, rhs] = arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`bool_and` must receive exactly two arguments".to_string(),
                ));
            };
            Ok(ConditionExpr::And(
                Box::new(lower_condition_bool_expr(lhs, state, program)?),
                Box::new(lower_condition_bool_expr(rhs, state, program)?),
            ))
        }
        "bool_or" => {
            let [lhs, rhs] = arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`bool_or` must receive exactly two arguments".to_string(),
                ));
            };
            Ok(ConditionExpr::Or(
                Box::new(lower_condition_bool_expr(lhs, state, program)?),
                Box::new(lower_condition_bool_expr(rhs, state, program)?),
            ))
        }
        "bool_not" => {
            let [value] = arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`bool_not` must receive exactly one argument".to_string(),
                ));
            };
            Ok(ConditionExpr::Not(Box::new(lower_condition_bool_expr(
                value, state, program,
            )?)))
        }
        _ => lower_comparison_expr(primitive, arguments, state),
    }
}

pub(crate) fn lower_bool_expr(term: &Term, state: &LoweringState) -> Result<ConditionExpr> {
    match term {
        Term::Group(inner) => lower_bool_expr(inner, state),
        Term::Path(_) | Term::FieldAccess { .. } => match resolve_value(term, &state.environment) {
            Ok(Value::Bool(condition)) => Ok(condition),
            Ok(other) => Err(Error::Unsupported(format!(
                "expected a `bool` value, got {other:?}"
            ))),
            Err(_) => lower_bool_builtin_path(term),
        },
        Term::Application { .. } => lower_bool_application_expr(term, state),
        _ => Err(Error::Unsupported(format!(
            "unsupported `bool` expression in entrypoint body: {term:?}"
        ))),
    }
}

fn lower_bool_builtin_path(term: &Term) -> Result<ConditionExpr> {
    let Term::Path(path) = term else {
        return Err(Error::Unsupported(format!(
            "unsupported `bool` expression in entrypoint body: {term:?}"
        )));
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return Err(Error::Unsupported(format!(
            "unsupported `bool` expression in entrypoint body: {term:?}"
        )));
    }
    match path.segments[0].lexeme.as_str() {
        "true" => Ok(ConditionExpr::Literal(true)),
        "false" => Ok(ConditionExpr::Literal(false)),
        name => Err(Error::Unsupported(format!(
            "unknown entrypoint local `{name}`"
        ))),
    }
}

fn lower_bool_application_expr(term: &Term, state: &LoweringState) -> Result<ConditionExpr> {
    let Term::Application { callee, arguments } = term else {
        return Err(Error::Unsupported(
            "`bool` expression must be a primitive or comparison call".to_string(),
        ));
    };
    let Term::Path(path) = callee.as_ref() else {
        return Err(Error::Unsupported(
            "`bool` expression must use a path callee".to_string(),
        ));
    };

    let primitive = path
        .segments
        .last()
        .map(|segment| segment.lexeme.as_str())
        .unwrap_or_default();

    if let Some(condition) = lower_bool_primitive_call(primitive, arguments, state)? {
        return Ok(condition);
    }

    lower_comparison_expr(primitive, arguments, state)
}

fn lower_bool_primitive_call(
    primitive: &str,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<ConditionExpr>> {
    match primitive {
        "bool_and" => {
            let [lhs, rhs] = arguments else {
                return Err(Error::Unsupported(
                    "`bool_and` must receive exactly two arguments".to_string(),
                ));
            };
            Ok(Some(ConditionExpr::And(
                Box::new(lower_bool_expr(lhs, state)?),
                Box::new(lower_bool_expr(rhs, state)?),
            )))
        }
        "bool_or" => {
            let [lhs, rhs] = arguments else {
                return Err(Error::Unsupported(
                    "`bool_or` must receive exactly two arguments".to_string(),
                ));
            };
            Ok(Some(ConditionExpr::Or(
                Box::new(lower_bool_expr(lhs, state)?),
                Box::new(lower_bool_expr(rhs, state)?),
            )))
        }
        "bool_not" => {
            let [value] = arguments else {
                return Err(Error::Unsupported(
                    "`bool_not` must receive exactly one argument".to_string(),
                ));
            };
            Ok(Some(ConditionExpr::Not(Box::new(lower_bool_expr(
                value, state,
            )?))))
        }
        _ => Ok(None),
    }
}

fn lower_comparison_expr(
    primitive: &str,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<ConditionExpr> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`bool` comparison must receive exactly two arguments".to_string(),
        ));
    };

    if let Some(kind) = i32_comparison_kind(primitive) {
        return Ok(ConditionExpr::I32 {
            kind,
            lhs: lower_i32_expr(lhs, state)?,
            rhs: lower_i32_expr(rhs, state)?,
        });
    }
    if let Some(kind) = i64_comparison_kind(primitive) {
        return Ok(ConditionExpr::I64 {
            kind,
            lhs: lower_i64_expr(lhs, state)?,
            rhs: lower_i64_expr(rhs, state)?,
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
        "unsupported `bool` comparison `{primitive}`"
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

fn lower_i64_literal_application(callee: &Term, arguments: &[Term]) -> Result<Option<I64Expr>> {
    let [suffix] = arguments else {
        return Ok(None);
    };
    let Term::IntegerLiteral(literal) = callee else {
        return Ok(None);
    };
    if !is_i64_suffix_term(suffix) {
        return Ok(None);
    }
    Ok(Some(parse_bare_i64_literal(literal)?))
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

fn i64_comparison_kind(name: &str) -> Option<ComparisonKind> {
    match name {
        "i64_eq" => Some(ComparisonKind::Eq),
        "i64_lte" => Some(ComparisonKind::Lte),
        "i64_lt" => Some(ComparisonKind::Lt),
        "i64_gte" => Some(ComparisonKind::Gte),
        "i64_gt" => Some(ComparisonKind::Gt),
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
        .map(|segment| segment.lexeme.as_str())
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
    if primitive == "i32_from_i64" {
        let [value] = normalized.as_slice() else {
            return Err(Error::Unsupported(format!(
                "`{}` must receive exactly one argument",
                segments.join("::")
            )));
        };
        return Ok(I32Expr::FromI64(Box::new(lower_i64_expr(value, state)?)));
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

fn lower_i64_primitive_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<I64Expr> {
    let Term::Path(path) = callee else {
        return Err(Error::Unsupported(
            "unsupported `i64` callee in entrypoint body".to_string(),
        ));
    };

    let segments: Vec<_> = path
        .segments
        .iter()
        .map(|segment| segment.lexeme.as_str())
        .collect();
    let normalized = normalize_numeric_literal_arguments(arguments);
    let primitive = segments.last().copied().unwrap_or_default();
    if primitive == "i64_from_i32" {
        let [value] = normalized.as_slice() else {
            return Err(Error::Unsupported(format!(
                "`{}` must receive exactly one argument",
                segments.join("::")
            )));
        };
        return Ok(I64Expr::FromI32(Box::new(lower_i32_expr(value, state)?)));
    }
    if primitive == "i64_from_u8" {
        let [value] = normalized.as_slice() else {
            return Err(Error::Unsupported(format!(
                "`{}` must receive exactly one argument",
                segments.join("::")
            )));
        };
        return Ok(I64Expr::FromU8(Box::new(lower_u8_expr(value, state)?)));
    }
    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{}` must receive exactly two arguments",
            segments.join("::")
        )));
    };
    let lhs = Box::new(lower_i64_expr(lhs, state)?);
    let rhs = Box::new(lower_i64_expr(rhs, state)?);

    match primitive {
        "i64_add" => Ok(I64Expr::Add(lhs, rhs)),
        "i64_sub" => Ok(I64Expr::Sub(lhs, rhs)),
        "i64_mul" => Ok(I64Expr::Mul(lhs, rhs)),
        "i64_div" => Ok(I64Expr::Div(lhs, rhs)),
        "i64_mod" => Ok(I64Expr::Mod(lhs, rhs)),
        _ => Err(Error::Unsupported(format!(
            "unsupported `i64` primitive call `{}`",
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
        .map(|segment| segment.lexeme.as_str())
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
        index: Box::new(lower_array_index_expr(index, state)?),
    }))
}

fn lower_dyn_array_get_i32_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Some((array, index)) = dyn_array_get_call_parts(
        callee,
        arguments,
        "dyn_array_get_i32",
        ArrayElementType::I32,
        state,
    )?
    else {
        return Ok(None);
    };
    let array_slot = dyn_array_slot(array, state, ArrayElementType::I32)?;
    Ok(Some(I32Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(index, state)?),
    }))
}

fn lower_dyn_array_get_i64_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I64Expr>> {
    let Some((array, index)) = dyn_array_get_call_parts(
        callee,
        arguments,
        "dyn_array_get_i64",
        ArrayElementType::I64,
        state,
    )?
    else {
        return Ok(None);
    };
    let array_slot = dyn_array_slot(array, state, ArrayElementType::I64)?;
    Ok(Some(I64Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(index, state)?),
    }))
}

fn lower_dyn_array_get_u8_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<U8Expr>> {
    let Some((array, index)) = dyn_array_get_call_parts(
        callee,
        arguments,
        "dyn_array_get_u8",
        ArrayElementType::U8,
        state,
    )?
    else {
        return Ok(None);
    };
    let array_slot = dyn_array_slot(array, state, ArrayElementType::U8)?;
    Ok(Some(U8Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(index, state)?),
    }))
}

fn dyn_array_get_call_parts<'a>(
    callee: &'a Term,
    arguments: &'a [Term],
    expected_name: &str,
    expected_element_type: ArrayElementType,
    state: &LoweringState,
) -> Result<Option<(&'a Term, &'a Term)>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return Ok(None);
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    if path.segments[0].lexeme == expected_name {
        if normalized.len() != 2 {
            return Err(Error::Unsupported(format!(
                "`{expected_name}` must receive exactly two arguments"
            )));
        }
        return Ok(Some((&arguments[0], &arguments[1])));
    }
    if path.segments[0].lexeme != "dyn_array_get" {
        return Ok(None);
    }
    if normalized.len() != 3 {
        return Err(Error::Unsupported(format!(
            "`dyn_array_get` must receive exactly three arguments"
        )));
    }
    if !type_argument_matches(&arguments[0], expected_element_type, state) {
        return Ok(None);
    }
    Ok(Some((&arguments[1], &arguments[2])))
}

fn type_argument_matches(
    term: &Term,
    expected_element_type: ArrayElementType,
    state: &LoweringState,
) -> bool {
    let Term::Path(path) = term else {
        return false;
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return false;
    }
    let type_name = match state.environment.get(&path.segments[0].lexeme) {
        Some(Value::Type(Term::Path(bound_path)))
            if bound_path.token_keyword_package.is_none() && bound_path.segments.len() == 1 =>
        {
            bound_path.segments[0].lexeme.as_str()
        }
        _ => path.segments[0].lexeme.as_str(),
    };
    matches!(
        (type_name, expected_element_type),
        ("i32", ArrayElementType::I32)
            | ("i64", ArrayElementType::I64)
            | ("u8", ArrayElementType::U8)
    )
}

fn dyn_array_slot(
    term: &Term,
    state: &LoweringState,
    expected_element_type: ArrayElementType,
) -> Result<usize> {
    match resolve_value(term, &state.environment)? {
        Value::Constructor(constructor)
            if constructor.type_name == "DynArray"
                && constructor.constructor_name == "dyn_array" =>
        {
            let Some(Value::Array {
                slot,
                element_type,
                kind: ArrayKind::Dynamic,
            }) = constructor.fields.first()
            else {
                return Err(Error::Unsupported(format!(
                    "`DynArray::dyn_array` does not contain a dynamic array field: {constructor:?}"
                )));
            };
            if *element_type != expected_element_type {
                return Err(Error::Unsupported(format!(
                    "`DynArray` element type mismatch: expected {expected_element_type:?}, got {element_type:?}"
                )));
            }
            Ok(*slot)
        }
        other => Err(Error::Unsupported(format!(
            "`DynArray` helper expects a `DynArray`, got {other:?}"
        ))),
    }
}

fn lower_i64_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I64Expr>> {
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
            element_type: ArrayElementType::I64,
            ..
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`get` expects an `i64` array reference, got {other:?}"
            )));
        }
    };

    Ok(Some(I64Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(index, state)?),
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

fn lower_i64_reference_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I64Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" || !arguments.is_empty() {
        return Ok(None);
    }

    match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::I64Reference(slot) => Ok(Some(I64Expr::Local(slot))),
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
        index: Box::new(lower_array_index_expr(index, state)?),
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
            index: Box::new(lower_array_index_expr(index, state)?),
        })),
        Value::StaticSlice { data_index, .. } => Ok(Some(U8Expr::StaticDataGet {
            data_index,
            index: Box::new(lower_array_index_expr(index, state)?),
        })),
        _ => Ok(None),
    }
}

pub(crate) fn lower_array_index_expr(term: &Term, state: &LoweringState) -> Result<I64Expr> {
    lower_i64_expr(term, state).or_else(|_| {
        lower_i32_expr(term, state).map(|index| match index {
            I32Expr::Literal(value) => I64Expr::Literal(i64::from(value)),
            other => I64Expr::FromI32(Box::new(other)),
        })
    })
}

pub(crate) fn normalize_numeric_literal_arguments(arguments: &[Term]) -> Vec<Term> {
    let mut normalized = Vec::with_capacity(arguments.len());
    let mut index = 0;
    while index < arguments.len() {
        if index + 1 < arguments.len()
            && matches!(arguments[index], Term::IntegerLiteral(_))
            && (is_i32_suffix_term(&arguments[index + 1])
                || is_i64_suffix_term(&arguments[index + 1])
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
