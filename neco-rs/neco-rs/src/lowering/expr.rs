use neco_rs_parser::{PathExpression, Term};

use crate::effect::{Value, resolve_value};
use crate::ir::{
    ArrayElementType, ArrayKind, ConditionExpr, F32Expr, I32Expr, I64Expr, LoweredProgram, U8Expr,
};
use crate::{Error, Result};

use super::pure::lower_pure_value;
use super::scalar::{ScalarBinaryOp, ScalarPrimitive, ScalarType, ScalarUnaryOp, scalar_primitive};
use super::typecheck::{
    is_f32_suffix_term, is_i32_suffix_term, is_i64_suffix_term, is_u8_suffix_term,
    parse_bare_f32_literal, parse_bare_i32_literal, parse_bare_i64_literal, parse_bare_u8_literal,
    parse_suffixed_f32_literal, parse_suffixed_i32_literal, parse_suffixed_i64_literal,
    parse_suffixed_u8_literal,
};
use super::{LoweringState, ensure_io_effect_allowed};

fn simple_builtin_path_name(path: &PathExpression) -> Option<&str> {
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return None;
    }
    Some(path.segments[0].lexeme.as_str())
}

fn render_path(path: &PathExpression) -> String {
    path.segments
        .iter()
        .map(|segment| segment.lexeme.as_str())
        .collect::<Vec<_>>()
        .join("::")
}

pub(crate) fn lower_i32_expr(term: &Term, state: &LoweringState) -> Result<I32Expr> {
    match term {
        Term::Group(inner) => lower_i32_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_i32_literal(literal),
        Term::MethodCall { .. } => Err(Error::Unsupported(format!(
            "unsupported `i32` expression in entrypoint body: {term:?}"
        ))),
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
            if let Some(expr) = lower_i32_reference_get_builtin_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_get_i32_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_array_len_call(callee, arguments, state)? {
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

pub(crate) fn lower_i64_expr(term: &Term, state: &LoweringState) -> Result<I64Expr> {
    match term {
        Term::Group(inner) => lower_i64_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_i64_literal(literal),
        Term::MethodCall { .. } => Err(Error::Unsupported(format!(
            "unsupported `i64` expression in entrypoint body: {term:?}"
        ))),
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
            if let Some(expr) = lower_i64_reference_get_builtin_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_get_i64_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_i64_array_len_call(callee, arguments, state)? {
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

pub(crate) fn lower_f32_expr(term: &Term, state: &LoweringState) -> Result<F32Expr> {
    match term {
        Term::Group(inner) => lower_f32_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_f32_literal(literal),
        Term::Path(_) | Term::FieldAccess { .. } => {
            match resolve_value(term, &state.environment)? {
                Value::F32(expr) => Ok(expr),
                other => Err(Error::Unsupported(format!(
                    "expected an `f32` value, got {other:?}"
                ))),
            }
        }
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_f32_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_f32_reference_get_builtin_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_get_f32_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_f32_array_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            lower_f32_primitive_call(callee, arguments, state)
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `f32` expression in entrypoint body: {term:?}"
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
            if let Some(expr) = lower_u8_reference_get_builtin_call(callee, arguments, state)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_dyn_array_get_u8_call(callee, arguments, state)? {
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

    let Some(primitive) = simple_builtin_path_name(path) else {
        return Err(Error::Unsupported(format!(
            "`bool` primitive call must use a simple builtin path, got `{}`",
            render_path(path)
        )));
    };

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

    let Some(primitive) = simple_builtin_path_name(path) else {
        return Err(Error::Unsupported(format!(
            "`bool` primitive call must use a simple builtin path, got `{}`",
            render_path(path)
        )));
    };

    if let Some(condition) =
        lower_bool_reference_get_builtin_call(callee.as_ref(), arguments, state)?
    {
        return Ok(condition);
    }

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

    match scalar_primitive(primitive) {
        Some(ScalarPrimitive::Comparison {
            ty: ScalarType::I32,
            kind,
        }) => Ok(ConditionExpr::I32 {
            kind,
            lhs: lower_i32_expr(lhs, state)?,
            rhs: lower_i32_expr(rhs, state)?,
        }),
        Some(ScalarPrimitive::Comparison {
            ty: ScalarType::I64,
            kind,
        }) => Ok(ConditionExpr::I64 {
            kind,
            lhs: lower_i64_expr(lhs, state)?,
            rhs: lower_i64_expr(rhs, state)?,
        }),
        Some(ScalarPrimitive::Comparison {
            ty: ScalarType::F32,
            kind,
        }) => Ok(ConditionExpr::F32 {
            kind,
            lhs: lower_f32_expr(lhs, state)?,
            rhs: lower_f32_expr(rhs, state)?,
        }),
        Some(ScalarPrimitive::Comparison {
            ty: ScalarType::U8,
            kind,
        }) => Ok(ConditionExpr::U8 {
            kind,
            lhs: lower_u8_expr(lhs, state)?,
            rhs: lower_u8_expr(rhs, state)?,
        }),
        _ => Err(Error::Unsupported(format!(
            "unsupported `bool` comparison `{primitive}`"
        ))),
    }
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

fn lower_f32_literal_application(callee: &Term, arguments: &[Term]) -> Result<Option<F32Expr>> {
    let [suffix] = arguments else {
        return Ok(None);
    };
    let Term::IntegerLiteral(literal) = callee else {
        return Ok(None);
    };
    if !is_f32_suffix_term(suffix) {
        return Ok(None);
    }
    parse_bare_f32_literal(literal).map(Some)
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

    let Some(primitive) = simple_builtin_path_name(path) else {
        return Err(Error::Unsupported(format!(
            "`i32` primitive call must use a simple builtin path, got `{}`",
            render_path(path)
        )));
    };
    let descriptor = scalar_primitive(primitive)
        .filter(|descriptor| scalar_primitive_result_type(*descriptor) == Some(ScalarType::I32))
        .ok_or_else(|| {
            Error::Unsupported(format!("unsupported `i32` primitive call `{primitive}`"))
        })?;
    let normalized = normalize_numeric_literal_arguments(arguments);
    ensure_scalar_primitive_arity(primitive, descriptor, normalized.len())?;

    match descriptor {
        ScalarPrimitive::Conversion {
            source: ScalarType::U8,
            ..
        } => Ok(I32Expr::FromU8(Box::new(lower_u8_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::I64,
            ..
        } => Ok(I32Expr::FromI64(Box::new(lower_i64_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::F32,
            ..
        } => Ok(I32Expr::FromF32(Box::new(lower_f32_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Binary { op, .. } => {
            let lhs = Box::new(lower_i32_expr(&normalized[0], state)?);
            let rhs = Box::new(lower_i32_expr(&normalized[1], state)?);
            match op {
                ScalarBinaryOp::Add => Ok(I32Expr::Add(lhs, rhs)),
                ScalarBinaryOp::Sub => Ok(I32Expr::Sub(lhs, rhs)),
                ScalarBinaryOp::Mul => Ok(I32Expr::Mul(lhs, rhs)),
                ScalarBinaryOp::Div => Ok(I32Expr::Div(lhs, rhs)),
                ScalarBinaryOp::Mod => Ok(I32Expr::Mod(lhs, rhs)),
                ScalarBinaryOp::Xor => Ok(I32Expr::Xor(lhs, rhs)),
                ScalarBinaryOp::Shl => Ok(I32Expr::Shl(lhs, rhs)),
                ScalarBinaryOp::Shr => Ok(I32Expr::Shr(lhs, rhs)),
            }
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `i32` primitive call `{primitive}`"
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

    let Some(primitive) = simple_builtin_path_name(path) else {
        return Err(Error::Unsupported(format!(
            "`i64` primitive call must use a simple builtin path, got `{}`",
            render_path(path)
        )));
    };
    let descriptor = scalar_primitive(primitive)
        .filter(|descriptor| scalar_primitive_result_type(*descriptor) == Some(ScalarType::I64))
        .ok_or_else(|| {
            Error::Unsupported(format!("unsupported `i64` primitive call `{primitive}`"))
        })?;
    let normalized = normalize_numeric_literal_arguments(arguments);
    ensure_scalar_primitive_arity(primitive, descriptor, normalized.len())?;

    match descriptor {
        ScalarPrimitive::Conversion {
            source: ScalarType::I32,
            ..
        } => Ok(I64Expr::FromI32(Box::new(lower_i32_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::U8,
            ..
        } => Ok(I64Expr::FromU8(Box::new(lower_u8_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::F32,
            ..
        } => Ok(I64Expr::FromF32(Box::new(lower_f32_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Binary { op, .. } => {
            let lhs = Box::new(lower_i64_expr(&normalized[0], state)?);
            let rhs = Box::new(lower_i64_expr(&normalized[1], state)?);
            match op {
                ScalarBinaryOp::Add => Ok(I64Expr::Add(lhs, rhs)),
                ScalarBinaryOp::Sub => Ok(I64Expr::Sub(lhs, rhs)),
                ScalarBinaryOp::Mul => Ok(I64Expr::Mul(lhs, rhs)),
                ScalarBinaryOp::Div => Ok(I64Expr::Div(lhs, rhs)),
                ScalarBinaryOp::Mod => Ok(I64Expr::Mod(lhs, rhs)),
                _ => Err(Error::Unsupported(format!(
                    "unsupported `i64` primitive call `{primitive}`"
                ))),
            }
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `i64` primitive call `{primitive}`"
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

    let Some(primitive) = simple_builtin_path_name(path) else {
        return Err(Error::Unsupported(format!(
            "`u8` primitive call must use a simple builtin path, got `{}`",
            render_path(path)
        )));
    };
    let descriptor = scalar_primitive(primitive)
        .filter(|descriptor| scalar_primitive_result_type(*descriptor) == Some(ScalarType::U8))
        .ok_or_else(|| {
            Error::Unsupported(format!("unsupported `u8` primitive call `{primitive}`"))
        })?;
    let normalized = normalize_numeric_literal_arguments(arguments);
    ensure_scalar_primitive_arity(primitive, descriptor, normalized.len())?;

    match descriptor {
        ScalarPrimitive::Conversion {
            source: ScalarType::I32,
            ..
        } => Ok(U8Expr::FromI32(Box::new(lower_i32_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::I64,
            ..
        } => Ok(U8Expr::FromI64(Box::new(lower_i64_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::F32,
            ..
        } => Ok(U8Expr::FromF32(Box::new(lower_f32_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Binary { op, .. } => {
            let lhs = Box::new(lower_u8_expr(&normalized[0], state)?);
            let rhs = Box::new(lower_u8_expr(&normalized[1], state)?);
            match op {
                ScalarBinaryOp::Add => Ok(U8Expr::Add(lhs, rhs)),
                ScalarBinaryOp::Sub => Ok(U8Expr::Sub(lhs, rhs)),
                ScalarBinaryOp::Mul => Ok(U8Expr::Mul(lhs, rhs)),
                ScalarBinaryOp::Div => Ok(U8Expr::Div(lhs, rhs)),
                ScalarBinaryOp::Mod => Ok(U8Expr::Mod(lhs, rhs)),
                _ => Err(Error::Unsupported(format!(
                    "unsupported `u8` primitive call `{primitive}`"
                ))),
            }
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `u8` primitive call `{primitive}`"
        ))),
    }
}

fn lower_f32_primitive_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<F32Expr> {
    let Term::Path(path) = callee else {
        return Err(Error::Unsupported(
            "unsupported `f32` callee in entrypoint body".to_string(),
        ));
    };

    let Some(primitive) = simple_builtin_path_name(path) else {
        return Err(Error::Unsupported(format!(
            "`f32` primitive call must use a simple builtin path, got `{}`",
            render_path(path)
        )));
    };
    let descriptor = scalar_primitive(primitive)
        .filter(|descriptor| scalar_primitive_result_type(*descriptor) == Some(ScalarType::F32))
        .ok_or_else(|| {
            Error::Unsupported(format!("unsupported `f32` primitive call `{primitive}`"))
        })?;
    let normalized = normalize_numeric_literal_arguments(arguments);
    ensure_scalar_primitive_arity(primitive, descriptor, normalized.len())?;

    match descriptor {
        ScalarPrimitive::Conversion {
            source: ScalarType::I32,
            ..
        } => Ok(F32Expr::FromI32(Box::new(lower_i32_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::I64,
            ..
        } => Ok(F32Expr::FromI64(Box::new(lower_i64_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Conversion {
            source: ScalarType::U8,
            ..
        } => Ok(F32Expr::FromU8(Box::new(lower_u8_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Unary {
            op: ScalarUnaryOp::Sqrt,
            ..
        } => Ok(F32Expr::Sqrt(Box::new(lower_f32_expr(
            &normalized[0],
            state,
        )?))),
        ScalarPrimitive::Binary { op, .. } => {
            let lhs = Box::new(lower_f32_expr(&normalized[0], state)?);
            let rhs = Box::new(lower_f32_expr(&normalized[1], state)?);
            match op {
                ScalarBinaryOp::Add => Ok(F32Expr::Add(lhs, rhs)),
                ScalarBinaryOp::Sub => Ok(F32Expr::Sub(lhs, rhs)),
                ScalarBinaryOp::Mul => Ok(F32Expr::Mul(lhs, rhs)),
                ScalarBinaryOp::Div => Ok(F32Expr::Div(lhs, rhs)),
                _ => Err(Error::Unsupported(format!(
                    "unsupported `f32` primitive call `{primitive}`"
                ))),
            }
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `f32` primitive call `{primitive}`"
        ))),
    }
}

fn scalar_primitive_result_type(descriptor: ScalarPrimitive) -> Option<ScalarType> {
    match descriptor {
        ScalarPrimitive::Binary { ty, .. } | ScalarPrimitive::Unary { ty, .. } => Some(ty),
        ScalarPrimitive::Conversion { dest, .. } => Some(dest),
        ScalarPrimitive::Comparison { .. } => None,
    }
}

fn ensure_scalar_primitive_arity(
    primitive: &str,
    descriptor: ScalarPrimitive,
    actual: usize,
) -> Result<()> {
    let expected = descriptor.arity();
    if actual == expected {
        return Ok(());
    }
    let noun = if expected == 1 {
        "argument"
    } else {
        "arguments"
    };
    Err(Error::Unsupported(format!(
        "`{primitive}` must receive exactly {expected} {noun}"
    )))
}

fn lower_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Some((receiver, index)) = array_get_call_parts(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "array_get")?;

    let resolved = resolve_value(&receiver, &state.environment)?;
    let Some(array_slot) = array_slot_for_element(&resolved, ArrayElementType::I32) else {
        return Err(Error::Unsupported(format!(
            "`get` expects an `i32` array reference, got {resolved:?}"
        )));
    };

    Ok(Some(I32Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(&index, state)?),
    }))
}

fn lower_array_len_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Some(receiver) = array_len_call_receiver(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "array_len")?;

    let resolved = resolve_value(receiver, &state.environment)?;
    match resolved {
        Value::StaticSlice { len, .. } => Ok(Some(I32Expr::Literal(len))),
        _ => match dynamic_array_slot(&resolved) {
            Some(slot) => Ok(Some(I32Expr::ArrayLen { array_slot: slot })),
            None => Err(Error::Unsupported(format!(
                "`array_len` expects a dynamic array reference, got {resolved:?}"
            ))),
        },
    }
}

fn array_slot_for_element(value: &Value, expected_element_type: ArrayElementType) -> Option<usize> {
    match value {
        Value::Array {
            slot, element_type, ..
        } if *element_type == expected_element_type => Some(*slot),
        Value::Reference { value, .. } => array_slot_for_element(value, expected_element_type),
        _ => None,
    }
}

fn dynamic_array_slot(value: &Value) -> Option<usize> {
    match value {
        Value::Array {
            slot,
            kind: ArrayKind::Dynamic,
            ..
        } => Some(*slot),
        Value::Reference { value, .. } => dynamic_array_slot(value),
        _ => None,
    }
}

fn array_get_call_parts(callee: &Term, arguments: &[Term]) -> Result<Option<(Term, Term)>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some()
        || path.segments.len() != 1
        || path.segments[0].lexeme != "array_get"
    {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [receiver, index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`array_get` must receive exactly an array and an index".to_string(),
        ));
    };
    Ok(Some((receiver.clone(), index.clone())))
}

fn array_len_call_receiver<'a>(
    callee: &'a Term,
    arguments: &'a [Term],
) -> Result<Option<&'a Term>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some()
        || path.segments.len() != 1
        || path.segments[0].lexeme != "array_len"
    {
        return Ok(None);
    }

    let [_receiver] = arguments else {
        return Err(Error::Unsupported(
            "`array_len` must receive exactly an array".to_string(),
        ));
    };
    Ok(Some(&arguments[0]))
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

fn lower_dyn_array_get_f32_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<F32Expr>> {
    let Some((array, index)) = dyn_array_get_call_parts(
        callee,
        arguments,
        "dyn_array_get_f32",
        ArrayElementType::F32,
        state,
    )?
    else {
        return Ok(None);
    };
    let array_slot = dyn_array_slot(array, state, ArrayElementType::F32)?;
    Ok(Some(F32Expr::ArrayGet {
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
        return Err(Error::Unsupported(
            "`dyn_array_get` must receive exactly three arguments".to_string(),
        ));
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
            | ("f32", ArrayElementType::F32)
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
    let Some((receiver, index)) = array_get_call_parts(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "array_get")?;

    let resolved = resolve_value(&receiver, &state.environment)?;
    let Some(array_slot) = array_slot_for_element(&resolved, ArrayElementType::I64) else {
        return Err(Error::Unsupported(format!(
            "`get` expects an `i64` array reference, got {resolved:?}"
        )));
    };

    Ok(Some(I64Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(&index, state)?),
    }))
}

fn lower_i64_array_len_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I64Expr>> {
    let Some(receiver) = array_len_call_receiver(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "array_len")?;

    let resolved = resolve_value(receiver, &state.environment)?;
    match dynamic_array_slot(&resolved) {
        Some(slot) => Ok(Some(I64Expr::ArrayLen { array_slot: slot })),
        None => Err(Error::Unsupported(format!(
            "`array_len` expects a dynamic array reference, got {resolved:?}"
        ))),
    }
}

fn lower_f32_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<F32Expr>> {
    let Some((receiver, index)) = array_get_call_parts(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "array_get")?;

    let resolved = resolve_value(&receiver, &state.environment)?;
    let Some(array_slot) = array_slot_for_element(&resolved, ArrayElementType::F32) else {
        return Err(Error::Unsupported(format!(
            "`get` expects an `f32` array reference, got {resolved:?}"
        )));
    };

    Ok(Some(F32Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(&index, state)?),
    }))
}

fn lower_i32_reference_get_builtin_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Some(receiver) = reference_get_builtin_receiver(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "ref_get")?;

    match resolve_value(receiver, &state.environment)? {
        Value::I32Reference { slot, .. } => Ok(Some(I32Expr::Local(slot))),
        Value::Reference { value, .. } => match value.as_ref() {
            Value::I32(expr) => Ok(Some(expr.clone())),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn lower_f32_reference_get_builtin_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<F32Expr>> {
    let Some(receiver) = reference_get_builtin_receiver(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "ref_get")?;

    match resolve_value(receiver, &state.environment)? {
        Value::F32Reference { slot, .. } => Ok(Some(F32Expr::Local(slot))),
        Value::Reference { value, .. } => match value.as_ref() {
            Value::F32(expr) => Ok(Some(expr.clone())),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn lower_i64_reference_get_builtin_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I64Expr>> {
    let Some(receiver) = reference_get_builtin_receiver(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "ref_get")?;

    match resolve_value(receiver, &state.environment)? {
        Value::I64Reference { slot, .. } => Ok(Some(I64Expr::Local(slot))),
        Value::Reference { value, .. } => match value.as_ref() {
            Value::I64(expr) => Ok(Some(expr.clone())),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn lower_u8_reference_get_builtin_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<U8Expr>> {
    let Some(receiver) = reference_get_builtin_receiver(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "ref_get")?;

    match resolve_value(receiver, &state.environment)? {
        Value::U8Reference { slot, .. } => Ok(Some(U8Expr::Local(slot))),
        Value::Reference { value, .. } => match value.as_ref() {
            Value::U8(expr) => Ok(Some(expr.clone())),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn lower_bool_reference_get_builtin_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<ConditionExpr>> {
    let Some(receiver) = reference_get_builtin_receiver(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "ref_get")?;

    match resolve_value(receiver, &state.environment)? {
        Value::BoolReference { slot, .. } => Ok(Some(ConditionExpr::Local(slot))),
        Value::Reference { value, .. } => match value.as_ref() {
            Value::Bool(condition) => Ok(Some(condition.clone())),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn reference_get_builtin_receiver<'a>(
    callee: &'a Term,
    arguments: &'a [Term],
) -> Result<Option<&'a Term>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some()
        || path.segments.len() != 1
        || path.segments[0].lexeme != "ref_get"
    {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [_ty, _receiver] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`ref_get` must receive exactly a type and a reference".to_string(),
        ));
    };
    Ok(Some(&arguments[1]))
}

fn lower_u8_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<U8Expr>> {
    let Some((receiver, index)) = array_get_call_parts(callee, arguments)? else {
        return Ok(None);
    };
    ensure_io_effect_allowed(state, "array_get")?;

    let resolved = resolve_value(&receiver, &state.environment)?;
    match resolved {
        Value::RuntimeArg(arg_index) => {
            return Ok(Some(U8Expr::RuntimeArgGet {
                arg_index: Box::new(arg_index),
                index: Box::new(lower_array_index_expr(&index, state)?),
            }));
        }
        Value::StaticSlice { data_index, .. } => {
            return Ok(Some(U8Expr::StaticDataGet {
                data_index,
                index: Box::new(lower_array_index_expr(&index, state)?),
            }));
        }
        _ => {}
    }

    let Some(array_slot) = array_slot_for_element(&resolved, ArrayElementType::U8) else {
        return Err(Error::Unsupported(format!(
            "`get` expects a `u8` array reference, got {resolved:?}"
        )));
    };

    Ok(Some(U8Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_array_index_expr(&index, state)?),
    }))
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
                || is_f32_suffix_term(&arguments[index + 1])
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
