use std::collections::HashMap;

use neco_rs_parser::{
    ArrowParameter, ArrowTerm, Block, ForallTerm, MatchExpression, PathExpression, Pattern,
    Statement, Term, TypedBinder,
};

use crate::effect::{Value, bind_pattern, resolve_value};
use crate::ir::{
    ComparisonKind, ConditionExpr, ConstructorValue, F32Expr, I32Expr, I64Expr, InternalAbiValue,
    InternalCallArgument, LoweredProgram, Operation, StructFieldValue, StructValue, U8Expr,
    intern_data,
};
use crate::{Error, Result};

use super::declarations::{
    ConstructorSignature, StatementFunction, StatementFunctionParameter, constructor_key,
};
use super::typecheck::{
    is_type_universe_annotation, nul_terminated_bytes, validate_value_against_type,
};
use super::{
    LoweringState, allocate_heap_slot, ensure_io_effect_allowed, lower_statement,
    normalize_numeric_literal_arguments,
};

fn simple_builtin_path_name(
    path: &PathExpression,
    state: &LoweringState,
    kind: &str,
    known_names: &[&str],
) -> Result<Option<String>> {
    if path.token_keyword_package.is_none() && path.segments.len() == 1 {
        return Ok(Some(
            state
                .resolve_builtin_alias(&path.segments[0].lexeme)
                .to_string(),
        ));
    }
    if let Some(name) = path.segments.last().map(|segment| segment.lexeme.as_str())
        && known_names.contains(&state.resolve_builtin_alias(name))
    {
        return Err(Error::Unsupported(format!(
            "`{kind}` builtin call must use a simple builtin path, got `{}`",
            render_path(path)
        )));
    }
    Ok(None)
}

fn render_path(path: &PathExpression) -> String {
    path.segments
        .iter()
        .map(|segment| segment.lexeme.as_str())
        .collect::<Vec<_>>()
        .join("::")
}

pub(super) fn lower_pure_value(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    match term {
        Term::Group(inner) => lower_pure_value(inner, state, program),
        Term::Block(block) => lower_pure_block_value(block, state, program),
        Term::Match(match_expr) => lower_match_value(match_expr, state, program),
        Term::StructLiteral { path, fields } => {
            lower_struct_literal_value(path, fields, state, program)
        }
        Term::StringLiteral(literal) => {
            let data_index = intern_data(program, nul_terminated_bytes(literal));
            let len = i32::try_from(literal.len()).map_err(|_| {
                Error::Unsupported("string literal is too long for `ArrayVL u8` length".to_string())
            })?;
            Ok(Value::StaticSlice { data_index, len })
        }
        Term::CharLiteral(_) => {
            if let Ok(expr) = super::lower_u8_expr(term, state) {
                return Ok(Value::U8(expr));
            }
            Err(Error::Unsupported(format!(
                "unsupported pure expression in entrypoint body: {term:?}"
            )))
        }
        Term::IntegerLiteral(_) | Term::Application { .. } | Term::MethodCall { .. } => {
            reject_io_expression_builtin_without_effect(term, state)?;
            if let Some(value) = lower_reference_get_value(term, state)? {
                return Ok(value);
            }
            if let Term::Application { callee, arguments } = term
                && let Some(value) =
                    lower_constructor_application(callee.as_ref(), arguments, state, program)?
            {
                return Ok(value);
            }
            if let Some(value) = lower_numeric_conversion_value(term, state, program)? {
                return Ok(value);
            }
            if let Some(value) = lower_i32_primitive_value(term, state, program)? {
                return Ok(value);
            }
            if let Some(value) = lower_f32_primitive_value(term, state, program)? {
                return Ok(value);
            }
            if let Some(value) = lower_pure_function_call(term, state, program)? {
                return Ok(value);
            }
            if let Some(value) = lower_io_function_call_value(term, state, program)? {
                return Ok(value);
            }
            if let Ok(condition) = super::lower_bool_expr(term, state) {
                return Ok(Value::Bool(condition));
            }
            if let Ok(expr) = super::lower_i32_expr(term, state) {
                return Ok(Value::I32(expr));
            }
            if let Ok(expr) = super::lower_i64_expr(term, state) {
                return Ok(Value::I64(expr));
            }
            if let Ok(expr) = super::lower_f32_expr(term, state) {
                return Ok(Value::F32(expr));
            }
            if let Ok(expr) = super::lower_u8_expr(term, state) {
                return Ok(Value::U8(expr));
            }
            Err(Error::Unsupported(format!(
                "unsupported pure expression in entrypoint body: {term:?}"
            )))
        }
        Term::FieldAccess { .. } => resolve_value(term, &state.environment),
        Term::Path(path) => lower_path_value(path, state, program),
        _ => Err(Error::Unsupported(format!(
            "unsupported pure expression in entrypoint body: {term:?}"
        ))),
    }
}

fn reject_io_expression_builtin_without_effect(term: &Term, state: &LoweringState) -> Result<()> {
    let Term::Application { callee, .. } = term else {
        return Ok(());
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(());
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return Ok(());
    }
    match path.segments[0].lexeme.as_str() {
        "ref_get" => ensure_io_effect_allowed(state, "ref_get"),
        "array_get" => ensure_io_effect_allowed(state, "array_get"),
        "array_len" => ensure_io_effect_allowed(state, "array_len"),
        _ => Ok(()),
    }
}

fn lower_numeric_conversion_value(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    let Some(primitive) = simple_builtin_path_name(
        path,
        state,
        "numeric conversion",
        &[
            "i32_from_u8",
            "i32_from_i64",
            "i64_from_i32",
            "i64_from_u8",
            "f32_from_i32",
            "f32_from_i64",
            "f32_from_u8",
            "u8_from_i32",
            "u8_from_i64",
            "u8_from_f32",
            "i32_from_f32",
            "i64_from_f32",
        ],
    )?
    else {
        return Ok(None);
    };
    if !matches!(
        primitive.as_str(),
        "i32_from_u8"
            | "i32_from_i64"
            | "i64_from_i32"
            | "i64_from_u8"
            | "f32_from_i32"
            | "f32_from_i64"
            | "f32_from_u8"
            | "u8_from_i32"
            | "u8_from_i64"
            | "u8_from_f32"
            | "i32_from_f32"
            | "i64_from_f32"
    ) {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [value] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{primitive}` must receive exactly one argument"
        )));
    };

    Ok(Some(match primitive.as_str() {
        "i32_from_u8" => Value::I32(I32Expr::FromU8(Box::new(lower_pure_u8_argument(
            value, state, program,
        )?))),
        "i32_from_i64" => Value::I32(I32Expr::FromI64(Box::new(lower_pure_i64_argument(
            value, state, program,
        )?))),
        "i32_from_f32" => Value::I32(I32Expr::FromF32(Box::new(lower_pure_f32_argument(
            value, state, program,
        )?))),
        "i64_from_i32" => Value::I64(I64Expr::FromI32(Box::new(lower_pure_i32_argument(
            value, state, program,
        )?))),
        "i64_from_u8" => Value::I64(I64Expr::FromU8(Box::new(lower_pure_u8_argument(
            value, state, program,
        )?))),
        "i64_from_f32" => Value::I64(I64Expr::FromF32(Box::new(lower_pure_f32_argument(
            value, state, program,
        )?))),
        "f32_from_i32" => Value::F32(F32Expr::FromI32(Box::new(lower_pure_i32_argument(
            value, state, program,
        )?))),
        "f32_from_i64" => Value::F32(F32Expr::FromI64(Box::new(lower_pure_i64_argument(
            value, state, program,
        )?))),
        "f32_from_u8" => Value::F32(F32Expr::FromU8(Box::new(lower_pure_u8_argument(
            value, state, program,
        )?))),
        "u8_from_i32" => Value::U8(U8Expr::FromI32(Box::new(lower_pure_i32_argument(
            value, state, program,
        )?))),
        "u8_from_i64" => Value::U8(U8Expr::FromI64(Box::new(lower_pure_i64_argument(
            value, state, program,
        )?))),
        "u8_from_f32" => Value::U8(U8Expr::FromF32(Box::new(lower_pure_f32_argument(
            value, state, program,
        )?))),
        _ => unreachable!("checked above"),
    }))
}

fn lower_f32_primitive_value(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    let Some(primitive) = simple_builtin_path_name(
        path,
        state,
        "f32 primitive",
        &["f32_add", "f32_sub", "f32_mul", "f32_div", "f32_sqrt"],
    )?
    else {
        return Ok(None);
    };
    if !matches!(
        primitive.as_str(),
        "f32_add" | "f32_sub" | "f32_mul" | "f32_div" | "f32_sqrt"
    ) {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    if primitive == "f32_sqrt" {
        let [value] = normalized.as_slice() else {
            return Err(Error::Unsupported(format!(
                "`{primitive}` must receive exactly one argument"
            )));
        };
        return Ok(Some(Value::F32(F32Expr::Sqrt(Box::new(
            lower_pure_f32_argument(value, state, program)?,
        )))));
    }

    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{primitive}` must receive exactly two arguments"
        )));
    };
    let lhs = Box::new(lower_pure_f32_argument(lhs, state, program)?);
    let rhs = Box::new(lower_pure_f32_argument(rhs, state, program)?);

    Ok(Some(Value::F32(match primitive.as_str() {
        "f32_add" => F32Expr::Add(lhs, rhs),
        "f32_sub" => F32Expr::Sub(lhs, rhs),
        "f32_mul" => F32Expr::Mul(lhs, rhs),
        "f32_div" => F32Expr::Div(lhs, rhs),
        _ => unreachable!("checked above"),
    })))
}

fn lower_i32_primitive_value(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    let Some(primitive) = simple_builtin_path_name(
        path,
        state,
        "i32 primitive",
        &[
            "i32_add", "i32_sub", "i32_mul", "i32_div", "i32_mod", "i32_xor", "i32_shl", "i32_shr",
        ],
    )?
    else {
        return Ok(None);
    };
    if !matches!(
        primitive.as_str(),
        "i32_add"
            | "i32_sub"
            | "i32_mul"
            | "i32_div"
            | "i32_mod"
            | "i32_xor"
            | "i32_shl"
            | "i32_shr"
    ) {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{primitive}` must receive exactly two arguments"
        )));
    };
    let lhs = Box::new(lower_pure_i32_argument(lhs, state, program)?);
    let rhs = Box::new(lower_pure_i32_argument(rhs, state, program)?);

    Ok(Some(Value::I32(match primitive.as_str() {
        "i32_add" => I32Expr::Add(lhs, rhs),
        "i32_sub" => I32Expr::Sub(lhs, rhs),
        "i32_mul" => I32Expr::Mul(lhs, rhs),
        "i32_div" => I32Expr::Div(lhs, rhs),
        "i32_mod" => I32Expr::Mod(lhs, rhs),
        "i32_xor" => I32Expr::Xor(lhs, rhs),
        "i32_shl" => I32Expr::Shl(lhs, rhs),
        "i32_shr" => I32Expr::Shr(lhs, rhs),
        _ => unreachable!("checked above"),
    })))
}

fn lower_pure_i32_argument(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<I32Expr> {
    match lower_pure_value(term, state, program)? {
        Value::I32(expr) => Ok(expr),
        other => Err(Error::Unsupported(format!(
            "expected an `i32` value, got {other:?}"
        ))),
    }
}

fn lower_pure_i64_argument(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<I64Expr> {
    match lower_pure_value(term, state, program)? {
        Value::I64(expr) => Ok(expr),
        other => Err(Error::Unsupported(format!(
            "expected an `i64` value, got {other:?}"
        ))),
    }
}

fn lower_pure_u8_argument(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<U8Expr> {
    match lower_pure_value(term, state, program)? {
        Value::U8(expr) => Ok(expr),
        other => Err(Error::Unsupported(format!(
            "expected a `u8` value, got {other:?}"
        ))),
    }
}

fn lower_pure_f32_argument(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<F32Expr> {
    match lower_pure_value(term, state, program)? {
        Value::F32(expr) => Ok(expr),
        other => Err(Error::Unsupported(format!(
            "expected an `f32` value, got {other:?}"
        ))),
    }
}

fn lower_reference_get_value(term: &Term, state: &LoweringState) -> Result<Option<Value>> {
    match term {
        Term::Application { callee, arguments } => {
            if let Term::Path(path) = callee.as_ref()
                && path.token_keyword_package.is_none()
                && path.segments.len() == 1
                && path.segments[0].lexeme == "ref_get"
            {
                let normalized = normalize_numeric_literal_arguments(arguments);
                let [_ty, receiver] = normalized.as_slice() else {
                    return Err(Error::Unsupported(
                        "`ref_get` must receive exactly a type and a reference".to_string(),
                    ));
                };
                return lower_reference_get_receiver_value(receiver, state).map(Some);
            }
            Ok(None)
        }
        _ => Ok(None),
    }
}

fn lower_reference_get_receiver_value(receiver: &Term, state: &LoweringState) -> Result<Value> {
    ensure_io_effect_allowed(state, "ref_get")?;
    let value = resolve_value(receiver, &state.environment)?;
    Ok(match value {
        Value::I32Reference { slot, .. } => Value::I32(I32Expr::Local(slot)),
        Value::I64Reference { slot, .. } => Value::I64(I64Expr::Local(slot)),
        Value::F32Reference { slot, .. } => Value::F32(F32Expr::Local(slot)),
        Value::U8Reference { slot, .. } => Value::U8(U8Expr::Local(slot)),
        Value::BoolReference { slot, .. } => Value::Bool(ConditionExpr::Local(slot)),
        Value::Reference { value, .. } => *value,
        other => other,
    })
}

fn lower_path_value(
    path: &PathExpression,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    if path.token_keyword_package.is_none() && path.segments.len() == 1 {
        let name = path.segments[0].lexeme.as_str();
        if let Some(value) = state.environment.get(name) {
            return Ok(value.clone());
        }
        match name {
            "true" => return Ok(Value::Bool(crate::ir::ConditionExpr::Literal(true))),
            "false" => return Ok(Value::Bool(crate::ir::ConditionExpr::Literal(false))),
            _ => {}
        }
        return Err(Error::Unsupported(format!(
            "unknown entrypoint local `{name}`"
        )));
    }

    let signature = lower_constructor_value(path, &state.constructors)?;
    if !signature.parameters.is_empty() {
        return Err(Error::Unsupported(format!(
            "constructor `{}::{}` requires {} arguments",
            signature.type_name,
            signature.constructor_name,
            signature.parameters.len()
        )));
    }
    if signature.is_rc {
        let heap_slot = allocate_heap_slot(program);
        program.operations.push(Operation::Mmap {
            len: I32Expr::Literal(8),
            result_slot: heap_slot,
        });
        program.operations.push(Operation::HeapStoreI32 {
            heap_slot,
            byte_offset: 0,
            value: I32Expr::Literal(signature.tag),
        });
        program.operations.push(Operation::HeapStoreI32 {
            heap_slot,
            byte_offset: 4,
            value: I32Expr::Literal(1),
        });
        return Ok(Value::Constructor(ConstructorValue {
            type_name: signature.type_name,
            constructor_name: signature.constructor_name,
            heap_slot: Some(heap_slot),
            runtime_tag: false,
            fields: Vec::new(),
        }));
    }

    Ok(Value::Constructor(ConstructorValue {
        type_name: signature.type_name,
        constructor_name: signature.constructor_name,
        heap_slot: None,
        runtime_tag: false,
        fields: Vec::new(),
    }))
}

fn lower_struct_literal_value(
    path: &PathExpression,
    fields: &[neco_rs_parser::StructLiteralField],
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return Err(Error::Unsupported(
            "only simple struct literal paths are supported in entrypoint lowering".to_string(),
        ));
    }

    let type_name = path.segments[0].lexeme.as_str();
    let Some(signature) = state.structs.get(type_name) else {
        return Err(Error::Unsupported(format!("unknown struct `{type_name}`")));
    };
    let mut values = HashMap::new();
    for field in fields {
        if values.contains_key(&field.name) {
            return Err(Error::Unsupported(format!(
                "duplicate field `{}` in struct literal `{type_name}`",
                field.name
            )));
        }
        values.insert(
            field.name.clone(),
            lower_pure_value(&field.value, state, program)?,
        );
    }

    let mut struct_fields = Vec::with_capacity(signature.fields.len());
    for field in &signature.fields {
        let Some(value) = values.remove(&field.name) else {
            return Err(Error::Unsupported(format!(
                "missing field `{}` in struct literal `{type_name}`",
                field.name
            )));
        };
        validate_value_against_type(&value, &field.ty, program)?;
        struct_fields.push(StructFieldValue {
            name: field.name.clone(),
            value,
        });
    }
    if let Some(extra) = values.keys().next() {
        return Err(Error::Unsupported(format!(
            "unknown field `{extra}` in struct literal `{type_name}`"
        )));
    }

    let heap_slot = if signature.is_rc {
        Some(lower_rc_struct_allocation(&struct_fields, program)?)
    } else {
        None
    };

    Ok(Value::Struct(StructValue {
        type_name: signature.type_name.clone(),
        heap_slot,
        fields: struct_fields,
    }))
}

fn lower_rc_struct_allocation(
    fields: &[StructFieldValue],
    program: &mut LoweredProgram,
) -> Result<usize> {
    let heap_slot = allocate_heap_slot(program);
    let object_len = 8 + fields
        .iter()
        .map(|field| rc_struct_field_size(&field.value))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .sum::<i32>();

    program.operations.push(Operation::Mmap {
        len: I32Expr::Literal(object_len),
        result_slot: heap_slot,
    });
    program.operations.push(Operation::HeapStoreI32 {
        heap_slot,
        byte_offset: 0,
        value: I32Expr::Literal(0),
    });
    program.operations.push(Operation::HeapStoreI32 {
        heap_slot,
        byte_offset: 4,
        value: I32Expr::Literal(1),
    });

    let mut byte_offset = 8;
    for field in fields {
        lower_rc_struct_field_store(heap_slot, byte_offset, &field.value, program)?;
        byte_offset += rc_struct_field_size(&field.value)?;
    }

    Ok(heap_slot)
}

fn rc_struct_field_size(value: &Value) -> Result<i32> {
    match value {
        Value::I32(_) => Ok(4),
        Value::I64(_) => Ok(8),
        Value::Constructor(ConstructorValue {
            heap_slot: Some(_), ..
        })
        | Value::Struct(StructValue {
            heap_slot: Some(_), ..
        }) => Ok(8),
        _ => Err(Error::Unsupported(
            "`struct(rc)` currently supports only integer and nested rc payload fields".to_string(),
        )),
    }
}

fn lower_rc_struct_field_store(
    heap_slot: usize,
    byte_offset: i32,
    value: &Value,
    program: &mut LoweredProgram,
) -> Result<()> {
    match value {
        Value::I32(value) => {
            program.operations.push(Operation::HeapStoreI32 {
                heap_slot,
                byte_offset,
                value: value.clone(),
            });
            Ok(())
        }
        Value::I64(value) => {
            program.operations.push(Operation::HeapStoreI64 {
                heap_slot,
                byte_offset,
                value: value.clone(),
            });
            Ok(())
        }
        Value::F32(_) => Err(Error::Unsupported(
            "`struct(rc)` currently supports only integer and nested rc payload fields".to_string(),
        )),
        Value::Constructor(ConstructorValue {
            heap_slot: Some(source_heap_slot),
            ..
        })
        | Value::Struct(StructValue {
            heap_slot: Some(source_heap_slot),
            ..
        }) => {
            retain_rc_heap_slot(*source_heap_slot, program);
            program.operations.push(Operation::HeapStorePtr {
                heap_slot,
                byte_offset,
                source_heap_slot: *source_heap_slot,
            });
            Ok(())
        }
        _ => Err(Error::Unsupported(
            "`struct(rc)` currently supports only integer and nested rc payload fields".to_string(),
        )),
    }
}

fn retain_rc_heap_slot(heap_slot: usize, program: &mut LoweredProgram) {
    program.operations.push(Operation::HeapAddI32 {
        heap_slot,
        byte_offset: 4,
        value: 1,
    });
}

pub(super) fn lower_constructor_value(
    path: &PathExpression,
    constructors: &HashMap<String, ConstructorSignature>,
) -> Result<ConstructorSignature> {
    if path.token_keyword_package.is_some() {
        return Err(Error::Unsupported(
            "package-qualified constructor paths are not supported in entrypoint lowering"
                .to_string(),
        ));
    }
    if path.segments.len() != 2 {
        return Err(Error::Unsupported(
            "only simple `Type::constructor` paths are supported in entrypoint lowering"
                .to_string(),
        ));
    }

    let key = constructor_key(&path.segments[0].lexeme, &path.segments[1].lexeme);
    constructors
        .get(&key)
        .cloned()
        .ok_or_else(|| Error::Unsupported(format!("unknown constructor `{key}`")))
}

fn lower_match_value(
    match_expr: &MatchExpression,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    let scrutinee = lower_pure_value(match_expr.scrutinee.as_ref(), state, program)?;
    if let Value::Constructor(constructor) = &scrutinee
        && constructor.runtime_tag
        && let Some(heap_slot) = constructor.heap_slot
    {
        return lower_dynamic_rc_match_value(match_expr, constructor, heap_slot, state, program);
    }
    for arm in &match_expr.arms {
        if let Some(bindings) =
            pattern_match_bindings(&arm.pattern, &scrutinee, &state.constructors)?
        {
            let mut scoped_state = state.child_scope();
            scoped_state.environment.extend(bindings);
            return lower_pure_value(arm.result.as_ref(), &scoped_state, program);
        }
    }

    Err(Error::Unsupported(
        "`#match` did not match any constructor arm".to_string(),
    ))
}

fn lower_dynamic_rc_match_value(
    match_expr: &MatchExpression,
    constructor: &ConstructorValue,
    heap_slot: usize,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    let result_heap_slot = allocate_heap_slot(program);

    let mut arms = Vec::new();
    for arm in &match_expr.arms {
        let mut arm_program = program.clone();
        let arm_operation_start = arm_program.operations.len();
        let (tag, bindings) = dynamic_rc_constructor_pattern_bindings(
            &arm.pattern,
            constructor,
            heap_slot,
            state,
            &mut arm_program,
        )?;
        let mut scoped_state = state.child_scope();
        scoped_state.environment.extend(bindings);
        let value = lower_pure_value(arm.result.as_ref(), &scoped_state, &mut arm_program)?;
        let result = DynamicRcMatchArmResult::from_value(value)?;
        let mut arm_operations = arm_program.operations.split_off(arm_operation_start);
        let internal_functions = take_internal_functions(&mut arm_operations);
        program.data = arm_program.data;
        program.compiled_ptx = arm_program.compiled_ptx;
        program.arrays = arm_program.arrays;
        program.heap_slots = program.heap_slots.max(arm_program.heap_slots);
        program.i32_slots = program.i32_slots.max(arm_program.i32_slots);
        program.i64_slots = program.i64_slots.max(arm_program.i64_slots);
        program.f32_slots = program.f32_slots.max(arm_program.f32_slots);
        program.u8_slots = program.u8_slots.max(arm_program.u8_slots);
        program.bool_slots = program.bool_slots.max(arm_program.bool_slots);
        program.requires_argv |= arm_program.requires_argv;
        program.operations.extend(internal_functions);
        arms.push((tag, arm_operations, result));
    }

    let Some(first_result) = arms.first().map(|(_, _, result)| result.clone()) else {
        return Err(Error::Unsupported(
            "`#match` must contain at least one constructor arm".to_string(),
        ));
    };
    first_result.prepare_result_slot(result_heap_slot, program)?;
    for (_, _, result) in &arms {
        first_result.validate_same_kind(result)?;
    }

    let Some((_, mut else_operations, else_result)) = arms.pop() else {
        unreachable!("empty arms handled above");
    };
    else_result.push_store(result_heap_slot, &mut else_operations);
    while let Some((tag, mut then_operations, then_result)) = arms.pop() {
        then_result.push_store(result_heap_slot, &mut then_operations);
        else_operations = vec![Operation::If {
            condition: ConditionExpr::I32 {
                kind: ComparisonKind::Eq,
                lhs: I32Expr::HeapLoadI32 {
                    heap_slot,
                    byte_offset: 0,
                },
                rhs: I32Expr::Literal(tag),
            },
            then_operations,
            else_operations,
        }];
    }
    program.operations.extend(else_operations);
    first_result.result_value(result_heap_slot)
}

fn take_internal_functions(operations: &mut Vec<Operation>) -> Vec<Operation> {
    let mut internal_functions = Vec::new();
    let mut retained = Vec::with_capacity(operations.len());
    for operation in operations.drain(..) {
        match operation {
            Operation::InternalFunction { .. } => internal_functions.push(operation),
            operation => retained.push(operation),
        }
    }
    *operations = retained;
    internal_functions
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DynamicRcMatchArmResult {
    I32(I32Expr),
    Constructor { type_name: String, heap_slot: usize },
}

impl DynamicRcMatchArmResult {
    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::I32(expr) => Ok(Self::I32(expr)),
            Value::Constructor(ConstructorValue {
                type_name,
                heap_slot: Some(heap_slot),
                ..
            }) => Ok(Self::Constructor {
                type_name,
                heap_slot,
            }),
            _ => Err(Error::Unsupported(
                "runtime `type(rc)` matches currently support only `i32` and `type(rc)` result expressions"
                    .to_string(),
            )),
        }
    }

    fn prepare_result_slot(
        &self,
        result_heap_slot: usize,
        program: &mut LoweredProgram,
    ) -> Result<()> {
        if matches!(self, Self::I32(_)) {
            program.operations.push(Operation::Mmap {
                len: I32Expr::Literal(4),
                result_slot: result_heap_slot,
            });
        }
        Ok(())
    }

    fn validate_same_kind(&self, other: &Self) -> Result<()> {
        match (self, other) {
            (Self::I32(_), Self::I32(_)) => Ok(()),
            (
                Self::Constructor { type_name, .. },
                Self::Constructor {
                    type_name: other_type,
                    ..
                },
            ) if type_name == other_type => Ok(()),
            _ => Err(Error::Unsupported(
                "runtime `type(rc)` match arms must return the same result kind".to_string(),
            )),
        }
    }

    fn push_store(&self, result_heap_slot: usize, operations: &mut Vec<Operation>) {
        match self {
            Self::I32(expr) => operations.push(Operation::HeapStoreI32 {
                heap_slot: result_heap_slot,
                byte_offset: 0,
                value: expr.clone(),
            }),
            Self::Constructor { heap_slot, .. } => operations.push(Operation::HeapSlotReplace {
                dest_heap_slot: result_heap_slot,
                source_heap_slot: *heap_slot,
            }),
        }
    }

    fn result_value(&self, result_heap_slot: usize) -> Result<Value> {
        match self {
            Self::I32(_) => Ok(Value::I32(I32Expr::HeapLoadI32 {
                heap_slot: result_heap_slot,
                byte_offset: 0,
            })),
            Self::Constructor { type_name, .. } => Ok(Value::Constructor(ConstructorValue {
                type_name: type_name.clone(),
                constructor_name: String::new(),
                heap_slot: Some(result_heap_slot),
                runtime_tag: true,
                fields: Vec::new(),
            })),
        }
    }
}

fn dynamic_rc_constructor_pattern_bindings(
    pattern: &Pattern,
    constructor: &ConstructorValue,
    heap_slot: usize,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<(i32, HashMap<String, Value>)> {
    let Pattern::Constructor { path, subpatterns } = pattern else {
        return Err(Error::Unsupported(
            "runtime `type(rc)` matches require constructor arms".to_string(),
        ));
    };
    let expected = lower_constructor_value(path, &state.constructors)?;
    if !expected.is_rc || expected.type_name != constructor.type_name {
        return Err(Error::Unsupported(format!(
            "runtime match expected a `{}` constructor arm",
            constructor.type_name
        )));
    }

    let mut bindings = HashMap::new();
    let mut byte_offset = 8;
    let mut subpattern_iter = subpatterns.iter();
    for parameter in &expected.parameters {
        let Some(subpattern) = subpattern_iter.next() else {
            return Err(Error::Unsupported(format!(
                "constructor `{}::{}` pattern has too few fields",
                expected.type_name, expected.constructor_name
            )));
        };
        if is_type_universe_annotation(&parameter.ty) {
            if !matches!(subpattern, Pattern::Wildcard) {
                return Err(Error::Unsupported(
                    "constructor type parameters in match patterns currently support only `_`"
                        .to_string(),
                ));
            }
            continue;
        }
        let value = dynamic_rc_field_value(&parameter.ty, heap_slot, byte_offset, state, program)?;
        byte_offset += dynamic_rc_field_size(&parameter.ty, state)?;
        match subpattern {
            Pattern::Wildcard => {}
            Pattern::Bind(name) => {
                bindings.insert(name.clone(), value);
            }
            Pattern::Constructor { .. } => {
                return Err(Error::Unsupported(
                    "nested runtime constructor patterns are not supported".to_string(),
                ));
            }
        }
    }
    if subpattern_iter.next().is_some() {
        return Err(Error::Unsupported(format!(
            "constructor `{}::{}` pattern has too many fields",
            expected.type_name, expected.constructor_name
        )));
    }
    Ok((expected.tag, bindings))
}

fn dynamic_rc_field_value(
    ty: &Term,
    heap_slot: usize,
    byte_offset: i32,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    if is_simple_term_path(ty, "i32") {
        return Ok(Value::I32(I32Expr::HeapLoadI32 {
            heap_slot,
            byte_offset,
        }));
    }
    if let Some(type_name) = runtime_rc_type_name(ty, state) {
        let field_heap_slot = allocate_heap_slot(program);
        program.operations.push(Operation::HeapLoadPtr {
            dest_heap_slot: field_heap_slot,
            source_heap_slot: heap_slot,
            byte_offset,
        });
        return Ok(Value::Constructor(ConstructorValue {
            type_name,
            constructor_name: String::new(),
            heap_slot: Some(field_heap_slot),
            runtime_tag: true,
            fields: Vec::new(),
        }));
    }
    Err(Error::Unsupported(
        "runtime `type(rc)` matches currently support only `i32` payload fields".to_string(),
    ))
}

fn dynamic_rc_field_size(ty: &Term, state: &LoweringState) -> Result<i32> {
    if is_simple_term_path(ty, "i32") {
        return Ok(4);
    }
    if runtime_rc_type_name(ty, state).is_some() {
        return Ok(8);
    }
    Err(Error::Unsupported(
        "runtime `type(rc)` matches currently support only `i32` payload fields".to_string(),
    ))
}

fn runtime_rc_type_name(ty: &Term, state: &LoweringState) -> Option<String> {
    let type_name = simple_nominal_type_name(ty)?;
    state
        .constructors
        .values()
        .any(|constructor| constructor.is_rc && constructor.type_name == type_name)
        .then(|| type_name.to_string())
}

fn simple_nominal_type_name(term: &Term) -> Option<&str> {
    match term {
        Term::Path(path)
            if path.token_keyword_package.is_none()
                && path.segments.len() == 1
                && !is_simple_term_path(term, "i32") =>
        {
            Some(path.segments[0].lexeme.as_str())
        }
        _ => None,
    }
}

fn is_simple_term_path(term: &Term, expected: &str) -> bool {
    matches!(
        term,
        Term::Path(path)
            if path.token_keyword_package.is_none()
                && path.segments.len() == 1
                && path.segments[0].lexeme == expected
    )
}

pub(super) fn pattern_match_bindings(
    pattern: &Pattern,
    value: &Value,
    constructors: &HashMap<String, ConstructorSignature>,
) -> Result<Option<HashMap<String, Value>>> {
    let (value, bind_as_reference, exclusive) = match value {
        Value::Reference { value, exclusive } => (value.as_ref(), true, *exclusive),
        _ => (value, false, false),
    };
    pattern_match_bindings_with_mode(pattern, value, constructors, bind_as_reference, exclusive)
}

fn pattern_match_bindings_with_mode(
    pattern: &Pattern,
    value: &Value,
    constructors: &HashMap<String, ConstructorSignature>,
    bind_as_reference: bool,
    exclusive: bool,
) -> Result<Option<HashMap<String, Value>>> {
    match pattern {
        Pattern::Wildcard => Ok(Some(HashMap::new())),
        Pattern::Bind(name) => {
            let mut bindings = HashMap::new();
            bindings.insert(
                name.clone(),
                if bind_as_reference {
                    Value::Reference {
                        value: Box::new(value.clone()),
                        exclusive,
                    }
                } else {
                    value.clone()
                },
            );
            Ok(Some(bindings))
        }
        Pattern::Constructor { path, subpatterns } => {
            let expected = lower_constructor_value(path, constructors)?;
            let Value::Constructor(actual) = value else {
                return Ok(None);
            };

            if actual.type_name != expected.type_name
                || actual.constructor_name != expected.constructor_name
            {
                return Ok(None);
            }

            let mut bindings = HashMap::new();
            let mut subpattern_iter = subpatterns.iter();
            let mut field_iter = actual.fields.iter();
            for parameter in &expected.parameters {
                if is_type_universe_annotation(&parameter.ty) {
                    let Some(subpattern) = subpattern_iter.next() else {
                        return Ok(None);
                    };
                    if !matches!(subpattern, Pattern::Wildcard) {
                        return Err(Error::Unsupported(
                            "constructor type parameters in match patterns currently support only `_`"
                                .to_string(),
                        ));
                    }
                    continue;
                }

                let Some(subpattern) = subpattern_iter.next() else {
                    return Ok(None);
                };
                let Some(field) = field_iter.next() else {
                    return Ok(None);
                };
                let _ = actual.heap_slot;
                let field_value: Value = field.clone();
                let Some(sub_bindings) = pattern_match_bindings_with_mode(
                    subpattern,
                    &field_value,
                    constructors,
                    bind_as_reference,
                    exclusive,
                )?
                else {
                    return Ok(None);
                };
                bindings.extend(sub_bindings);
            }
            if subpattern_iter.next().is_some() || field_iter.next().is_some() {
                return Ok(None);
            }
            Ok(Some(bindings))
        }
    }
}

fn lower_constructor_application(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    let signature = match lower_constructor_value(path, &state.constructors) {
        Ok(signature) => signature,
        Err(Error::Unsupported(_)) => return Ok(None),
        Err(error) => return Err(error),
    };

    let normalized_arguments = normalize_numeric_literal_arguments(arguments);
    if signature.parameters.len() != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "constructor `{}::{}` must receive exactly {} arguments",
            signature.type_name,
            signature.constructor_name,
            signature.parameters.len()
        )));
    }

    let mut fields = Vec::with_capacity(normalized_arguments.len());
    let mut type_bindings = HashMap::new();
    for (parameter, argument) in signature.parameters.iter().zip(normalized_arguments.iter()) {
        let parameter_ty = substitute_type_bindings(&parameter.ty, &type_bindings);
        let value = if is_type_universe_annotation(&parameter_ty) {
            Value::Type(resolve_type_argument(argument, state))
        } else {
            lower_pure_value(argument, state, program)?
        };
        validate_value_against_type(&value, &parameter_ty, program)?;
        if let (Some(name), Value::Type(ty)) = (&parameter.name, &value) {
            type_bindings.insert(name.clone(), ty.clone());
        }
        if !matches!(value, Value::Type(_)) {
            fields.push(value);
        }
    }

    if signature.is_rc {
        let heap_slot = allocate_heap_slot(program);
        let object_len = 8 + fields
            .iter()
            .map(|field| match field {
                Value::I32(_) => Ok(4),
                Value::I64(_) => Ok(8),
                Value::Constructor(ConstructorValue {
                    heap_slot: Some(_), ..
                })
                | Value::Struct(StructValue {
                    heap_slot: Some(_), ..
                }) => Ok(8),
                Value::Constructor(ConstructorValue {
                    heap_slot: None, ..
                })
                | Value::Struct(StructValue {
                    heap_slot: None, ..
                }) => Ok(0),
                _ => Err(Error::Unsupported(
                    "`type(rc)` currently supports only integer, nested `type(rc)`, and compile-time constructor payload fields"
                        .to_string(),
                )),
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .sum::<i32>();
        program.operations.push(Operation::Mmap {
            len: I32Expr::Literal(object_len),
            result_slot: heap_slot,
        });
        program.operations.push(Operation::HeapStoreI32 {
            heap_slot,
            byte_offset: 0,
            value: I32Expr::Literal(signature.tag),
        });
        program.operations.push(Operation::HeapStoreI32 {
            heap_slot,
            byte_offset: 4,
            value: I32Expr::Literal(1),
        });
        let mut byte_offset = 8;
        for field in &fields {
            match field {
                Value::I32(value) => {
                    program.operations.push(Operation::HeapStoreI32 {
                        heap_slot,
                        byte_offset,
                        value: value.clone(),
                    });
                    byte_offset += 4;
                }
                Value::I64(value) => {
                    program.operations.push(Operation::HeapStoreI64 {
                        heap_slot,
                        byte_offset,
                        value: value.clone(),
                    });
                    byte_offset += 8;
                }
                Value::Constructor(ConstructorValue {
                    heap_slot: Some(source_heap_slot),
                    ..
                })
                | Value::Struct(StructValue {
                    heap_slot: Some(source_heap_slot),
                    ..
                }) => {
                    retain_rc_heap_slot(*source_heap_slot, program);
                    program.operations.push(Operation::HeapStorePtr {
                        heap_slot,
                        byte_offset,
                        source_heap_slot: *source_heap_slot,
                    });
                    byte_offset += 8;
                }
                Value::Constructor(ConstructorValue {
                    heap_slot: None, ..
                })
                | Value::Struct(StructValue {
                    heap_slot: None, ..
                }) => {}
                _ => {
                    return Err(Error::Unsupported(
                        "`type(rc)` currently supports only integer, nested `type(rc)`, and compile-time constructor payload fields"
                            .to_string(),
                    ));
                }
            }
        }

        return Ok(Some(Value::Constructor(ConstructorValue {
            type_name: signature.type_name,
            constructor_name: signature.constructor_name,
            heap_slot: Some(heap_slot),
            runtime_tag: false,
            fields,
        })));
    }

    Ok(Some(Value::Constructor(ConstructorValue {
        type_name: signature.type_name,
        constructor_name: signature.constructor_name,
        heap_slot: None,
        runtime_tag: false,
        fields,
    })))
}

fn lower_pure_function_call(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return Ok(None);
    }

    let name = path.segments[0].lexeme.as_str();
    let Some(function) = state.functions.get(name).cloned() else {
        return Ok(None);
    };
    if state.environment.contains_key(name) {
        return Ok(None);
    }
    let normalized_arguments = normalize_numeric_literal_arguments(arguments);
    if function.parameters.len() != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "pure function `{name}` must receive exactly {} arguments",
            function.parameters.len()
        )));
    }

    let mut scoped_state = state.child_scope();
    scoped_state.io_effect_allowed = false;
    scoped_state.pure_function_call_stack.push(name.to_string());
    let mut type_bindings = HashMap::new();
    for (parameter, argument) in function.parameters.iter().zip(normalized_arguments.iter()) {
        let parameter_ty = substitute_type_bindings(&parameter.ty, &type_bindings);
        let value = if is_type_universe_annotation(&parameter_ty) {
            Value::Type(resolve_type_argument(argument, state))
        } else {
            lower_pure_value(argument, state, program)?
        };
        validate_value_against_type(&value, &parameter_ty, program)?;
        if let Value::Type(ty) = &value {
            type_bindings.insert(parameter.name.clone(), ty.clone());
        }
        let value = wrap_reference_parameter(value, &parameter_ty);
        scoped_state
            .environment
            .insert(parameter.name.clone(), value);
    }
    if state
        .pure_function_call_stack
        .iter()
        .any(|active| active == name)
        && !recursive_call_is_structurally_smaller(&function.parameters, state, &scoped_state)
    {
        return lower_runtime_pure_function_call(
            name,
            &function,
            &normalized_arguments,
            state,
            program,
            &type_bindings,
        );
    }

    let value = lower_pure_block_value(&function.body, &scoped_state, program)?;
    let result_ty = substitute_type_bindings(&function.result_ty, &type_bindings);
    validate_value_against_type(&value, &result_ty, program)?;
    Ok(Some(value))
}

fn lower_runtime_pure_function_call(
    name: &str,
    function: &super::declarations::PureFunction,
    normalized_arguments: &[Term],
    state: &LoweringState,
    program: &mut LoweredProgram,
    type_bindings: &HashMap<String, Term>,
) -> Result<Option<Value>> {
    let arguments = runtime_internal_call_arguments(
        &function.parameters,
        normalized_arguments,
        state,
        program,
    )?;
    ensure_internal_pure_function(name, function, state, program, type_bindings)?;

    let result_ty = substitute_type_bindings(&function.result_ty, type_bindings);
    let (result, value) = internal_result_slot(&result_ty, program)?;
    program.operations.push(Operation::CallInternal {
        name: name.to_string(),
        arguments,
        result: Some(result),
    });
    Ok(Some(value))
}

fn ensure_internal_pure_function(
    name: &str,
    function: &super::declarations::PureFunction,
    state: &LoweringState,
    program: &mut LoweredProgram,
    type_bindings: &HashMap<String, Term>,
) -> Result<()> {
    if program
        .operations
        .iter()
        .any(|operation| matches!(operation, Operation::InternalFunction { name: existing, .. } if existing == name))
    {
        return Ok(());
    }

    let placeholder_index = program.operations.len();
    program.operations.push(Operation::InternalFunction {
        name: name.to_string(),
        parameters: Vec::new(),
        body_operations: Vec::new(),
    });

    let mut scoped_state = state.child_scope();
    scoped_state.io_effect_allowed = false;
    scoped_state.pure_function_call_stack.push(name.to_string());
    let parameters = bind_internal_parameters(&function.parameters, &mut scoped_state, program)?;

    let operation_start = program.operations.len();
    let value = lower_pure_block_value(&function.body, &scoped_state, program)?;
    let result_ty = substitute_type_bindings(&function.result_ty, type_bindings);
    validate_value_against_type(&value, &result_ty, program)?;
    let return_value = internal_return_argument(&value)?;
    let mut body_operations = program.operations.split_off(operation_start);
    body_operations.push(Operation::ReturnInternal {
        value: Some(return_value),
    });
    program.operations[placeholder_index] = Operation::InternalFunction {
        name: name.to_string(),
        parameters,
        body_operations,
    };
    Ok(())
}

fn bind_internal_parameters(
    parameters: &[super::declarations::PureFunctionParameter],
    scoped_state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<Vec<InternalAbiValue>> {
    let mut lowered = Vec::with_capacity(parameters.len());
    for parameter in parameters {
        if is_type_universe_annotation(&parameter.ty) {
            return Err(Error::Unsupported(
                "runtime recursive pure functions with type parameters are not supported"
                    .to_string(),
            ));
        }
        let (abi, value) = internal_parameter_slot(&parameter.ty, program)?;
        scoped_state
            .environment
            .insert(parameter.name.clone(), value);
        lowered.push(abi);
    }
    Ok(lowered)
}

fn internal_parameter_slot(
    ty: &Term,
    program: &mut LoweredProgram,
) -> Result<(InternalAbiValue, Value)> {
    if is_simple_term_path(ty, "i32") {
        let slot = program.i32_slots;
        program.i32_slots += 1;
        return Ok((
            InternalAbiValue::I32(slot),
            Value::I32(I32Expr::Local(slot)),
        ));
    }
    if is_simple_term_path(ty, "i64") {
        let slot = program.i64_slots;
        program.i64_slots += 1;
        return Ok((
            InternalAbiValue::I64(slot),
            Value::I64(I64Expr::Local(slot)),
        ));
    }
    if is_simple_term_path(ty, "u8") {
        let slot = program.u8_slots;
        program.u8_slots += 1;
        return Ok((InternalAbiValue::U8(slot), Value::U8(U8Expr::Local(slot))));
    }
    if is_simple_term_path(ty, "bool") {
        let slot = program.bool_slots;
        program.bool_slots += 1;
        return Ok((
            InternalAbiValue::Bool(slot),
            Value::Bool(ConditionExpr::Local(slot)),
        ));
    }
    let heap_slot = allocate_heap_slot(program);
    Ok((
        InternalAbiValue::HeapPtr(heap_slot),
        Value::Constructor(ConstructorValue {
            type_name: simple_nominal_type_name(ty).unwrap_or("").to_string(),
            constructor_name: String::new(),
            heap_slot: Some(heap_slot),
            runtime_tag: true,
            fields: Vec::new(),
        }),
    ))
}

fn runtime_internal_call_arguments(
    parameters: &[super::declarations::PureFunctionParameter],
    normalized_arguments: &[Term],
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Vec<InternalCallArgument>> {
    parameters
        .iter()
        .zip(normalized_arguments.iter())
        .map(|(parameter, argument)| {
            if is_type_universe_annotation(&parameter.ty) {
                return Err(Error::Unsupported(
                    "runtime recursive pure functions with type parameters are not supported"
                        .to_string(),
                ));
            }
            let value = lower_pure_value(argument, state, program)?;
            validate_value_against_type(&value, &parameter.ty, program)?;
            internal_call_argument(&value)
        })
        .collect()
}

fn internal_call_argument(value: &Value) -> Result<InternalCallArgument> {
    match value {
        Value::I32(expr) => Ok(InternalCallArgument::I32(expr.clone())),
        Value::I64(expr) => Ok(InternalCallArgument::I64(expr.clone())),
        Value::U8(expr) => Ok(InternalCallArgument::U8(expr.clone())),
        Value::Bool(expr) => Ok(InternalCallArgument::Bool(expr.clone())),
        Value::Constructor(ConstructorValue {
            heap_slot: Some(slot),
            ..
        })
        | Value::Struct(StructValue {
            heap_slot: Some(slot),
            ..
        }) => Ok(InternalCallArgument::HeapPtr(*slot)),
        _ => Err(Error::Unsupported(
            "runtime recursive pure functions support only scalar and `type(rc)` arguments"
                .to_string(),
        )),
    }
}

fn internal_result_slot(
    ty: &Term,
    program: &mut LoweredProgram,
) -> Result<(InternalAbiValue, Value)> {
    if is_simple_term_path(ty, "i32") {
        let slot = program.i32_slots;
        program.i32_slots += 1;
        return Ok((
            InternalAbiValue::I32(slot),
            Value::I32(I32Expr::Local(slot)),
        ));
    }
    if is_simple_term_path(ty, "i64") {
        let slot = program.i64_slots;
        program.i64_slots += 1;
        return Ok((
            InternalAbiValue::I64(slot),
            Value::I64(I64Expr::Local(slot)),
        ));
    }
    if is_simple_term_path(ty, "u8") {
        let slot = program.u8_slots;
        program.u8_slots += 1;
        return Ok((InternalAbiValue::U8(slot), Value::U8(U8Expr::Local(slot))));
    }
    if is_simple_term_path(ty, "bool") {
        let slot = program.bool_slots;
        program.bool_slots += 1;
        return Ok((
            InternalAbiValue::Bool(slot),
            Value::Bool(ConditionExpr::Local(slot)),
        ));
    }
    let heap_slot = allocate_heap_slot(program);
    Ok((
        InternalAbiValue::HeapPtr(heap_slot),
        Value::Constructor(ConstructorValue {
            type_name: simple_nominal_type_name(ty).unwrap_or("").to_string(),
            constructor_name: String::new(),
            heap_slot: Some(heap_slot),
            runtime_tag: true,
            fields: Vec::new(),
        }),
    ))
}

fn internal_return_argument(value: &Value) -> Result<InternalCallArgument> {
    internal_call_argument(value)
}

fn recursive_call_is_structurally_smaller(
    parameters: &[super::declarations::PureFunctionParameter],
    caller_state: &LoweringState,
    callee_state: &LoweringState,
) -> bool {
    parameters.iter().any(|parameter| {
        let Some(caller_value) = caller_state.environment.get(&parameter.name) else {
            return false;
        };
        let Some(callee_value) = callee_state.environment.get(&parameter.name) else {
            return false;
        };
        match (
            finite_static_constructor_size(caller_value),
            finite_static_constructor_size(callee_value),
        ) {
            (Some(caller_size), Some(callee_size)) => callee_size < caller_size,
            _ => false,
        }
    })
}

fn finite_static_constructor_size(value: &Value) -> Option<usize> {
    match value {
        Value::Reference { value, .. } => finite_static_constructor_size(value),
        Value::Constructor(constructor) => {
            if constructor.runtime_tag {
                return None;
            }
            let mut size = 1;
            for field in &constructor.fields {
                size += finite_static_value_size(field)?;
            }
            Some(size)
        }
        Value::Struct(struct_value) => {
            let mut size = 1;
            for field in &struct_value.fields {
                size += finite_static_value_size(&field.value)?;
            }
            Some(size)
        }
        _ => None,
    }
}

fn finite_static_value_size(value: &Value) -> Option<usize> {
    match value {
        Value::I32(_)
        | Value::I64(_)
        | Value::F32(_)
        | Value::U8(_)
        | Value::Bool(_)
        | Value::Unit
        | Value::Type(_)
        | Value::StaticSlice { .. } => Some(1),
        Value::Reference { value, .. } => finite_static_value_size(value),
        Value::Constructor(_) | Value::Struct(_) => finite_static_constructor_size(value),
        _ => None,
    }
}

fn lower_io_function_call_value(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    if !state.io_effect_allowed {
        return Ok(None);
    }

    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return Ok(None);
    }

    let name = path.segments[0].lexeme.as_str();
    if state.environment.contains_key(name) {
        return Ok(None);
    }
    let Some(function) = state.statement_functions.get(name).cloned() else {
        return Ok(None);
    };
    if !statement_function_has_io_effect(&function) {
        return Ok(None);
    }

    let normalized_arguments = normalize_numeric_literal_arguments(arguments);
    if function.parameters.len() != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "function `{name}` must receive exactly {} arguments",
            function.parameters.len()
        )));
    }

    let mut scoped_state = state.child_scope();
    scoped_state.io_effect_allowed = true;
    let mut type_bindings = HashMap::new();
    for (parameter, argument) in function.parameters.iter().zip(normalized_arguments.iter()) {
        let parameter_ty = substitute_type_bindings(&parameter.ty, &type_bindings);
        let value = if is_type_universe_annotation(&parameter_ty) {
            Value::Type(resolve_type_argument(argument, state))
        } else {
            lower_pure_value(argument, state, program)?
        };
        validate_value_against_type(&value, &parameter_ty, program)?;
        if let Value::Type(ty) = &value {
            type_bindings.insert(parameter.name.clone(), ty.clone());
        }
        let value = wrap_reference_parameter(value, &parameter_ty);
        scoped_state
            .environment
            .insert(parameter.name.clone(), value);
    }

    let value = lower_pure_block_value(&function.body, &scoped_state, program)?;
    let result_ty = substitute_type_bindings(&function.result_ty, &type_bindings);
    validate_value_against_type(&value, &result_ty, program)?;
    Ok(Some(value))
}

fn resolve_type_argument(term: &Term, state: &LoweringState) -> Term {
    let Term::Path(path) = term else {
        return term.clone();
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return term.clone();
    }
    match state.environment.get(&path.segments[0].lexeme) {
        Some(Value::Type(bound)) => bound.clone(),
        _ => term.clone(),
    }
}

fn wrap_reference_parameter(value: Value, ty: &Term) -> Value {
    let Term::Reference { exclusive, .. } = ty else {
        return value;
    };
    match value {
        Value::I32Reference { .. }
        | Value::I64Reference { .. }
        | Value::F32Reference { .. }
        | Value::U8Reference { .. }
        | Value::BoolReference { .. }
        | Value::Reference { .. }
        | Value::StaticSlice { .. }
        | Value::RuntimeArg(_) => value,
        value => Value::Reference {
            value: Box::new(value),
            exclusive: *exclusive,
        },
    }
}

pub(super) fn substitute_type_bindings(term: &Term, type_bindings: &HashMap<String, Term>) -> Term {
    match term {
        Term::Path(path)
            if path.token_keyword_package.is_none()
                && path.segments.len() == 1
                && type_bindings.contains_key(&path.segments[0].lexeme) =>
        {
            type_bindings[&path.segments[0].lexeme].clone()
        }
        Term::Group(inner) => Term::Group(Box::new(substitute_type_bindings(inner, type_bindings))),
        Term::Application { callee, arguments } => Term::Application {
            callee: Box::new(substitute_type_bindings(callee, type_bindings)),
            arguments: arguments
                .iter()
                .map(|argument| substitute_type_bindings(argument, type_bindings))
                .collect(),
        },
        Term::Reference {
            referent,
            exclusive,
        } => Term::Reference {
            referent: Box::new(substitute_type_bindings(referent, type_bindings)),
            exclusive: *exclusive,
        },
        Term::Arrow(arrow) => Term::Arrow(ArrowTerm {
            parameter: match &arrow.parameter {
                ArrowParameter::Binder(binder) => ArrowParameter::Binder(TypedBinder {
                    name: binder.name.clone(),
                    ty: Box::new(substitute_type_bindings(&binder.ty, type_bindings)),
                }),
                ArrowParameter::Domain(domain) => ArrowParameter::Domain(Box::new(
                    substitute_type_bindings(domain, type_bindings),
                )),
            },
            result: Box::new(substitute_type_bindings(&arrow.result, type_bindings)),
        }),
        Term::Forall(forall) => Term::Forall(ForallTerm {
            binder: TypedBinder {
                name: forall.binder.name.clone(),
                ty: Box::new(substitute_type_bindings(&forall.binder.ty, type_bindings)),
            },
            body: Box::new(substitute_type_bindings(&forall.body, type_bindings)),
        }),
        _ => term.clone(),
    }
}

pub(super) fn lower_pure_block_value(
    block: &Block,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    let mut scoped_state = state.child_scope();

    for statement in &block.statements {
        match statement {
            Statement::Let(let_stmt)
                if let_stmt.operator == neco_rs_parser::LetOperator::Equals =>
            {
                let value = lower_pure_value(let_stmt.value.as_ref(), &scoped_state, program)?;
                let ty = substitute_type_bindings(
                    let_stmt.ty.as_ref(),
                    &type_bindings_from_environment(&scoped_state.environment),
                );
                validate_value_against_type(&value, &ty, program)?;
                let value = wrap_reference_parameter(value, &ty);
                bind_pattern(&let_stmt.binder, value, &mut scoped_state.environment);
            }
            Statement::Let(_) => {
                return Err(Error::Unsupported(
                    "effectful statements are not supported in pure function bodies".to_string(),
                ));
            }
            Statement::LetRef(_) => {
                return Err(Error::Unsupported(
                    "`#letref` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Expression(_) => {
                return Err(Error::Unsupported(
                    "expression statements are not supported in pure function bodies".to_string(),
                ));
            }
            Statement::If(_) => {
                return Err(Error::Unsupported(
                    "`#if` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Loop(_) => {
                return Err(Error::Unsupported(
                    "`#loop` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Break => {
                return Err(Error::Unsupported(
                    "`#break` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Continue => {
                return Err(Error::Unsupported(
                    "`#continue` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Item(_) => {
                return Err(Error::Unsupported(
                    "items inside pure function bodies are not supported".to_string(),
                ));
            }
        }
    }

    let Some(tail) = block.tail.as_deref() else {
        return Err(Error::Unsupported(
            "pure function bodies must end with a value expression".to_string(),
        ));
    };
    lower_pure_value(tail, &scoped_state, program)
}

fn type_bindings_from_environment(environment: &HashMap<String, Value>) -> HashMap<String, Term> {
    environment
        .iter()
        .filter_map(|(name, value)| match value {
            Value::Type(ty) => Some((name.clone(), ty.clone())),
            _ => None,
        })
        .collect()
}

pub(super) fn lower_function_call_statement(
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<bool>> {
    let Some((function, normalized_arguments, name)) = resolve_function_call(term, state)? else {
        return Ok(None);
    };
    ensure_function_effect_allowed(&function, name, state)?;

    let (mut scoped_state, _type_bindings) =
        bind_function_arguments(&function.parameters, &normalized_arguments, state, program)?;
    scoped_state.io_effect_allowed = statement_function_has_io_effect(&function);

    let terminated = lower_function_body_statements(&function.body, &mut scoped_state, program)?;
    propagate_reference_arguments(
        &function.parameters,
        &normalized_arguments,
        &scoped_state,
        state,
    );

    state.next_array_slot = state.next_array_slot.max(scoped_state.next_array_slot);
    state.next_i32_slot = state.next_i32_slot.max(scoped_state.next_i32_slot);
    state.next_i64_slot = state.next_i64_slot.max(scoped_state.next_i64_slot);
    state.next_f32_slot = state.next_f32_slot.max(scoped_state.next_f32_slot);

    Ok(Some(terminated))
}

pub(super) fn lower_function_call_value(
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Some((function, normalized_arguments, name)) = resolve_function_call(term, state)? else {
        return Ok(None);
    };
    ensure_function_effect_allowed(&function, name, state)?;

    let (mut scoped_state, type_bindings) =
        bind_function_arguments(&function.parameters, &normalized_arguments, state, program)?;
    scoped_state.io_effect_allowed = statement_function_has_io_effect(&function);

    let terminated = lower_function_body_statements(&function.body, &mut scoped_state, program)?;
    if terminated {
        return Err(Error::Unsupported(format!(
            "function `{name}` used as a value cannot terminate before returning"
        )));
    }

    let Some(tail) = function.body.tail.as_deref() else {
        return Err(Error::Unsupported(format!(
            "function `{name}` body must end with a value expression"
        )));
    };
    super::ensure_no_nested_io_effects(tail, &scoped_state)?;
    let value = lower_pure_value(tail, &scoped_state, program)?;
    let result_ty = substitute_type_bindings(&function.result_ty, &type_bindings);
    validate_value_against_type(&value, &result_ty, program)?;
    propagate_reference_arguments(
        &function.parameters,
        &normalized_arguments,
        &scoped_state,
        state,
    );

    state.next_array_slot = state.next_array_slot.max(scoped_state.next_array_slot);
    state.next_i32_slot = state.next_i32_slot.max(scoped_state.next_i32_slot);
    state.next_i64_slot = state.next_i64_slot.max(scoped_state.next_i64_slot);
    state.next_f32_slot = state.next_f32_slot.max(scoped_state.next_f32_slot);

    Ok(Some(value))
}

fn ensure_function_effect_allowed(
    function: &StatementFunction,
    name: &str,
    state: &LoweringState,
) -> Result<()> {
    if !statement_function_has_io_effect(function) || state.io_effect_allowed {
        return Ok(());
    }
    Err(Error::Unsupported(format!(
        "function `{name}` requires `#with IO`"
    )))
}

fn statement_function_has_io_effect(function: &StatementFunction) -> bool {
    function.effect.as_deref() == Some("IO")
}

fn resolve_function_call<'a>(
    term: &'a Term,
    state: &LoweringState,
) -> Result<Option<(StatementFunction, Vec<Term>, &'a str)>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return Ok(None);
    }

    let name = path.segments[0].lexeme.as_str();
    if state.environment.contains_key(name) {
        return Ok(None);
    }
    let Some(function) = state.statement_functions.get(name).cloned() else {
        return Ok(None);
    };

    let normalized_arguments = normalize_numeric_literal_arguments(arguments);
    if function.parameters.len() != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "function `{name}` must receive exactly {} arguments",
            function.parameters.len()
        )));
    }

    Ok(Some((function, normalized_arguments, name)))
}

fn bind_function_arguments(
    parameters: &[StatementFunctionParameter],
    normalized_arguments: &[Term],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<(LoweringState, HashMap<String, Term>)> {
    let mut scoped_state = state.child_scope();
    let mut type_bindings = HashMap::new();
    for (parameter, argument) in parameters.iter().zip(normalized_arguments.iter()) {
        let parameter_ty = substitute_type_bindings(&parameter.ty, &type_bindings);
        let value = if is_type_universe_annotation(&parameter_ty) {
            Value::Type(resolve_type_argument(argument, state))
        } else {
            lower_pure_value(argument, state, program)?
        };
        validate_value_against_type(&value, &parameter_ty, program)?;
        if let Value::Type(ty) = &value {
            type_bindings.insert(parameter.name.clone(), ty.clone());
        }
        let value = wrap_reference_parameter(value, &parameter_ty);
        scoped_state
            .environment
            .insert(parameter.name.clone(), value);
    }
    Ok((scoped_state, type_bindings))
}

fn propagate_reference_arguments(
    parameters: &[StatementFunctionParameter],
    normalized_arguments: &[Term],
    scoped_state: &LoweringState,
    caller_state: &mut LoweringState,
) {
    for (parameter, argument) in parameters.iter().zip(normalized_arguments.iter()) {
        if !matches!(parameter.ty, Term::Reference { .. }) {
            continue;
        }
        let Some(argument_name) = simple_local_name(argument) else {
            continue;
        };
        let Some(value) = scoped_state.environment.get(&parameter.name).cloned() else {
            continue;
        };
        match value {
            Value::Constructor(_) | Value::Struct(_) => {
                caller_state
                    .environment
                    .insert(argument_name.to_string(), value);
            }
            Value::Reference { value, exclusive }
                if matches!(value.as_ref(), Value::Constructor(_) | Value::Struct(_)) =>
            {
                let propagated = if matches!(
                    caller_state.environment.get(argument_name),
                    Some(Value::Reference { .. })
                ) {
                    Value::Reference { value, exclusive }
                } else {
                    *value
                };
                caller_state
                    .environment
                    .insert(argument_name.to_string(), propagated);
            }
            _ => {}
        }
    }
}

fn simple_local_name(term: &Term) -> Option<&str> {
    let Term::Path(path) = term else {
        return None;
    };
    if path.token_keyword_package.is_none() && path.segments.len() == 1 {
        Some(path.segments[0].lexeme.as_str())
    } else {
        None
    }
}

fn lower_function_body_statements(
    body: &Block,
    scoped_state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    let mut terminated = false;
    for statement in &body.statements {
        if terminated {
            return Err(Error::Unsupported(
                "statements after `IO::sys_exit` are not supported".to_string(),
            ));
        }
        terminated = lower_statement(statement, scoped_state, program)?;
    }
    Ok(terminated)
}
