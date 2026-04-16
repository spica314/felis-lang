use std::collections::HashMap;

use neco_rs_parser::{Block, MatchExpression, PathExpression, Pattern, Statement, Term};

use crate::effect::{Value, bind_pattern, resolve_value};
use crate::ir::{ConstructorValue, I32Expr, LoweredProgram, Operation, intern_data};
use crate::{Error, Result};

use super::declarations::{ConstructorSignature, constructor_key};
use super::typecheck::{nul_terminated_bytes, validate_value_against_type};
use super::{
    LoweringState, allocate_heap_slot, lower_statement, normalize_numeric_literal_arguments,
};

pub(super) fn lower_pure_value(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    match term {
        Term::Group(inner) => lower_pure_value(inner, state, program),
        Term::Block(block) => lower_pure_block_value(block, state, program),
        Term::Match(match_expr) => lower_match_value(match_expr, state, program),
        Term::StringLiteral(literal) => {
            let data_index = intern_data(program, nul_terminated_bytes(literal));
            Ok(Value::ByteString(data_index))
        }
        Term::CharLiteral(_) => {
            if let Ok(expr) = super::lower_u8_expr(term, state) {
                return Ok(Value::U8(expr));
            }
            Err(Error::Unsupported(format!(
                "unsupported pure expression in entrypoint body: {term:?}"
            )))
        }
        Term::MethodCall { receiver, method } if method == "as_bytes" => {
            match resolve_value(receiver.as_ref(), &state.environment)? {
                Value::ByteString(data_index) => Ok(Value::ByteString(data_index)),
                other => Err(Error::Unsupported(format!(
                    "`as_bytes` expects a string reference, got {other:?}"
                ))),
            }
        }
        Term::IntegerLiteral(_) | Term::Application { .. } | Term::MethodCall { .. } => {
            if let Term::Application { callee, arguments } = term
                && let Some(value) =
                    lower_constructor_application(callee.as_ref(), arguments, state, program)?
            {
                return Ok(value);
            }
            if let Some(value) = lower_pure_function_call(term, state, program)? {
                return Ok(value);
            }
            if let Ok(expr) = super::lower_i32_expr(term, state) {
                return Ok(Value::I32(expr));
            }
            if let Ok(expr) = super::lower_u8_expr(term, state) {
                return Ok(Value::U8(expr));
            }
            Err(Error::Unsupported(format!(
                "unsupported pure expression in entrypoint body: {term:?}"
            )))
        }
        Term::Path(path) => lower_path_value(path, state, program),
        _ => Err(Error::Unsupported(format!(
            "unsupported pure expression in entrypoint body: {term:?}"
        ))),
    }
}

fn lower_path_value(
    path: &PathExpression,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    if !path.starts_with_package && path.segments.len() == 1 && path.segments[0].suffixes.is_empty()
    {
        let name = path.segments[0].name.as_str();
        if let Some(value) = state.environment.get(name) {
            return Ok(value.clone());
        }
        return Err(Error::Unsupported(format!(
            "unknown entrypoint local `{name}`"
        )));
    }

    let signature = lower_constructor_value(path, &state.constructors)?;
    if signature.arity != 0 {
        return Err(Error::Unsupported(format!(
            "constructor `{}::{}` requires {} arguments",
            signature.type_name, signature.constructor_name, signature.arity
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
            value: I32Expr::Literal(0),
        });
        return Ok(Value::Constructor(ConstructorValue {
            type_name: signature.type_name,
            constructor_name: signature.constructor_name,
            heap_slot: Some(heap_slot),
            fields: Vec::new(),
        }));
    }

    Ok(Value::Constructor(ConstructorValue {
        type_name: signature.type_name,
        constructor_name: signature.constructor_name,
        heap_slot: None,
        fields: Vec::new(),
    }))
}

pub(super) fn lower_constructor_value(
    path: &PathExpression,
    constructors: &HashMap<String, ConstructorSignature>,
) -> Result<ConstructorSignature> {
    if path.starts_with_package {
        return Err(Error::Unsupported(
            "package-qualified constructor paths are not supported in entrypoint lowering"
                .to_string(),
        ));
    }
    if path.segments.len() != 2
        || path
            .segments
            .iter()
            .any(|segment| !segment.suffixes.is_empty())
    {
        return Err(Error::Unsupported(
            "only simple `Type::constructor` paths are supported in entrypoint lowering"
                .to_string(),
        ));
    }

    let key = constructor_key(&path.segments[0].name, &path.segments[1].name);
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

fn pattern_match_bindings(
    pattern: &Pattern,
    value: &Value,
    constructors: &HashMap<String, ConstructorSignature>,
) -> Result<Option<HashMap<String, Value>>> {
    pattern_match_bindings_with_mode(pattern, value, constructors, false)
}

fn pattern_match_bindings_with_mode(
    pattern: &Pattern,
    value: &Value,
    constructors: &HashMap<String, ConstructorSignature>,
    bind_as_reference: bool,
) -> Result<Option<HashMap<String, Value>>> {
    match pattern {
        Pattern::Wildcard => Ok(Some(HashMap::new())),
        Pattern::Bind(name) => {
            let mut bindings = HashMap::new();
            if bind_as_reference {
                let _ = bind_as_reference;
            }
            bindings.insert(name.clone(), value.clone());
            Ok(Some(bindings))
        }
        Pattern::Constructor { path, subpatterns } => {
            let expected = lower_constructor_value(path, constructors)?;
            let Value::Constructor(actual) = value else {
                return Ok(None);
            };

            if actual.type_name != expected.type_name
                || actual.constructor_name != expected.constructor_name
                || actual.fields.len() != subpatterns.len()
            {
                return Ok(None);
            }

            let mut bindings = HashMap::new();
            for (index, (subpattern, field)) in
                subpatterns.iter().zip(actual.fields.iter()).enumerate()
            {
                let _ = (index, actual.heap_slot);
                let field_value: Value = field.clone();
                let Some(sub_bindings) = pattern_match_bindings_with_mode(
                    subpattern,
                    &field_value,
                    constructors,
                    expected.is_rc,
                )?
                else {
                    return Ok(None);
                };
                bindings.extend(sub_bindings);
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
    if signature.arity != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "constructor `{}::{}` must receive exactly {} arguments",
            signature.type_name, signature.constructor_name, signature.arity
        )));
    }

    let mut fields = Vec::with_capacity(normalized_arguments.len());
    for argument in &normalized_arguments {
        fields.push(lower_pure_value(argument, state, program)?);
    }

    if signature.is_rc {
        let heap_slot = allocate_heap_slot(program);
        let object_len = 8 + fields
            .iter()
            .map(|field| match field {
                Value::I32(_) => Ok(4),
                Value::Constructor(ConstructorValue {
                    heap_slot: Some(_), ..
                }) => Ok(8),
                _ => Err(Error::Unsupported(
                    "`type(rc)` currently supports only `i32` and nested `type(rc)` payload fields"
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
            value: I32Expr::Literal(0),
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
                Value::Constructor(ConstructorValue {
                    heap_slot: Some(source_heap_slot),
                    ..
                }) => {
                    program.operations.push(Operation::HeapStorePtr {
                        heap_slot,
                        byte_offset,
                        source_heap_slot: *source_heap_slot,
                    });
                    byte_offset += 8;
                }
                _ => {
                    return Err(Error::Unsupported(
                        "`type(rc)` currently supports only `i32` and nested `type(rc)` payload fields"
                            .to_string(),
                    ));
                }
            }
        }

        return Ok(Some(Value::Constructor(ConstructorValue {
            type_name: signature.type_name,
            constructor_name: signature.constructor_name,
            heap_slot: Some(heap_slot),
            fields,
        })));
    }

    Ok(Some(Value::Constructor(ConstructorValue {
        type_name: signature.type_name,
        constructor_name: signature.constructor_name,
        heap_slot: None,
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
    if path.starts_with_package || path.segments.len() != 1 || !path.segments[0].suffixes.is_empty()
    {
        return Ok(None);
    }

    let name = path.segments[0].name.as_str();
    let Some(function) = state.functions.get(name) else {
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
    for (parameter, argument) in function.parameters.iter().zip(normalized_arguments.iter()) {
        let value = lower_pure_value(argument, state, program)?;
        validate_value_against_type(&value, &parameter.ty, program)?;
        scoped_state
            .environment
            .insert(parameter.name.clone(), value);
    }

    let value = lower_pure_block_value(&function.body, &scoped_state, program)?;
    validate_value_against_type(&value, &function.result_ty, program)?;
    Ok(Some(value))
}

fn lower_pure_block_value(
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
                bind_pattern(&let_stmt.binder, value, &mut scoped_state.environment);
            }
            Statement::Let(_) => {
                return Err(Error::Unsupported(
                    "effectful statements are not supported in pure function bodies".to_string(),
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

pub(super) fn lower_procedure_call_statement(
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<bool>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.starts_with_package || path.segments.len() != 1 || !path.segments[0].suffixes.is_empty()
    {
        return Ok(None);
    }

    let name = path.segments[0].name.as_str();
    if state.environment.contains_key(name) {
        return Ok(None);
    }
    let Some(procedure) = state.procedures.get(name).cloned() else {
        return Ok(None);
    };

    let normalized_arguments = normalize_numeric_literal_arguments(arguments);
    if procedure.parameters.len() != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "procedure `{name}` must receive exactly {} arguments",
            procedure.parameters.len()
        )));
    }

    let mut scoped_state = state.child_scope();
    for (parameter, argument) in procedure.parameters.iter().zip(normalized_arguments.iter()) {
        let value = lower_pure_value(argument, state, program)?;
        validate_value_against_type(&value, &parameter.ty, program)?;
        scoped_state
            .environment
            .insert(parameter.name.clone(), value);
    }

    let mut terminated = false;
    for statement in &procedure.body.statements {
        if terminated {
            return Err(Error::Unsupported(
                "statements after `IO::exit` are not supported".to_string(),
            ));
        }
        terminated = lower_statement(statement, &mut scoped_state, program)?;
    }

    state.next_array_slot = state.next_array_slot.max(scoped_state.next_array_slot);
    state.next_i32_slot = state.next_i32_slot.max(scoped_state.next_i32_slot);

    Ok(Some(terminated))
}
