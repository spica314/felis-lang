use std::collections::HashMap;

use neco_rs_parser::{BindingPattern, Term};

use crate::effect::{Value, bind_pattern, resolve_value};
use crate::{
    ArrayElementType, Error, ExitCodeExpr, LoweredProgram, LoweringState, Operation, Result,
    lower_i32_expr, lower_u8_expr,
};

pub(crate) fn lower_io_reference(
    binder: &BindingPattern,
    path: &[&str],
    environment: &mut HashMap<String, Value>,
) -> Option<Result<bool>> {
    match path {
        ["IO", "stdin"] => {
            bind_pattern(binder, Value::FileDescriptor(0), environment);
            Some(Ok(false))
        }
        ["IO", "stdout"] => {
            bind_pattern(binder, Value::FileDescriptor(1), environment);
            Some(Ok(false))
        }
        _ => None,
    }
}

pub(crate) fn lower_io_call(
    binder: &BindingPattern,
    path: &[&str],
    arguments: &[Term],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Option<Result<bool>> {
    match path {
        ["IO", "read"] => {
            let (fd, array_slot, len, result_slot) = match parse_read_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Read {
                fd,
                array_slot,
                len,
                result_slot,
            });
            bind_pattern(
                binder,
                Value::I32(crate::I32Expr::Local(result_slot)),
                &mut state.environment,
            );
            Some(Ok(false))
        }
        ["IO", "write"] => {
            let operation = match parse_write_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(operation);
            bind_pattern(binder, Value::Unit, &mut state.environment);
            Some(Ok(false))
        }
        ["IO", "exit"] => {
            let exit_code = match parse_exit_code_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Exit(exit_code));
            bind_pattern(binder, Value::Unit, &mut state.environment);
            Some(Ok(true))
        }
        ["IO", "array_new"] => {
            let (element_type, length) = match parse_array_new_arguments(arguments) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            let array_slot = state.allocate_array(element_type, length, program);
            bind_pattern(
                binder,
                Value::Array {
                    slot: array_slot,
                    element_type,
                },
                &mut state.environment,
            );
            Some(Ok(false))
        }
        _ => None,
    }
}

fn parse_write_arguments(
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Operation> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [fd_term, bytes_term, len_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::write` must receive a file descriptor, a byte string reference, and a length"
                .to_string(),
        ));
    };

    let fd = match resolve_value(fd_term, &state.environment)? {
        Value::FileDescriptor(fd) => fd,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::write` expects a file descriptor as its first argument, got {other:?}"
            )));
        }
    };

    let len = lower_i32_expr(len_term, state).map_err(|_| {
        Error::Unsupported("`IO::write` expects an `i32` length as its third argument".to_string())
    })?;

    match resolve_value(bytes_term, &state.environment)? {
        Value::ByteString(data_index) => Ok(Operation::WriteStatic {
            fd,
            data_index,
            len,
        }),
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
        } => Ok(Operation::WriteArray {
            fd,
            array_slot: slot,
            len,
        }),
        other => Err(Error::Unsupported(format!(
            "`IO::write` expects a byte string reference or `u8` array as its second argument, got {other:?}"
        ))),
    }
}

fn parse_read_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(u32, usize, crate::I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [fd_term, bytes_term, len_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::read` must receive a file descriptor, a `u8` array reference, and a length"
                .to_string(),
        ));
    };

    let fd = match resolve_value(fd_term, &state.environment)? {
        Value::FileDescriptor(fd) => fd,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::read` expects a file descriptor as its first argument, got {other:?}"
            )));
        }
    };

    let array_slot = match resolve_value(bytes_term, &state.environment)? {
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::read` expects a `u8` array reference as its second argument, got {other:?}"
            )));
        }
    };

    let len = lower_i32_expr(len_term, state).map_err(|_| {
        Error::Unsupported("`IO::read` expects an `i32` length as its third argument".to_string())
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok((fd, array_slot, len, result_slot))
}

fn normalize_numeric_literal_arguments(arguments: &[Term]) -> Vec<Term> {
    let mut normalized = Vec::with_capacity(arguments.len());
    let mut index = 0;
    while index < arguments.len() {
        if index + 1 < arguments.len()
            && matches!(arguments[index], Term::IntegerLiteral(_))
            && (is_i32_suffix(&arguments[index + 1]) || is_u8_suffix(&arguments[index + 1]))
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

fn parse_exit_code_arguments(arguments: &[Term], state: &LoweringState) -> Result<ExitCodeExpr> {
    match arguments {
        [value] => lower_i32_expr(value, state)
            .map(ExitCodeExpr::I32)
            .or_else(|_| lower_u8_expr(value, state).map(ExitCodeExpr::U8)),
        [value, suffix] if is_i32_suffix(suffix) || is_u8_suffix(suffix) => {
            let term = Term::Application {
                callee: Box::new(value.clone()),
                arguments: vec![suffix.clone()],
            };
            lower_i32_expr(&term, state)
                .map(ExitCodeExpr::I32)
                .or_else(|_| lower_u8_expr(&term, state).map(ExitCodeExpr::U8))
        }
        _ => Err(Error::Unsupported(
            "`IO::exit` must receive a single `i32` or `u8` expression".to_string(),
        )),
    }
}

fn parse_array_new_arguments(arguments: &[Term]) -> Result<(ArrayElementType, usize)> {
    let normalized = normalize_i32_literal_arguments(arguments);
    let [element_type, length] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::array_new` must receive an element type and a constant i32 length".to_string(),
        ));
    };

    let Term::Path(path) = element_type else {
        return Err(Error::Unsupported(
            "`IO::array_new` element type must be a simple path".to_string(),
        ));
    };
    let [segment] = path.segments.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::array_new` element type must be a simple path".to_string(),
        ));
    };
    let element_type = match segment.name.as_str() {
        "i32" => ArrayElementType::I32,
        "u8" => ArrayElementType::U8,
        _ => {
            return Err(Error::Unsupported(
                "`IO::array_new` currently supports only `i32` and `u8` arrays".to_string(),
            ));
        }
    };

    let length = parse_i32_literal_term(length)?;
    let length = usize::try_from(length)
        .map_err(|_| Error::Unsupported("`IO::array_new` length must be non-negative".to_string()))?;
    Ok((element_type, length))
}

fn is_i32_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "i32",
        _ => false,
    }
}

fn is_u8_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "u8",
        _ => false,
    }
}

fn parse_i32_literal_term(term: &Term) -> Result<i32> {
    match term {
        Term::IntegerLiteral(literal) => parse_bare_i32_digits(literal),
        Term::Application { callee, arguments } => {
            let [suffix] = arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`IO::array_new` length must be an `i32` literal".to_string(),
                ));
            };
            if !is_i32_suffix(suffix) {
                return Err(Error::Unsupported(
                    "`IO::array_new` length must be an `i32` literal".to_string(),
                ));
            }
            let Term::IntegerLiteral(literal) = callee.as_ref() else {
                return Err(Error::Unsupported(
                    "`IO::array_new` length must be an `i32` literal".to_string(),
                ));
            };
            parse_bare_i32_digits(literal)
        }
        _ => Err(Error::Unsupported(
            "`IO::array_new` length must be an `i32` literal".to_string(),
        )),
    }
}

fn parse_bare_i32_digits(literal: &str) -> Result<i32> {
    literal.parse::<i32>().map_err(|_| {
        Error::Unsupported("`IO::array_new` length must be a valid `i32` literal".to_string())
    })
}

fn normalize_i32_literal_arguments(arguments: &[Term]) -> Vec<Term> {
    let mut normalized = Vec::with_capacity(arguments.len());
    let mut index = 0;
    while index < arguments.len() {
        if index + 1 < arguments.len()
            && matches!(arguments[index], Term::IntegerLiteral(_))
            && is_i32_suffix(&arguments[index + 1])
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
