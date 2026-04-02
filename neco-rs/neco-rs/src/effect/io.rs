use std::collections::HashMap;

use neco_rs_parser::{BindingPattern, Term};

use crate::effect::{Value, bind_pattern, resolve_value};
use crate::{
    Error, ExitCodeExpr, LoweredProgram, LoweringState, Operation, Result, lower_i32_expr,
    lower_u8_expr,
};

pub(crate) fn lower_io_reference(
    binder: &BindingPattern,
    path: &[&str],
    environment: &mut HashMap<String, Value>,
) -> Option<Result<bool>> {
    match path {
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
        ["IO", "write"] => {
            let (fd, data_index) = match parse_write_arguments(arguments, &state.environment) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Write { fd, data_index });
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
            let array_slot = match parse_array_new_arguments(arguments) {
                Ok(length) => state.allocate_array(length, program),
                Err(err) => return Some(Err(err)),
            };
            bind_pattern(binder, Value::Array(array_slot), &mut state.environment);
            Some(Ok(false))
        }
        _ => None,
    }
}

fn parse_write_arguments(
    arguments: &[Term],
    environment: &HashMap<String, Value>,
) -> Result<(u32, usize)> {
    let [fd_term, bytes_term] = arguments else {
        return Err(Error::Unsupported(
            "`IO::write` must receive a file descriptor and a byte string reference".to_string(),
        ));
    };

    let fd = match resolve_value(fd_term, environment)? {
        Value::FileDescriptor(fd) => fd,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::write` expects a file descriptor as its first argument, got {other:?}"
            )));
        }
    };

    let data_index = match resolve_value(bytes_term, environment)? {
        Value::ByteString(data_index) => data_index,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::write` expects a byte string reference as its second argument, got {other:?}"
            )));
        }
    };

    Ok((fd, data_index))
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

fn parse_array_new_arguments(arguments: &[Term]) -> Result<usize> {
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
    if segment.name != "i32" {
        return Err(Error::Unsupported(
            "`IO::array_new` currently supports only `i32` arrays".to_string(),
        ));
    }

    let length = parse_i32_literal_term(length)?;
    usize::try_from(length)
        .map_err(|_| Error::Unsupported("`IO::array_new` length must be non-negative".to_string()))
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
