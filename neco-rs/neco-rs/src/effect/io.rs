use std::collections::HashMap;

use neco_rs_parser::{BindingPattern, Term};

use crate::effect::{Value, bind_pattern, resolve_value};
use crate::{Error, LoweredProgram, Operation, Result};

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
    environment: &mut HashMap<String, Value>,
    program: &mut LoweredProgram,
) -> Option<Result<bool>> {
    match path {
        ["IO", "write"] => {
            let (fd, data_index) = match parse_write_arguments(arguments, environment) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Write { fd, data_index });
            bind_pattern(binder, Value::Unit, environment);
            Some(Ok(false))
        }
        ["IO", "exit"] => {
            let exit_code = match parse_exit_code_arguments(arguments) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Exit(exit_code));
            bind_pattern(binder, Value::Unit, environment);
            Some(Ok(true))
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

fn parse_exit_code_arguments(arguments: &[Term]) -> Result<i32> {
    match arguments {
        [value] => parse_i32_literal(value),
        [value, suffix] if is_i32_suffix(suffix) => parse_bare_integer_literal(value),
        _ => Err(Error::Unsupported(
            "`IO::exit` must receive a single `i32` literal argument".to_string(),
        )),
    }
}

fn parse_i32_literal(term: &Term) -> Result<i32> {
    let Term::IntegerLiteral(literal) = term else {
        return Err(Error::Unsupported(
            "exit code must be an integer literal".to_string(),
        ));
    };

    let digits = literal
        .strip_suffix("i32")
        .ok_or_else(|| Error::Unsupported("exit code must use the `i32` suffix".to_string()))?;
    digits.parse::<i32>().map_err(|_| {
        Error::Unsupported(format!(
            "exit code literal `{literal}` could not be parsed as i32"
        ))
    })
}

fn parse_bare_integer_literal(term: &Term) -> Result<i32> {
    let Term::IntegerLiteral(literal) = term else {
        return Err(Error::Unsupported(
            "exit code must be an integer literal".to_string(),
        ));
    };

    literal.parse::<i32>().map_err(|_| {
        Error::Unsupported(format!(
            "exit code literal `{literal}` could not be parsed as i32"
        ))
    })
}

fn is_i32_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "i32",
        _ => false,
    }
}
