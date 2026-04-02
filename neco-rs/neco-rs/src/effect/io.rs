use std::collections::HashMap;

use neco_rs_parser::{BindingPattern, Term};

use crate::effect::{Value, bind_pattern, resolve_value};
use crate::{Error, LoweredProgram, Operation, Result, lower_i32_expr};

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
            let exit_code = match parse_exit_code_arguments(arguments, environment) {
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

fn parse_exit_code_arguments(
    arguments: &[Term],
    environment: &HashMap<String, Value>,
) -> Result<crate::I32Expr> {
    match arguments {
        [value] => lower_i32_expr(value, environment),
        [value, suffix] if is_i32_suffix(suffix) => lower_i32_expr(
            &Term::Application {
                callee: Box::new(value.clone()),
                arguments: vec![suffix.clone()],
            },
            environment,
        ),
        _ => Err(Error::Unsupported(
            "`IO::exit` must receive a single `i32` expression".to_string(),
        )),
    }
}

fn is_i32_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "i32",
        _ => false,
    }
}
