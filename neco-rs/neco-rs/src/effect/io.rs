use neco_rs_parser::{BindingPattern, Term};

use crate::effect::{Value, bind_pattern, resolve_value};
use crate::ir::{
    ArrayElementType, ArrayKind, ExitCodeExpr, I32Expr, I64Expr, KernelArgumentRef, LoweredProgram,
    OpenPath, Operation, PathBufSource, U8Expr, intern_data,
};
use crate::lowering::{
    LoweringState, lower_i32_expr, lower_i64_expr, lower_u8_expr, validate_value_against_type,
};
use crate::{Error, Result};

pub(crate) fn lower_io_reference(
    binder: &BindingPattern,
    ty: &Term,
    path: &[&str],
    state: &mut LoweringState,
    program: &LoweredProgram,
) -> Option<Result<bool>> {
    match path {
        ["IO", "stdin"] => {
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::FileDescriptor(I32Expr::Literal(0)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "stdout"] => {
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::FileDescriptor(I32Expr::Literal(1)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        _ => None,
    }
}

pub(crate) fn lower_io_call(
    binder: &BindingPattern,
    ty: &Term,
    path: &[&str],
    arguments: &[Term],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Option<Result<bool>> {
    match path {
        ["IO", "sys_read"] => {
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
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "sys_open"] => {
            let (path, flags, mode, result_slot) = match parse_open_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Open {
                path,
                flags,
                mode,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::FileDescriptor(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "pathbuf_new"] => {
            let (capacity, array_slot) =
                match parse_pathbuf_new_arguments(arguments, state, program) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            program.operations.push(Operation::ArrayAllocDynamic {
                array_slot,
                len: capacity,
            });
            program.operations.push(Operation::ArraySetU8 {
                array_slot,
                index: I64Expr::Literal(0),
                value: U8Expr::Literal(0),
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::PathBuf { slot: array_slot },
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "pathbuf_push"] => {
            let (path_slot, source) = match parse_pathbuf_push_arguments(arguments, state, program)
            {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program
                .operations
                .push(Operation::PathBufPush { path_slot, source });
            if let Err(error) = bind_checked_pattern(binder, Value::Unit, ty, state, program) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "pathbuf_pop"] => {
            let path_slot = match parse_pathbuf_pop_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::PathBufPop { path_slot });
            if let Err(error) = bind_checked_pattern(binder, Value::Unit, ty, state, program) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "sys_close"] => {
            let fd = match parse_close_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Close { fd });
            if let Err(error) = bind_checked_pattern(binder, Value::Unit, ty, state, program) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "sys_write"] => {
            let operation = match parse_write_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(operation);
            if let Err(error) = bind_checked_pattern(binder, Value::Unit, ty, state, program) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "panic"] => {
            let operation = match parse_panic_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(operation);
            program
                .operations
                .push(Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(101))));
            if let Err(error) = bind_checked_pattern(binder, Value::Unit, ty, state, program) {
                return Some(Err(error));
            }
            Some(Ok(true))
        }
        ["IO", "arrayvl_replace"] => {
            let (dest_slot, source_slot) = match parse_arrayvl_replace_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::ArrayReplace {
                dest_slot,
                source_slot,
            });
            if let Err(error) = bind_checked_pattern(binder, Value::Unit, ty, state, program) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "cu_init"] => {
            let (flags, result_slot) = match parse_cu_init_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program
                .operations
                .push(Operation::CuInit { flags, result_slot });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "cu_device_get"] => {
            let (device_slot, ordinal, result_slot) =
                match parse_cu_device_get_arguments(arguments, state) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            program.operations.push(Operation::CuDeviceGet {
                device_slot,
                ordinal,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "cu_ctx_create_v2"] => {
            let (ctx_slot, flags, device, result_slot) =
                match parse_cu_ctx_create_v2_arguments(arguments, state) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            program.operations.push(Operation::CuCtxCreateV2 {
                ctx_slot,
                flags,
                device,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "cu_module_load_data"] => {
            let (module_slot, data_index, result_slot) =
                match parse_cu_module_load_data_arguments(arguments, state) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            program.operations.push(Operation::CuModuleLoadData {
                module_slot,
                data_index,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "cu_module_get_function"] => {
            let (function_slot, module, name_data_index, result_slot) =
                match parse_cu_module_get_function_arguments(arguments, state, program) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            program.operations.push(Operation::CuModuleGetFunction {
                function_slot,
                module,
                name_data_index,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "cu_launch_kernel"] => {
            let launch = match parse_cu_launch_kernel_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::CuLaunchKernel {
                function: launch.function,
                arg: launch.arg,
                grid_dim_x: launch.grid_dim_x,
                grid_dim_y: launch.grid_dim_y,
                grid_dim_z: launch.grid_dim_z,
                block_dim_x: launch.block_dim_x,
                block_dim_y: launch.block_dim_y,
                block_dim_z: launch.block_dim_z,
                shared_mem_bytes: launch.shared_mem_bytes,
                stream: launch.stream,
                result_slot: launch.result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(launch.result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "arrayvlptx_new"] => {
            let (element_type, length, result_slot) =
                match parse_arrayvlptx_new_arguments(arguments, state) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            if matches!(ty, Term::Reference { .. }) {
                return Some(Err(Error::Unsupported(
                    "`IO::arrayvlptx_new` returns an `ArrayVLPTX T` value; use `#let` for the value"
                        .to_string(),
                )));
            }
            let array_slot =
                state.allocate_array(element_type, ArrayKind::DeviceDynamic, 0, program);
            program.operations.push(Operation::CuMemAllocV2 {
                array_slot,
                len: length,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::Array {
                    slot: array_slot,
                    element_type,
                    kind: ArrayKind::DeviceDynamic,
                },
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "arrayvl_to_ptx"] => {
            let (source_slot, dest_slot, len, result_slot) =
                match parse_arrayvl_copy_arguments(path[1], arguments, state, false) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            program.operations.push(Operation::CuMemcpyHtoDV2 {
                dest_slot,
                source_slot,
                len,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "arrayvl_from_ptx"] => {
            let (source_slot, dest_slot, len, result_slot) =
                match parse_arrayvl_copy_arguments(path[1], arguments, state, true) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            program.operations.push(Operation::CuMemcpyDtoHV2 {
                dest_slot,
                source_slot,
                len,
                result_slot,
            });
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::I32(I32Expr::Local(result_slot)),
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "sys_exit"] => {
            let exit_code = match parse_exit_code_arguments(arguments, state) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            program.operations.push(Operation::Exit(exit_code));
            if let Err(error) = bind_checked_pattern(binder, Value::Unit, ty, state, program) {
                return Some(Err(error));
            }
            Some(Ok(true))
        }
        ["IO", "array_new"] | ["IO", "arrayvl_new"] => {
            let (element_type, length, static_length) =
                match parse_array_new_arguments(path[1], arguments, state) {
                    Ok(value) => value,
                    Err(err) => return Some(Err(err)),
                };
            if matches!(path[1], "array_new" | "arrayvl_new")
                && matches!(ty, Term::Reference { .. })
            {
                let expected = if path[1] == "array_new" {
                    "Array T len"
                } else {
                    "ArrayVL T"
                };
                return Some(Err(Error::Unsupported(format!(
                    "`IO::{}` returns an `{}` value; use `#let` for the value, then `#letref #excl` to borrow it",
                    path[1], expected
                ))));
            }
            let kind = match path[1] {
                "array_new" => ArrayKind::Fixed,
                "arrayvl_new" => ArrayKind::Dynamic,
                _ => unreachable!(),
            };
            let array_slot =
                state.allocate_array(element_type, kind, static_length.unwrap_or(0), program);
            if static_length.is_none() {
                program.operations.push(Operation::ArrayAllocDynamic {
                    array_slot,
                    len: length,
                });
            }
            if let Err(error) = bind_checked_pattern(
                binder,
                Value::Array {
                    slot: array_slot,
                    element_type,
                    kind,
                },
                ty,
                state,
                program,
            ) {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        ["IO", "arg"] => {
            let arg_index = match parse_arg_arguments(arguments, state, program) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            if let Err(error) =
                bind_checked_pattern(binder, Value::RuntimeArg(arg_index), ty, state, program)
            {
                return Some(Err(error));
            }
            Some(Ok(false))
        }
        _ => None,
    }
}

fn parse_panic_arguments(arguments: &[Term], state: &LoweringState) -> Result<Operation> {
    let [message_term] = arguments else {
        return Err(Error::Unsupported(
            "`IO::panic` must receive a single `ArrayVL u8` message".to_string(),
        ));
    };

    match resolve_value(message_term, &state.environment)? {
        Value::StaticSlice { data_index, len } => Ok(Operation::WriteStatic {
            fd: I32Expr::Literal(2),
            data_index,
            len: I32Expr::Literal(len),
        }),
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
            ..
        } => Ok(Operation::WriteArray {
            fd: I32Expr::Literal(2),
            array_slot: slot,
            len: I32Expr::ArrayLen { array_slot: slot },
        }),
        other => Err(Error::Unsupported(format!(
            "`IO::panic` expects an `ArrayVL u8` message, got {other:?}"
        ))),
    }
}

fn bind_checked_pattern(
    binder: &BindingPattern,
    value: Value,
    ty: &Term,
    state: &mut LoweringState,
    program: &LoweredProgram,
) -> Result<()> {
    validate_value_against_type(&value, ty, program)?;
    bind_pattern(binder, value, &mut state.environment);
    Ok(())
}

fn parse_write_arguments(arguments: &[Term], state: &LoweringState) -> Result<Operation> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [fd_term, bytes_term, len_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::sys_write` must receive a file descriptor, an `ArrayVL u8`, and a length"
                .to_string(),
        ));
    };

    let fd = match resolve_value(fd_term, &state.environment)? {
        Value::FileDescriptor(fd) => fd,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::sys_write` expects a file descriptor as its first argument, got {other:?}"
            )));
        }
    };

    let len = lower_i32_expr(len_term, state).map_err(|_| {
        Error::Unsupported(
            "`IO::sys_write` expects an `i32` length as its third argument".to_string(),
        )
    })?;

    match resolve_value(bytes_term, &state.environment)? {
        Value::StaticSlice { data_index, .. } => Ok(Operation::WriteStatic {
            fd,
            data_index,
            len,
        }),
        value if u8_array_slot(&value).is_some() => {
            let slot = u8_array_slot(&value).expect("checked u8 array slot");
            Ok(Operation::WriteArray {
                fd,
                array_slot: slot,
                len,
            })
        }
        other => Err(Error::Unsupported(format!(
            "`IO::sys_write` expects an `ArrayVL u8` or `u8` array as its second argument, got {other:?}"
        ))),
    }
}

fn parse_read_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(I32Expr, usize, I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [fd_term, bytes_term, len_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::sys_read` must receive a file descriptor, a `u8` array reference, and a length"
                .to_string(),
        ));
    };

    let fd = match resolve_value(fd_term, &state.environment)? {
        Value::FileDescriptor(fd) => fd,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::sys_read` expects a file descriptor as its first argument, got {other:?}"
            )));
        }
    };

    let array_slot = match resolve_value(bytes_term, &state.environment)? {
        value if u8_array_slot(&value).is_some() => {
            u8_array_slot(&value).expect("checked u8 array slot")
        }
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::sys_read` expects a `u8` array reference as its second argument, got {other:?}"
            )));
        }
    };

    let len = lower_i32_expr(len_term, state).map_err(|_| {
        Error::Unsupported(
            "`IO::sys_read` expects an `i32` length as its third argument".to_string(),
        )
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok((fd, array_slot, len, result_slot))
}

fn u8_array_slot(value: &Value) -> Option<usize> {
    match value {
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
            ..
        } => Some(*slot),
        Value::Reference { value, .. } => u8_array_slot(value),
        _ => None,
    }
}

fn parse_arrayvl_replace_arguments(
    arguments: &[Term],
    state: &LoweringState,
) -> Result<(usize, usize)> {
    let normalized = normalize_i32_literal_arguments(arguments);
    let [element_type, dest, source] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::arrayvl_replace` must receive an element type and two `ArrayVL` values"
                .to_string(),
        ));
    };
    let element_type = parse_array_element_type(element_type, "arrayvl_replace", state)?;
    let dest_slot = parse_dynamic_array_slot(dest, element_type, state, "arrayvl_replace")?;
    let source_slot = parse_dynamic_array_slot(source, element_type, state, "arrayvl_replace")?;
    Ok((dest_slot, source_slot))
}

fn parse_dynamic_array_slot(
    term: &Term,
    element_type: ArrayElementType,
    state: &LoweringState,
    builtin_name: &str,
) -> Result<usize> {
    match resolve_value(term, &state.environment)? {
        Value::Array {
            slot,
            element_type: actual_element_type,
            kind: ArrayKind::Dynamic,
        } if actual_element_type == element_type => Ok(slot),
        other => Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` expects matching dynamic `ArrayVL` values, got {other:?}"
        ))),
    }
}

fn parse_open_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(OpenPath, I32Expr, I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [path_term, flags_term, mode_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::sys_open` must receive a `PathBuf`, flags, and mode".to_string(),
        ));
    };

    let path = if matches!(path_term, Term::StringLiteral(_)) {
        return Err(Error::Unsupported(
            "`IO::sys_open` expects a `PathBuf` as its first argument".to_string(),
        ));
    } else {
        OpenPath::PathBuf(parse_pathbuf_slot(path_term, state, "sys_open", false)?)
    };

    let flags = lower_i32_expr(flags_term, state).map_err(|_| {
        Error::Unsupported("`IO::sys_open` expects `i32` flags as its second argument".to_string())
    })?;
    let mode = lower_i32_expr(mode_term, state).map_err(|_| {
        Error::Unsupported("`IO::sys_open` expects an `i32` mode as its third argument".to_string())
    })?;

    let result_slot = state.allocate_i32_slot();
    Ok((path, flags, mode, result_slot))
}

fn parse_pathbuf_new_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<(I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [capacity_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::pathbuf_new` must receive an `i32` capacity".to_string(),
        ));
    };
    let capacity = lower_i32_expr(capacity_term, state).map_err(|_| {
        Error::Unsupported("`IO::pathbuf_new` expects an `i32` capacity".to_string())
    })?;
    let array_slot = state.allocate_array(ArrayElementType::U8, ArrayKind::Dynamic, 0, program);
    Ok((capacity, array_slot))
}

fn parse_pathbuf_push_arguments(
    arguments: &[Term],
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<(usize, PathBufSource)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [path_term, source_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::pathbuf_push` must receive a `PathBuf` and an `ArrayVL u8` source".to_string(),
        ));
    };
    let path_slot = parse_pathbuf_slot(path_term, state, "pathbuf_push", true)?;
    let source = if let Term::StringLiteral(literal) = source_term {
        let mut bytes = literal.as_bytes().to_vec();
        bytes.push(0);
        PathBufSource::StaticData(intern_data(program, bytes))
    } else {
        match resolve_value(source_term, &state.environment)? {
            Value::StaticSlice { data_index, .. } => PathBufSource::StaticData(data_index),
            Value::RuntimeArg(arg_index) => PathBufSource::RuntimeArg(arg_index),
            value if u8_array_slot(&value).is_some() => {
                PathBufSource::Array(u8_array_slot(&value).expect("checked u8 array slot"))
            }
            other => {
                return Err(Error::Unsupported(format!(
                    "`IO::pathbuf_push` expects a nul-terminated `u8` source, got {other:?}"
                )));
            }
        }
    };
    Ok((path_slot, source))
}

fn parse_pathbuf_pop_arguments(arguments: &[Term], state: &LoweringState) -> Result<usize> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [path_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::pathbuf_pop` must receive a `PathBuf`".to_string(),
        ));
    };
    parse_pathbuf_slot(path_term, state, "pathbuf_pop", true)
}

fn parse_pathbuf_slot(
    term: &Term,
    state: &LoweringState,
    builtin_name: &str,
    require_exclusive: bool,
) -> Result<usize> {
    match resolve_value(term, &state.environment)? {
        Value::PathBuf { slot } if !require_exclusive => Ok(slot),
        Value::Reference { value, exclusive } => match value.as_ref() {
            Value::PathBuf { slot } if !require_exclusive || exclusive => Ok(*slot),
            Value::PathBuf { .. } => Err(Error::Unsupported(format!(
                "`IO::{builtin_name}` requires an exclusive `PathBuf` reference"
            ))),
            other => Err(Error::Unsupported(format!(
                "`IO::{builtin_name}` expects a `PathBuf`, got {other:?}"
            ))),
        },
        Value::PathBuf { .. } => Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` requires an exclusive `PathBuf` reference"
        ))),
        other => Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` expects a `PathBuf`, got {other:?}"
        ))),
    }
}

fn parse_close_arguments(arguments: &[Term], state: &LoweringState) -> Result<I32Expr> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [fd_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::sys_close` must receive a file descriptor".to_string(),
        ));
    };

    match resolve_value(fd_term, &state.environment)? {
        Value::FileDescriptor(fd) => Ok(fd),
        other => Err(Error::Unsupported(format!(
            "`IO::sys_close` expects a file descriptor, got {other:?}"
        ))),
    }
}

fn parse_cu_init_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [flags_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::cu_init` must receive a single `i32` flags argument".to_string(),
        ));
    };

    let flags = lower_i32_expr(flags_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_init` expects an `i32` flags argument".to_string())
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok((flags, result_slot))
}

fn parse_cu_device_get_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(usize, I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [device_term, ordinal_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::cu_device_get` must receive an exclusive `i32` reference and an `i32` ordinal"
                .to_string(),
        ));
    };

    let device_slot = match resolve_value(device_term, &state.environment)? {
        Value::I32Reference {
            slot,
            exclusive: true,
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::cu_device_get` expects an exclusive `i32` reference as its first argument, got {other:?}"
            )));
        }
    };
    let ordinal = lower_i32_expr(ordinal_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_device_get` expects an `i32` ordinal".to_string())
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok((device_slot, ordinal, result_slot))
}

fn parse_cu_ctx_create_v2_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(usize, I32Expr, I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [ctx_term, flags_term, device_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::cu_ctx_create_v2` must receive an exclusive `i64` reference, `i32` flags, and an `i32` device"
                .to_string(),
        ));
    };

    let ctx_slot = match resolve_value(ctx_term, &state.environment)? {
        Value::I64Reference {
            slot,
            exclusive: true,
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::cu_ctx_create_v2` expects an exclusive `i64` reference as its first argument, got {other:?}"
            )));
        }
    };
    let flags = lower_i32_expr(flags_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_ctx_create_v2` expects `i32` flags".to_string())
    })?;
    let device = lower_i32_expr(device_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_ctx_create_v2` expects an `i32` device".to_string())
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok((ctx_slot, flags, device, result_slot))
}

fn parse_cu_module_load_data_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(usize, usize, usize)> {
    let [module_term, ptx_term] = arguments else {
        return Err(Error::Unsupported(
            "`IO::cu_module_load_data` must receive an exclusive `i64` reference and a PTX string"
                .to_string(),
        ));
    };

    let module_slot = match resolve_value(module_term, &state.environment)? {
        Value::I64Reference {
            slot,
            exclusive: true,
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::cu_module_load_data` expects an exclusive `i64` reference as its first argument, got {other:?}"
            )));
        }
    };
    let data_index = match resolve_value(ptx_term, &state.environment)? {
        Value::StaticSlice { data_index, .. } => data_index,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::cu_module_load_data` expects a static PTX string as its second argument, got {other:?}"
            )));
        }
    };
    let result_slot = state.allocate_i32_slot();
    Ok((module_slot, data_index, result_slot))
}

fn parse_cu_module_get_function_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<(usize, I64Expr, usize, usize)> {
    let [function_term, module_term, name_term] = arguments else {
        return Err(Error::Unsupported(
            "`IO::cu_module_get_function` must receive an exclusive `i64` reference, an `i64` module, and a function name string"
                .to_string(),
        ));
    };

    let function_slot = match resolve_value(function_term, &state.environment)? {
        Value::I64Reference {
            slot,
            exclusive: true,
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::cu_module_get_function` expects an exclusive `i64` reference as its first argument, got {other:?}"
            )));
        }
    };
    let module = lower_i64_expr(module_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_module_get_function` expects an `i64` module".to_string())
    })?;
    let name_data_index = if let Term::StringLiteral(literal) = name_term {
        let mut bytes = literal.as_bytes().to_vec();
        bytes.push(0);
        intern_data(program, bytes)
    } else {
        match resolve_value(name_term, &state.environment)? {
            Value::StaticSlice { data_index, .. } => data_index,
            other => {
                return Err(Error::Unsupported(format!(
                    "`IO::cu_module_get_function` expects a function name string as its third argument, got {other:?}"
                )));
            }
        }
    };
    let result_slot = state.allocate_i32_slot();
    Ok((function_slot, module, name_data_index, result_slot))
}

struct CuLaunchKernelArguments {
    function: I64Expr,
    arg: KernelArgumentRef,
    grid_dim_x: I32Expr,
    grid_dim_y: I32Expr,
    grid_dim_z: I32Expr,
    block_dim_x: I32Expr,
    block_dim_y: I32Expr,
    block_dim_z: I32Expr,
    shared_mem_bytes: I32Expr,
    stream: I64Expr,
    result_slot: usize,
}

fn parse_cu_launch_kernel_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<CuLaunchKernelArguments> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [
        function_term,
        arg_term,
        grid_dim_x_term,
        grid_dim_y_term,
        grid_dim_z_term,
        block_dim_x_term,
        block_dim_y_term,
        block_dim_z_term,
        shared_mem_bytes_term,
        stream_term,
    ] = normalized.as_slice()
    else {
        return Err(Error::Unsupported(
            "`IO::cu_launch_kernel` must receive a function, one argument reference, grid dims, block dims, shared memory bytes, and stream"
                .to_string(),
        ));
    };

    let function = lower_i64_expr(function_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects an `i64` function".to_string())
    })?;
    let arg = match resolve_value(arg_term, &state.environment)? {
        Value::I32Reference { slot, .. } => KernelArgumentRef::I32(slot),
        Value::I64Reference { slot, .. } => KernelArgumentRef::I64(slot),
        Value::F32Reference { slot, .. } => KernelArgumentRef::F32(slot),
        Value::Array {
            slot,
            kind: ArrayKind::DeviceDynamic,
            ..
        } => KernelArgumentRef::ArrayPtx(slot),
        other => {
            return Err(Error::Unsupported(format!(
                "`IO::cu_launch_kernel` expects a primitive reference or `ArrayVLPTX` value as its kernel argument, got {other:?}"
            )));
        }
    };
    let grid_dim_x = lower_i32_expr(grid_dim_x_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects `i32` grid_dim_x".to_string())
    })?;
    let grid_dim_y = lower_i32_expr(grid_dim_y_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects `i32` grid_dim_y".to_string())
    })?;
    let grid_dim_z = lower_i32_expr(grid_dim_z_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects `i32` grid_dim_z".to_string())
    })?;
    let block_dim_x = lower_i32_expr(block_dim_x_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects `i32` block_dim_x".to_string())
    })?;
    let block_dim_y = lower_i32_expr(block_dim_y_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects `i32` block_dim_y".to_string())
    })?;
    let block_dim_z = lower_i32_expr(block_dim_z_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects `i32` block_dim_z".to_string())
    })?;
    let shared_mem_bytes = lower_i32_expr(shared_mem_bytes_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects `i32` shared memory bytes".to_string())
    })?;
    let stream = lower_i64_expr(stream_term, state).map_err(|_| {
        Error::Unsupported("`IO::cu_launch_kernel` expects an `i64` stream".to_string())
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok(CuLaunchKernelArguments {
        function,
        arg,
        grid_dim_x,
        grid_dim_y,
        grid_dim_z,
        block_dim_x,
        block_dim_y,
        block_dim_z,
        shared_mem_bytes,
        stream,
        result_slot,
    })
}

fn parse_arrayvlptx_new_arguments(
    arguments: &[Term],
    state: &mut LoweringState,
) -> Result<(ArrayElementType, I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [element_type_term, length_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::arrayvlptx_new` must receive an element type and an i32 length".to_string(),
        ));
    };
    let element_type = parse_array_element_type(element_type_term, "arrayvlptx_new", state)?;
    let length = lower_i32_expr(length_term, state).map_err(|_| {
        Error::Unsupported("`IO::arrayvlptx_new` length must be an i32 expression".to_string())
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok((element_type, length, result_slot))
}

fn parse_arrayvl_copy_arguments(
    builtin_name: &str,
    arguments: &[Term],
    state: &mut LoweringState,
    from_ptx: bool,
) -> Result<(usize, usize, I32Expr, usize)> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [element_type_term, source_term, dest_term, len_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` must receive an element type, source array, destination array, and i32 length"
        )));
    };
    let element_type = parse_array_element_type(element_type_term, builtin_name, state)?;
    let source_slot = if from_ptx {
        parse_device_array_slot(source_term, element_type, state, builtin_name)?
    } else {
        parse_dynamic_array_slot(source_term, element_type, state, builtin_name)?
    };
    let dest_slot = if from_ptx {
        parse_dynamic_array_slot(dest_term, element_type, state, builtin_name)?
    } else {
        parse_device_array_slot(dest_term, element_type, state, builtin_name)?
    };
    let len = lower_i32_expr(len_term, state).map_err(|_| {
        Error::Unsupported(format!(
            "`IO::{builtin_name}` length must be an i32 expression"
        ))
    })?;
    let result_slot = state.allocate_i32_slot();
    Ok((source_slot, dest_slot, len, result_slot))
}

fn parse_device_array_slot(
    term: &Term,
    element_type: ArrayElementType,
    state: &LoweringState,
    builtin_name: &str,
) -> Result<usize> {
    match resolve_value(term, &state.environment)? {
        Value::Array {
            slot,
            element_type: actual_element_type,
            kind: ArrayKind::DeviceDynamic,
        } if actual_element_type == element_type => Ok(slot),
        other => Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` expects matching dynamic `ArrayVLPTX` values, got {other:?}"
        ))),
    }
}

fn normalize_numeric_literal_arguments(arguments: &[Term]) -> Vec<Term> {
    let mut normalized = Vec::with_capacity(arguments.len());
    let mut index = 0;
    while index < arguments.len() {
        if index + 1 < arguments.len()
            && matches!(arguments[index], Term::IntegerLiteral(_))
            && (is_i32_suffix(&arguments[index + 1])
                || is_i64_suffix(&arguments[index + 1])
                || is_f32_suffix(&arguments[index + 1])
                || is_u8_suffix(&arguments[index + 1]))
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
            .or_else(|_| lower_i64_expr(value, state).map(ExitCodeExpr::I64))
            .or_else(|_| lower_u8_expr(value, state).map(ExitCodeExpr::U8)),
        [value, suffix]
            if is_i32_suffix(suffix) || is_i64_suffix(suffix) || is_u8_suffix(suffix) =>
        {
            let term = Term::Application {
                callee: Box::new(value.clone()),
                arguments: vec![suffix.clone()],
            };
            lower_i32_expr(&term, state)
                .map(ExitCodeExpr::I32)
                .or_else(|_| lower_i64_expr(&term, state).map(ExitCodeExpr::I64))
                .or_else(|_| lower_u8_expr(&term, state).map(ExitCodeExpr::U8))
        }
        _ => Err(Error::Unsupported(
            "`IO::sys_exit` must receive a single `i32`, `i64`, or `u8` expression".to_string(),
        )),
    }
}

fn parse_array_new_arguments(
    builtin_name: &str,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<(ArrayElementType, I32Expr, Option<usize>)> {
    let normalized = normalize_i32_literal_arguments(arguments);
    let [element_type, length] = normalized.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` must receive an element type and a constant i32 length"
        )));
    };

    let element_type = parse_array_element_type(element_type, builtin_name, state)?;

    let static_length = match parse_i32_literal_term(length, builtin_name) {
        Ok(length) => Some(usize::try_from(length).map_err(|_| {
            Error::Unsupported(format!("`IO::{builtin_name}` length must be non-negative"))
        })?),
        Err(_) if builtin_name == "arrayvl_new" => None,
        Err(error) => return Err(error),
    };
    let length = lower_i32_expr(length, state).map_err(|_| {
        Error::Unsupported(format!(
            "`IO::{builtin_name}` length must be an i32 expression"
        ))
    })?;
    Ok((element_type, length, static_length))
}

fn parse_array_element_type(
    term: &Term,
    builtin_name: &str,
    state: &LoweringState,
) -> Result<ArrayElementType> {
    let Term::Path(path) = term else {
        return Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` element type must be a simple path"
        )));
    };
    let [segment] = path.segments.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` element type must be a simple path"
        )));
    };
    let type_name = match state.environment.get(&segment.lexeme) {
        Some(Value::Type(Term::Path(bound_path)))
            if bound_path.token_keyword_package.is_none() && bound_path.segments.len() == 1 =>
        {
            bound_path.segments[0].lexeme.as_str()
        }
        _ => segment.lexeme.as_str(),
    };
    match type_name {
        "i32" => Ok(ArrayElementType::I32),
        "i64" => Ok(ArrayElementType::I64),
        "f32" => Ok(ArrayElementType::F32),
        "u8" => Ok(ArrayElementType::U8),
        _ => Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` currently supports only `i32`, `i64`, `f32`, and `u8` arrays"
        ))),
    }
}

fn parse_arg_arguments(
    arguments: &[Term],
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<I32Expr> {
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [arg_index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`IO::arg` must receive exactly one `i32` index".to_string(),
        ));
    };
    program.requires_argv = true;
    lower_i32_expr(arg_index, state)
        .map_err(|_| Error::Unsupported("`IO::arg` expects an `i32` index".to_string()))
}

fn is_i32_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].lexeme == "i32",
        _ => false,
    }
}

fn is_i64_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].lexeme == "i64",
        _ => false,
    }
}

fn is_f32_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].lexeme == "f32",
        _ => false,
    }
}

fn is_u8_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].lexeme == "u8",
        _ => false,
    }
}

pub(crate) fn parse_i32_literal_term(term: &Term, builtin_name: &str) -> Result<i32> {
    match term {
        Term::IntegerLiteral(literal) => parse_bare_i32_digits(literal, builtin_name),
        Term::Application { callee, arguments } => {
            let [suffix] = arguments.as_slice() else {
                return Err(Error::Unsupported(format!(
                    "`IO::{builtin_name}` length must be an `i32` literal"
                )));
            };
            if !is_i32_suffix(suffix) {
                return Err(Error::Unsupported(format!(
                    "`IO::{builtin_name}` length must be an `i32` literal"
                )));
            }
            let Term::IntegerLiteral(literal) = callee.as_ref() else {
                return Err(Error::Unsupported(format!(
                    "`IO::{builtin_name}` length must be an `i32` literal"
                )));
            };
            parse_bare_i32_digits(literal, builtin_name)
        }
        _ => Err(Error::Unsupported(format!(
            "`IO::{builtin_name}` length must be an `i32` literal"
        ))),
    }
}

fn parse_bare_i32_digits(literal: &str, builtin_name: &str) -> Result<i32> {
    parse_prefixed_i32_digits(literal).map_err(|_| {
        Error::Unsupported(format!(
            "`IO::{builtin_name}` length must be a valid `i32` literal"
        ))
    })
}

fn parse_prefixed_i32_digits(literal: &str) -> std::result::Result<i32, std::num::ParseIntError> {
    if let Some(hex) = literal
        .strip_prefix("0x")
        .or_else(|| literal.strip_prefix("0X"))
    {
        i32::from_str_radix(hex, 16)
    } else {
        literal.parse::<i32>()
    }
}

pub(crate) fn normalize_i32_literal_arguments(arguments: &[Term]) -> Vec<Term> {
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
