use crate::{ArrayInfo, error::CompileError, syscall::SyscallCompiler};
use neco_felis_syn::*;
use std::collections::HashMap;

use super::StatementCompiler;
use super::arithmetic;
use super::control_flow;
use super::memory;

#[allow(clippy::too_many_arguments)]
pub fn compile_proc_term(
    proc_term: &ProcTerm<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    match proc_term {
        ProcTerm::Apply(apply) => compile_proc_apply(
            apply,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
            stack_offset,
            output,
        ),
        ProcTerm::Variable(var) => compile_proc_variable(var, variables, output),
        ProcTerm::Number(num) => compile_proc_number(num, output),
        ProcTerm::FieldAccess(field_access) => {
            // Convert field access to method chain parameters for compilation
            // (they use the same underlying logic)
            memory::compile_proc_field_access(
                field_access,
                variables,
                arrays,
                variable_arrays,
                output,
            )
        }
        ProcTerm::MethodChain(method_chain) => memory::compile_proc_method_chain(
            method_chain,
            variables,
            arrays,
            variable_arrays,
            output,
        ),
        ProcTerm::If(if_expr) => control_flow::compile_proc_if(
            if_expr,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
            output,
        ),
        ProcTerm::Match(match_term) => compile_proc_match(
            match_term,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
            stack_offset,
            output,
        ),
        ProcTerm::Paren(paren) => compile_proc_term(
            &paren.proc_term,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
            stack_offset,
            output,
        ),
        _ => Err(CompileError::UnsupportedConstruct(format!("{proc_term:?}"))),
    }
}

pub fn compile_proc_variable(
    var: &ProcTermVariable<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    let var_name = var.variable.s();

    // Check if the variable exists in our variable map
    if let Some(&offset) = variables.get(var_name) {
        // Load the variable value from its stack location into rax
        output.push_str(&format!(
            "    mov rax, qword ptr [rbp - 8 - {}]\n",
            offset - 8
        ));
        Ok(())
    } else {
        Err(CompileError::UnsupportedConstruct(format!(
            "Unknown variable: {var_name}"
        )))
    }
}

pub fn compile_proc_number(
    num: &ProcTermNumber<PhaseParse>,
    output: &mut String,
) -> Result<(), CompileError> {
    let number_value = parse_number(num.number.s());
    output.push_str(&format!("    mov rax, {number_value}\n"));
    Ok(())
}

fn parse_number(number_str: &str) -> String {
    if number_str.ends_with("u64") {
        number_str.trim_end_matches("u64").to_string()
    } else if number_str.ends_with("f32") {
        // For f32 numbers, we need to handle them specially
        let float_value = number_str.trim_end_matches("f32");
        // Convert to u32 representation for storage
        if let Ok(f) = float_value.parse::<f32>() {
            format!("0x{:08x}", f.to_bits())
        } else {
            float_value.to_string()
        }
    } else {
        number_str.to_string()
    }
}

fn lookup_variable_offset(name: &str, variables: &HashMap<String, i32>) -> Option<i32> {
    variables.get(name).copied().or_else(|| {
        name.rsplit_once('#')
            .and_then(|(base, _)| variables.get(base).copied())
    })
}

fn load_array_pointer(
    array_term: &ProcTerm<PhaseParse>,
    variables: &HashMap<String, i32>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(String, String), CompileError> {
    let object_name = if let ProcTerm::Variable(var) = array_term {
        var.variable.s().to_string()
    } else {
        return Err(CompileError::UnsupportedConstruct(format!(
            "Unsupported array reference: {array_term:?}"
        )));
    };

    let array_key = if variable_arrays.contains_key(&object_name) {
        object_name.clone()
    } else if let Some((base, _)) = object_name.rsplit_once('#')
        && variable_arrays.contains_key(base)
    {
        base.to_string()
    } else {
        object_name.clone()
    };

    let ptr_offset = lookup_variable_offset(&object_name, variables).ok_or_else(|| {
        CompileError::UnsupportedConstruct(format!("Unknown array variable: {object_name}"))
    })?;

    output.push_str(&format!(
        "    mov rax, qword ptr [rbp - 8 - {}]\n",
        ptr_offset - 8
    ));

    if let Some(info) = variable_arrays.get(&array_key)
        && info.starts_with("ref:")
    {
        output.push_str("    mov rax, qword ptr [rax]\n");
    }

    Ok((object_name, array_key))
}

fn apply_index_offset(
    index_term: &ProcTerm<PhaseParse>,
    element_size: usize,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    match index_term {
        ProcTerm::Number(num) => {
            let idx = crate::arrays::parse_number(num.number.s());
            if let Ok(idx_val) = idx.parse::<usize>() {
                let offset = idx_val.saturating_mul(element_size);
                if offset > 0 {
                    output.push_str(&format!("    add rax, {offset}\n"));
                }
            }
            Ok(())
        }
        ProcTerm::Variable(var) => {
            let var_name = var.variable.s();
            if let Some(var_offset) = lookup_variable_offset(var_name, variables) {
                output.push_str(&format!(
                    "    mov rbx, qword ptr [rbp - 8 - {}]\n",
                    var_offset - 8
                ));
                if element_size.is_power_of_two() {
                    let shift = element_size.trailing_zeros();
                    output.push_str(&format!("    shl rbx, {shift}\n"));
                } else {
                    output.push_str(&format!("    imul rbx, {element_size}\n"));
                }
                output.push_str("    add rax, rbx\n");
                Ok(())
            } else {
                Err(CompileError::UnsupportedConstruct(format!(
                    "Unknown index variable: {var_name}"
                )))
            }
        }
        other => Err(CompileError::UnsupportedConstruct(format!(
            "Unsupported index type: {other:?}"
        ))),
    }
}

fn compile_builtin_array_get(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    variable_arrays: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "array_get expects 2 arguments, got {}",
            apply.args.len()
        )));
    }
    let array_term = &apply.args[0];
    let index_term = &apply.args[1];

    let (_object_name, array_key) =
        load_array_pointer(array_term, variables, variable_arrays, output)?;
    let element_size =
        memory::resolve_array_element_size(&array_key, "get", variable_arrays, arrays).unwrap_or(8);

    apply_index_offset(index_term, element_size, variables, output)?;
    Ok(())
}

fn compile_builtin_array_set(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    variable_arrays: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 3 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "array_set expects 3 arguments, got {}",
            apply.args.len()
        )));
    }

    let array_term = &apply.args[0];
    let index_term = &apply.args[1];
    let value_term = &apply.args[2];

    let (_object_name, array_key) =
        load_array_pointer(array_term, variables, variable_arrays, output)?;
    let element_size =
        memory::resolve_array_element_size(&array_key, "set", variable_arrays, arrays).unwrap_or(8);

    apply_index_offset(index_term, element_size, variables, output)?;

    output.push_str("    mov rbx, rax\n");

    match value_term {
        ProcTerm::Number(num) => {
            let val = crate::arrays::parse_number(num.number.s());
            output.push_str(&format!("    mov rax, {val}\n"));
        }
        ProcTerm::Variable(var) => {
            let var_name = var.variable.s();
            if let Some(var_offset) = lookup_variable_offset(var_name, variables) {
                output.push_str(&format!(
                    "    mov rax, qword ptr [rbp - 8 - {}]\n",
                    var_offset - 8
                ));
            } else {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Unknown variable: {var_name}"
                )));
            }
        }
        _ => {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Unsupported value type for array set: {value_term:?}"
            )));
        }
    }

    output.push_str("    mov qword ptr [rbx], rax\n");
    Ok(())
}

fn compile_builtin_array_len(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    variable_arrays: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 1 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "array_len expects 1 argument, got {}",
            apply.args.len()
        )));
    }

    let object_name = if let ProcTerm::Variable(var) = &apply.args[0] {
        var.variable.s().to_string()
    } else {
        return Err(CompileError::UnsupportedConstruct(format!(
            "Unsupported array argument: {:?}",
            apply.args[0]
        )));
    };

    let mut array_key = object_name.clone();
    if let Some((base, _)) = object_name.rsplit_once('#')
        && (variable_arrays.contains_key(base) || arrays.contains_key(base))
    {
        array_key = base.to_string();
    }

    let mut size_owner = array_key.clone();
    if let Some(info) = variable_arrays.get(&array_key)
        && info.starts_with("ref:")
        && let Some(target) = reference_variables
            .get(&object_name)
            .or_else(|| reference_variables.get(&array_key))
            .cloned()
            .or_else(|| {
                object_name
                    .rsplit_once('#')
                    .and_then(|(base, _)| reference_variables.get(base).cloned())
            })
            .or_else(|| {
                array_key
                    .rsplit_once('#')
                    .and_then(|(base, _)| reference_variables.get(base).cloned())
            })
    {
        size_owner = target;
    }

    if let Some(array_info) = arrays.get(&array_key) {
        if let Some(size) = array_info.size {
            output.push_str(&format!("    mov rax, {size}\n"));
            return Ok(());
        } else {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Static array {array_key} has no size information"
            )));
        }
    }

    if variable_arrays.contains_key(&array_key) {
        let candidates = [
            format!("{size_owner}_size"),
            format!("{array_key}_size"),
            format!("{object_name}_size"),
        ];
        if let Some(&size_offset) = candidates.iter().find_map(|name| variables.get(name)) {
            output.push_str(&format!(
                "    mov rax, qword ptr [rbp - 8 - {}]\n",
                size_offset - 8
            ));
            return Ok(());
        }
    }

    if let Some(&size_offset) = variables
        .get(&format!("{size_owner}_size"))
        .or_else(|| variables.get(&format!("{array_key}_size")))
        .or_else(|| variables.get(&format!("{object_name}_size")))
    {
        output.push_str(&format!(
            "    mov rax, qword ptr [rbp - 8 - {}]\n",
            size_offset - 8
        ));
        return Ok(());
    }

    Err(CompileError::UnsupportedConstruct(format!(
        "Unknown array variable: {object_name}"
    )))
}

#[allow(clippy::too_many_arguments)]
fn compile_builtin_deref(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 1 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "deref expects 1 argument, got {}",
            apply.args.len()
        )));
    }

    let arg = &apply.args[0];
    compile_proc_term(
        arg,
        variables,
        reference_variables,
        builtins,
        arrays,
        variable_arrays,
        stack_offset,
        output,
    )?;
    output.push_str("    mov rax, qword ptr [rax]\n");
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub fn compile_proc_apply(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    if let ProcTerm::MethodChain(method_chain) = &*apply.f {
        return Err(CompileError::UnsupportedConstruct(format!(
            "Unsupported method chain apply: {}.{}",
            method_chain.object.s(),
            method_chain.field.s()
        )));
    }

    if let ProcTerm::Variable(var) = &*apply.f {
        if let Some(builtin) = builtins.get(var.variable.s()) {
            match builtin.as_str() {
                "array_get" => {
                    return compile_builtin_array_get(
                        apply,
                        variables,
                        variable_arrays,
                        arrays,
                        output,
                    );
                }
                "array_set" => {
                    return compile_builtin_array_set(
                        apply,
                        variables,
                        variable_arrays,
                        arrays,
                        output,
                    );
                }
                "array_len" => {
                    return compile_builtin_array_len(
                        apply,
                        variables,
                        reference_variables,
                        variable_arrays,
                        arrays,
                        output,
                    );
                }
                "deref" => {
                    return compile_builtin_deref(
                        apply,
                        variables,
                        reference_variables,
                        builtins,
                        arrays,
                        variable_arrays,
                        stack_offset,
                        output,
                    );
                }
                "syscall" => {
                    return SyscallCompiler::compile_proc_syscall(&apply.args, variables, output);
                }
                "u64_add" => return arithmetic::compile_u64_add_direct(apply, variables, output),
                "u64_sub" => return arithmetic::compile_u64_sub_direct(apply, variables, output),
                "u64_mul" => return arithmetic::compile_u64_mul_direct(apply, variables, output),
                "u64_div" => return arithmetic::compile_u64_div_direct(apply, variables, output),
                "u64_mod" => return arithmetic::compile_u64_mod_direct(apply, variables, output),
                "u64_eq" => return arithmetic::compile_u64_eq_direct(apply, variables, output),
                "f32_add" => {
                    return arithmetic::compile_f32_add_direct(
                        apply,
                        variables,
                        arrays,
                        variable_arrays,
                        output,
                    );
                }
                "f32_sub" => {
                    return arithmetic::compile_f32_sub_direct(
                        apply,
                        variables,
                        arrays,
                        variable_arrays,
                        output,
                    );
                }
                "f32_mul" => {
                    return arithmetic::compile_f32_mul_direct(
                        apply,
                        variables,
                        arrays,
                        variable_arrays,
                        output,
                    );
                }
                "f32_div" => {
                    return arithmetic::compile_f32_div_direct(
                        apply,
                        variables,
                        arrays,
                        variable_arrays,
                        output,
                    );
                }
                "f32_to_u64" => {
                    return arithmetic::compile_f32_to_u64_direct(
                        apply,
                        variables,
                        arrays,
                        variable_arrays,
                        output,
                    );
                }
                "u64_to_f32" => {
                    return arithmetic::compile_u64_to_f32_direct(
                        apply,
                        variables,
                        arrays,
                        variable_arrays,
                        output,
                    );
                }
                "u64" => return arithmetic::compile_u64_direct(apply, variables, output),
                "f32" => return arithmetic::compile_f32_direct(apply, variables, output),
                _ => {}
            }
        } else {
            // This is a call to a user-defined procedure
            let proc_name = var.variable.s();

            // Set up arguments in registers (following System V ABI)
            for (i, arg) in apply.args.iter().enumerate() {
                match i {
                    0 => SyscallCompiler::load_proc_argument_into_register(
                        arg, "rdi", variables, output,
                    )?,
                    1 => SyscallCompiler::load_proc_argument_into_register(
                        arg, "rsi", variables, output,
                    )?,
                    2 => SyscallCompiler::load_proc_argument_into_register(
                        arg, "rdx", variables, output,
                    )?,
                    3 => SyscallCompiler::load_proc_argument_into_register(
                        arg, "rcx", variables, output,
                    )?,
                    4 => SyscallCompiler::load_proc_argument_into_register(
                        arg, "r8", variables, output,
                    )?,
                    5 => SyscallCompiler::load_proc_argument_into_register(
                        arg, "r9", variables, output,
                    )?,
                    _ => {
                        return Err(CompileError::UnsupportedConstruct(
                            "More than 6 arguments not supported".to_string(),
                        ));
                    }
                }
            }

            // Call the user-defined procedure
            output.push_str(&format!("    call {proc_name}\n"));
            return Ok(());
        }
    }
    Err(CompileError::UnsupportedConstruct(format!("{apply:?}")))
}

#[allow(clippy::too_many_arguments)]
fn compile_proc_match(
    match_term: &ProcTermMatch<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    if let Some(branch) = match_term.branches.first() {
        let mut branch_variables = variables.clone();
        let mut branch_reference_variables = reference_variables.clone();
        let mut branch_variable_arrays = variable_arrays.clone();
        let scrutinee_name = match_term.scrutinee.s();
        for field in &branch.pattern.fields {
            let field_var_name = format!("{scrutinee_name}_{}", field.field_name.s());
            if let Some(&offset) = variables.get(&field_var_name) {
                branch_variables.insert(field.binder.s().to_string(), offset);
            }
            if let Some(array_info) = variable_arrays.get(&field_var_name).cloned() {
                branch_variable_arrays.insert(field.binder.s().to_string(), array_info);
            }
            if let Some(reference) = reference_variables.get(&field_var_name).cloned() {
                branch_reference_variables.insert(field.binder.s().to_string(), reference);
            }
        }
        StatementCompiler::compile_statements(
            &branch.body,
            &mut branch_variables,
            &mut branch_reference_variables,
            builtins,
            arrays,
            &mut branch_variable_arrays,
            stack_offset,
            output,
        )?;
    }
    Ok(())
}
