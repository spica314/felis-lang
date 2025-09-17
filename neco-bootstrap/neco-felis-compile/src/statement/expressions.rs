use crate::{ArrayInfo, error::CompileError, syscall::SyscallCompiler};
use neco_felis_syn::*;
use std::collections::HashMap;

use super::arithmetic;
use super::constructors;
use super::control_flow;
use super::memory;

pub fn compile_proc_term(
    proc_term: &ProcTerm<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    match proc_term {
        ProcTerm::Apply(apply) => {
            compile_proc_apply(apply, variables, builtins, arrays, variable_arrays, output)
        }
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
        ProcTerm::ConstructorCall(constructor_call) => constructors::compile_proc_constructor_call(
            constructor_call,
            arrays,
            output,
            &mut 0,
            &mut HashMap::new(),
        ),
        ProcTerm::Dereference(dereference) => memory::compile_proc_dereference(
            dereference,
            variables,
            reference_variables,
            builtins,
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
        ProcTerm::Paren(paren) => compile_proc_term(
            &paren.proc_term,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
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

pub fn compile_proc_apply(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    // Handle field access apply (e.g., points.#len ())
    if let ProcTerm::MethodChain(method_chain) = &*apply.f {
        // Check if this is an array length operation
        if method_chain.field.s() == "#len" || method_chain.field.s() == "len" {
            // Ensure we have no arguments or only unit argument
            if apply.args.len() > 1
                || (apply.args.len() == 1 && !matches!(apply.args[0], ProcTerm::Unit(_)))
            {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Array length operation expects no arguments or unit argument, got {} args",
                    apply.args.len()
                )));
            }

            // Generate code to get array length
            let array_name = method_chain.object.s();
            if let Some(array_info) = arrays.get(array_name) {
                // Static array - use compile-time size if available
                if let Some(size) = array_info.size {
                    output.push_str(&format!("    mov rax, {size}\n"));
                    return Ok(());
                } else {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "Static array {array_name} has no size information"
                    )));
                }
            } else if let Some(_array_type) = variable_arrays.get(array_name) {
                // This is a variable that holds a dynamically allocated array
                // The size is stored in a variable named "{array_name}_size"
                let size_var_name = format!("{array_name}_size");
                if let Some(&size_offset) = variables.get(&size_var_name) {
                    // Load the array size from memory into rax
                    output.push_str(&format!(
                        "    mov rax, qword ptr [rbp - 8 - {}]\n",
                        size_offset - 8
                    ));
                    return Ok(());
                } else {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "Size variable not found for array: {array_name}"
                    )));
                }
            } else if let Some(&size_offset) = variables.get(&format!("{array_name}_size")) {
                // Fallback for struct objects: use propagated `{object}_size`
                output.push_str(&format!(
                    "    mov rax, qword ptr [rbp - 8 - {}]\n",
                    size_offset - 8
                ));
                return Ok(());
            } else {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Unknown array variable: {array_name}"
                )));
            }
        } else {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Unsupported field access operation: {}",
                method_chain.field.s()
            )));
        }
    }

    if let ProcTerm::Variable(var) = &*apply.f {
        if let Some(builtin) = builtins.get(var.variable.s()) {
            match builtin.as_str() {
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
