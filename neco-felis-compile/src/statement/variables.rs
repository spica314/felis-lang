use crate::{ArrayInfo, error::CompileError};
use neco_felis_syn::*;
use std::collections::HashMap;

use super::arithmetic;
use super::expressions;

#[allow(clippy::too_many_arguments)]
pub fn compile_let_statement(
    let_stmt: &StatementLet<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    let var_name = let_stmt.variable_name().to_string();
    *stack_offset += 8;
    let offset = *stack_offset;

    match &*let_stmt.value {
        ProcTerm::ConstructorCall(constructor_call) => {
            super::constructors::compile_proc_constructor_call_with_var(
                constructor_call,
                &var_name,
                arrays,
                output,
                stack_offset,
                variables,
                variable_arrays,
            )?;

            // The constructor call handles the variable registration internally
            Ok(())
        }
        ProcTerm::Apply(apply) => {
            // Check if this is a f32 arithmetic operation that should store f32 result
            if let ProcTerm::Variable(var) = &*apply.f
                && let Some(builtin) = builtins.get(var.variable.s())
            {
                match builtin.as_str() {
                    "f32_add" | "f32_sub" | "f32_mul" | "f32_div" => {
                        // These operations produce f32 results that should be stored as f32
                        arithmetic::compile_f32_arithmetic_for_let(
                            apply,
                            builtin,
                            variables,
                            arrays,
                            variable_arrays,
                            output,
                            offset,
                        )?;
                        variables.insert(var_name, offset);
                        return Ok(());
                    }
                    "u64_mod" => {
                        // Modulo operation stores remainder from rdx directly
                        arithmetic::compile_u64_mod_for_let(apply, variables, output, offset)?;
                        variables.insert(var_name, offset);
                        return Ok(());
                    }
                    _ => {}
                }
            }

            // Compile other expressions normally
            expressions::compile_proc_term(
                &let_stmt.value,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                output,
            )?;

            // Store the result from rax to the variable's stack location
            output.push_str(&format!(
                "    mov qword ptr [rbp - 8 - {}], rax\n",
                offset - 8
            ));

            variables.insert(var_name, offset);
            Ok(())
        }
        ProcTerm::Number(num) => {
            // Direct number assignment - store immediately without going through rax
            let number_value = parse_number(num.number.s());
            output.push_str(&format!(
                "    mov qword ptr [rbp - 8 - {}], {}\n",
                offset - 8,
                number_value
            ));
            variables.insert(var_name, offset);
            Ok(())
        }
        _ => {
            // Compile the expression
            expressions::compile_proc_term(
                &let_stmt.value,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                output,
            )?;

            // Store the result from rax to the variable's stack location
            output.push_str(&format!(
                "    mov qword ptr [rbp - 8 - {}], rax\n",
                offset - 8
            ));

            variables.insert(var_name, offset);
            Ok(())
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn compile_let_mut_statement(
    let_mut_stmt: &StatementLetMut<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &mut HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    let var_name = let_mut_stmt.variable_name().to_string();
    let ref_var_name = let_mut_stmt.reference_variable_name().to_string();

    // Allocate space for the value (8 bytes)
    *stack_offset += 8;
    let value_offset = *stack_offset;

    // Compile the initial value
    expressions::compile_proc_term(
        &let_mut_stmt.value,
        variables,
        reference_variables,
        builtins,
        arrays,
        variable_arrays,
        output,
    )?;

    // Store the initial value
    output.push_str(&format!(
        "    mov qword ptr [rbp - 8 - {}], rax\n",
        value_offset - 8
    ));

    // Allocate space for the reference (8 bytes) - pointer to the value
    *stack_offset += 8;
    let ref_offset = *stack_offset;

    // Calculate the address of the value and store it as the reference
    output.push_str(&format!(
        "    lea rax, qword ptr [rbp - 8 - {}]\n",
        value_offset - 8
    ));
    output.push_str(&format!(
        "    mov qword ptr [rbp - 8 - {}], rax\n",
        ref_offset - 8
    ));

    // Register both variables
    variables.insert(var_name.clone(), value_offset);
    variables.insert(ref_var_name.clone(), ref_offset);

    // Track that ref_var_name is a reference to var_name
    reference_variables.insert(ref_var_name, var_name);

    Ok(())
}

pub fn compile_assign_statement(
    assign_stmt: &StatementAssign<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    let var_name = assign_stmt.variable.s();

    // Check if this is a mutable variable (reference)
    if let Some(&ref_offset) = variables.get(var_name) {
        // Compile the value to assign
        expressions::compile_proc_term(
            &assign_stmt.value,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
            output,
        )?;

        // Load the reference (address of the value location)
        output.push_str(&format!(
            "    mov rbx, qword ptr [rbp - 8 - {}]\n",
            ref_offset - 8
        ));

        // Store the new value at the referenced location
        output.push_str("    mov qword ptr [rbx], rax\n");

        Ok(())
    } else {
        Err(CompileError::UnsupportedConstruct(format!(
            "Cannot assign to non-mutable variable: {var_name}"
        )))
    }
}

pub fn compile_field_assign_statement(
    field_assign_stmt: &StatementFieldAssign<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    // Compile the field access to get the address
    super::memory::compile_proc_field_access(
        &field_assign_stmt.field_access,
        variables,
        arrays,
        variable_arrays,
        output,
    )?;

    // Save the address in rbx
    output.push_str("    mov rbx, rax\n");

    // Compile the value to assign
    expressions::compile_proc_term(
        &field_assign_stmt.value,
        variables,
        reference_variables,
        builtins,
        arrays,
        variable_arrays,
        output,
    )?;

    // Store the value at the field address
    output.push_str("    mov qword ptr [rbx], rax\n");

    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub fn compile_return_statement(
    return_stmt: &StatementReturn<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    // Compile the return value expression
    expressions::compile_proc_term(
        &return_stmt.value,
        variables,
        reference_variables,
        builtins,
        arrays,
        variable_arrays,
        output,
    )?;

    // For procedures, the return value is expected to be in rax
    // The compile_proc_term already puts the result in rax, so no additional work needed
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
