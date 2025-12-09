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
    let mut offset = *stack_offset;

    match &*let_stmt.value {
        ProcTerm::StructValue(struct_value) => {
            // Expand struct value into per-field stack slots with names var_field
            let fields = &struct_value.fields;
            for (idx, field) in fields.iter().enumerate() {
                if idx > 0 {
                    *stack_offset += 8;
                    offset = *stack_offset;
                }
                // Compile field value
                super::expressions::compile_proc_term(
                    &field.value,
                    variables,
                    reference_variables,
                    builtins,
                    arrays,
                    variable_arrays,
                    stack_offset,
                    output,
                )?;

                // Store into stack
                output.push_str(&format!(
                    "    mov qword ptr [rbp - 8 - {}], rax\n",
                    offset - 8
                ));

                // Register variable with combined name
                let field_var_name = format!("{}_{}", var_name, field.name.s());
                variables.insert(field_var_name, offset);

                // Propagate array metadata if the source value is an array variable
                if let ProcTerm::Variable(src_var) = &*field.value
                    && let Some(array_info) = variable_arrays.get(src_var.variable.s()).cloned()
                {
                    let combined_name = format!("{}_{}", var_name, field.name.s());
                    variable_arrays.insert(combined_name, array_info);
                }

                // If the field value is a variable that has a corresponding size slot,
                // also propagate its size into a combined name slot: `{var_name}_{field}_size`.
                if let ProcTerm::Variable(v) = &*field.value {
                    let src_name = v.variable.s();
                    let src_size_name = format!("{src_name}_size");
                    if let Some(&src_size_off) = variables.get(&src_size_name) {
                        *stack_offset += 8;
                        let size_off = *stack_offset;
                        // Copy size value
                        output.push_str(&format!(
                            "    mov rax, qword ptr [rbp - 8 - {}]\n",
                            src_size_off - 8
                        ));
                        output.push_str(&format!(
                            "    mov qword ptr [rbp - 8 - {}], rax\n",
                            size_off - 8
                        ));
                        // Register size slot
                        let combined_size_name = format!("{}_{}_size", var_name, field.name.s());
                        variables.insert(combined_size_name, size_off);
                    }
                }
            }
            // Also propagate a common size for the struct itself (e.g., `points_size`)
            for field in fields {
                let combined_size_name = format!("{}_{}_size", var_name, field.name.s());
                if let Some(&src_size_off) = variables.get(&combined_size_name) {
                    *stack_offset += 8;
                    let size_off = *stack_offset;
                    output.push_str(&format!(
                        "    mov rax, qword ptr [rbp - 8 - {}]\n",
                        src_size_off - 8
                    ));
                    output.push_str(&format!(
                        "    mov qword ptr [rbp - 8 - {}], rax\n",
                        size_off - 8
                    ));
                    let struct_size_name = format!("{var_name}_size");
                    variables.insert(struct_size_name, size_off);
                    break;
                }
            }
            // Do not register `var_name` directly; fields are accessible via `var_name_field`
            Ok(())
        }
        ProcTerm::Apply(apply) => {
            if let ProcTerm::Variable(var) = &*apply.f
                && let Some(builtin) = builtins.get(var.variable.s())
                && builtin == "array_new_with_size"
            {
                super::constructors::compile_builtin_array_new_with_size_apply(
                    apply,
                    &var_name,
                    builtins,
                    output,
                    stack_offset,
                    variables,
                    variable_arrays,
                )?;
                return Ok(());
            }

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
                stack_offset,
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
                stack_offset,
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

    let mut value_offset: Option<i32> = None;

    // Handle constructor calls specially so we can reuse array allocation logic
    if let ProcTerm::Apply(apply) = &*let_mut_stmt.value
        && let ProcTerm::Variable(var) = &*apply.f
        && let Some(builtin) = builtins.get(var.variable.s())
        && builtin == "array_new_with_size"
    {
        super::constructors::compile_builtin_array_new_with_size_apply(
            apply,
            &var_name,
            builtins,
            output,
            stack_offset,
            variables,
            variable_arrays,
        )?;

        value_offset = Some(*variables.get(&var_name).ok_or_else(|| {
            CompileError::UnsupportedConstruct(format!(
                "Failed to register variable for array allocation: {var_name}"
            ))
        })?);
    }

    if let Some(value_offset) = value_offset {
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
        variables.insert(ref_var_name.clone(), ref_offset);
        reference_variables.insert(ref_var_name.clone(), var_name.clone());

        if let Some((base, _)) = var_name.rsplit_once('#') {
            variables.entry(base.to_string()).or_insert(value_offset);
        }
        if let Some((base, _)) = ref_var_name.rsplit_once('#') {
            variables.entry(base.to_string()).or_insert(ref_offset);
        }

        if let Some(array_info) = variable_arrays.get(&var_name).cloned() {
            let entry = format!("ref:{array_info}");
            variable_arrays.insert(ref_var_name.to_string(), entry.clone());
            if let Some((base, _)) = ref_var_name.rsplit_once('#') {
                variable_arrays.entry(base.to_string()).or_insert(entry);
            }
        }

        return Ok(());
    }

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
        stack_offset,
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
    if let Some((base, _)) = var_name.rsplit_once('#') {
        variables.entry(base.to_string()).or_insert(value_offset);
    }
    if let Some((base, _)) = ref_var_name.rsplit_once('#') {
        variables.entry(base.to_string()).or_insert(ref_offset);
    }

    // Track that ref_var_name is a reference to var_name
    reference_variables.insert(ref_var_name.clone(), var_name.clone());

    // Propagate array metadata so references to arrays can be used directly
    if let Some(array_info) = variable_arrays.get(&var_name).cloned() {
        let entry = format!("ref:{array_info}");
        variable_arrays.insert(ref_var_name.clone(), entry.clone());
        if let Some((base, _)) = ref_var_name.rsplit_once('#') {
            variable_arrays.entry(base.to_string()).or_insert(entry);
        }
    }

    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub fn compile_assign_statement(
    assign_stmt: &StatementAssign<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
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
            stack_offset,
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

#[allow(clippy::too_many_arguments)]
pub fn compile_field_assign_statement(
    method_chain_assign_stmt: &StatementMethodChainAssign<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    // Compile the field access to get the address
    super::memory::compile_proc_method_chain(
        &method_chain_assign_stmt.method_chain,
        variables,
        arrays,
        variable_arrays,
        output,
    )?;

    // Save the address in rbx
    output.push_str("    mov rbx, rax\n");

    // Fast-path: if the value is an f32 literal, store 4 bytes
    if let ProcTerm::Number(num) = &*method_chain_assign_stmt.value {
        let number_str = num.number.s();
        if let Some(float_value) = number_str.strip_suffix("f32")
            && let Ok(f) = float_value.parse::<f32>()
        {
            output.push_str(&format!("    mov ebx, 0x{:08x}\n", f.to_bits()));
            output.push_str("    mov dword ptr [rbx], ebx\n");
            return Ok(());
        }
    }

    // Compile the value to assign (generic path -> 8-byte store)
    expressions::compile_proc_term(
        &method_chain_assign_stmt.value,
        variables,
        reference_variables,
        builtins,
        arrays,
        variable_arrays,
        stack_offset,
        output,
    )?;

    // Store the value at the field address (8 bytes)
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
    stack_offset: &mut i32,
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
        stack_offset,
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
