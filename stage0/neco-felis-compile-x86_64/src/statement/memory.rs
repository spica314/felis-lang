use crate::{ArrayInfo, error::CompileError};
use neco_felis_syn::*;
use std::collections::HashMap;

pub fn load_proc_argument_into_register(
    arg: &ProcTerm<PhaseParse>,
    register: &str,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    match arg {
        ProcTerm::Number(num) => {
            let number_value = parse_number(num.number.s());
            output.push_str(&format!("    mov {register}, {number_value}\n"));
        }
        ProcTerm::FieldAccess(field_access) => {
            // Support local struct-like variables (object_field) or fall back to address load
            let object_name = field_access.object.s();
            let field_name = field_access.field.s();
            let combined = format!("{object_name}_{field_name}");
            if let Some(&var_offset) = variables.get(&combined) {
                output.push_str(&format!(
                    "    mov {register}, qword ptr [rbp - 8 - {}]\n",
                    var_offset - 8
                ));
            } else {
                // Compute address then load
                compile_proc_field_access(
                    field_access,
                    variables,
                    &HashMap::new(),
                    &HashMap::new(),
                    output,
                )?;
                output.push_str(&format!("    mov {register}, qword ptr [rax]\n"));
            }
        }
        ProcTerm::Variable(var) => {
            let var_name = var.variable.s();
            if let Some(&var_offset) = variables.get(var_name) {
                output.push_str(&format!(
                    "    mov {register}, qword ptr [rbp - 8 - {}]\n",
                    var_offset - 8
                ));
            } else {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Unknown variable: {var_name}"
                )));
            }
        }
        ProcTerm::Paren(paren) => {
            // Handle parenthesized expressions by delegating to the inner term
            load_proc_argument_into_register(&paren.proc_term, register, variables, output)?;
        }
        _ => {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Unsupported argument type: {arg:?}"
            )));
        }
    }
    Ok(())
}

pub fn load_f32_proc_argument_into_register(
    arg: &ProcTerm<PhaseParse>,
    register: &str,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    match arg {
        ProcTerm::Number(num) => {
            let number_str = num.number.s();
            if let Some(float_value) = number_str.strip_suffix("f32") {
                // Use direct encoding
                let f = float_value.parse::<f32>().unwrap_or(0.0);
                output.push_str(&format!("    mov eax, 0x{:08x}\n", f.to_bits()));
                output.push_str(&format!("    movd {register}, eax\n"));
            } else {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Expected f32 number, got: {number_str}"
                )));
            }
        }
        ProcTerm::FieldAccess(field_access) => {
            let object_name = field_access.object.s();
            let field_name = field_access.field.s();
            let combined = format!("{object_name}_{field_name}");
            if let Some(&var_offset) = variables.get(&combined) {
                output.push_str(&format!(
                    "    movss {register}, dword ptr [rbp - 8 - {}]\n",
                    var_offset - 8
                ));
            } else {
                // Compute address then load as f32
                compile_proc_field_access(
                    field_access,
                    variables,
                    arrays,
                    variable_arrays,
                    output,
                )?;
                output.push_str(&format!("    movss {register}, dword ptr [rax]\n"));
            }
        }
        ProcTerm::Variable(var) => {
            let var_name = var.variable.s();
            if let Some(&var_offset) = variables.get(var_name) {
                // Load f32 value from memory - this handles both f32 variables stored as f32
                // and u64 variables that need to be interpreted as f32
                output.push_str(&format!(
                    "    movss {register}, dword ptr [rbp - 8 - {}]\n",
                    var_offset - 8
                ));
            } else {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Unknown variable: {var_name}"
                )));
            }
        }
        ProcTerm::Paren(paren) => {
            // Handle parenthesized expressions by delegating to the inner term
            load_f32_proc_argument_into_register(
                &paren.proc_term,
                register,
                variables,
                arrays,
                variable_arrays,
                output,
            )?;
        }
        _ => {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Unsupported f32 argument type: {arg:?}"
            )));
        }
    }
    Ok(())
}

pub fn compile_proc_field_access(
    field_access: &ProcTermFieldAccess<PhaseParse>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    // Field access is essentially the same as method chain,
    // just parsed differently (no whitespace before dot)
    // We can reuse the same logic by extracting the common parts
    let object_name = field_access.object.s();
    let field_name = field_access.field.s();

    compile_field_or_method_impl(
        object_name,
        field_name,
        field_access.index.as_deref(),
        variables,
        arrays,
        variable_arrays,
        output,
    )
}

pub fn compile_proc_method_chain(
    method_chain: &ProcTermMethodChain<PhaseParse>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    let object_name = method_chain.object.s();
    let field_name = method_chain.field.s();

    compile_field_or_method_impl(
        object_name,
        field_name,
        method_chain.index.as_deref(),
        variables,
        arrays,
        variable_arrays,
        output,
    )
}

fn compile_field_or_method_impl(
    object_name: &str,
    field_name: &str,
    index: Option<&ProcTerm<PhaseParse>>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    let lookup_offset = |name: &str| {
        variables.get(name).copied().or_else(|| {
            name.rsplit_once('#')
                .and_then(|(base, _)| variables.get(base).copied())
        })
    };

    // Support local "struct-like" variables expanded as separate stack slots named object_field
    let local_field_var = format!("{object_name}_{field_name}");
    if let Some(&field_offset) = variables.get(&local_field_var) {
        // Treat local field slot as holding a pointer to the start of the array
        output.push_str(&format!(
            "    mov rax, qword ptr [rbp - 8 - {}]\n",
            field_offset - 8
        ));

        let element_size =
            resolve_array_element_size(&local_field_var, field_name, variable_arrays, arrays)
                .or_else(|| {
                    resolve_array_element_size(object_name, field_name, variable_arrays, arrays)
                })
                .unwrap_or(8);

        // Apply index if present (default 8-byte stride)
        if let Some(index_term) = index {
            match index_term {
                ProcTerm::Number(num) => {
                    let index = parse_number(num.number.s());
                    if let Ok(idx_val) = index.parse::<usize>() {
                        let offset = idx_val.saturating_mul(element_size);
                        if offset > 0 {
                            output.push_str(&format!("    add rax, {offset}\n"));
                        }
                    }
                }
                ProcTerm::Variable(var) => {
                    if let Some(var_offset) = lookup_offset(var.variable.s()) {
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
                    }
                }
                _ => {}
            }
        }
        return Ok(());
    }
    // Check if this is a Structure of Arrays (SoA) access
    let soa_ptr_var_name = format!("{object_name}_{field_name}_ptr");
    if let Some(&ptr_offset) = variables.get(&soa_ptr_var_name) {
        // This is SoA access - load the field array pointer
        output.push_str(&format!(
            "    mov rax, qword ptr [rbp - 8 - {}]\n",
            ptr_offset - 8
        ));

        // Handle index if present
        if let Some(index_term) = index {
            // Get array info for element size calculation
            if let Some(array_type_name) = variable_arrays.get(object_name)
                && let Some(array_info) = arrays.get(array_type_name)
            {
                let element_size = crate::arrays::get_element_size(
                    &array_info.field_types,
                    &array_info.field_names,
                    field_name,
                )?;

                match index_term {
                    ProcTerm::Number(num) => {
                        let index = crate::arrays::parse_number(num.number.s());
                        let offset = index.parse::<usize>().unwrap_or(0) * element_size;
                        if offset > 0 {
                            output.push_str(&format!("    add rax, {offset}\n"));
                        }
                    }
                    ProcTerm::Variable(var) => {
                        if let Some(&var_offset) = variables.get(var.variable.s()) {
                            output.push_str(&format!(
                                "    mov rbx, qword ptr [rbp - 8 - {}]\n",
                                var_offset - 8
                            ));
                            output.push_str(&format!("    mov rcx, {element_size}\n"));
                            output.push_str("    imul rbx, rcx\n");
                            output.push_str("    add rax, rbx\n");
                        }
                    }
                    _ => {
                        return Err(CompileError::UnsupportedConstruct(format!(
                            "Unsupported index type in field access: {index_term:?}"
                        )));
                    }
                }
            }
        }
        // rax now contains the address of the field element in the SoA
        Ok(())
    } else {
        let available: Vec<_> = variables.keys().cloned().collect();
        Err(CompileError::UnsupportedConstruct(format!(
            "Unknown variable: {object_name} (candidates: {available:?})"
        )))
    }
}

pub fn resolve_array_element_type(
    key: &str,
    field_name: &str,
    variable_arrays: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
) -> Option<String> {
    if let Some(info) = variable_arrays.get(key) {
        let info = info.strip_prefix("ref:").unwrap_or(info.as_str());
        if let Some(elem) = info.strip_prefix("builtin:") {
            return Some(elem.to_string());
        }
        if let Some(array_info) = arrays.get(info) {
            return crate::arrays::get_field_type(
                &array_info.field_types,
                &array_info.field_names,
                field_name,
            )
            .ok();
        }
    }
    None
}

pub fn resolve_array_element_size(
    key: &str,
    field_name: &str,
    variable_arrays: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
) -> Option<usize> {
    resolve_array_element_type(key, field_name, variable_arrays, arrays)
        .map(|ty| crate::arrays::get_type_size(&ty))
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
