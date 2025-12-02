use crate::{ArrayInfo, error::CompileError};
use neco_felis_syn::*;
use std::collections::HashMap;

fn resolve_size_argument(
    size_term: &ProcTerm<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<String, CompileError> {
    match size_term {
        ProcTerm::Number(num) => Ok(crate::arrays::parse_number(num.number.s())),
        ProcTerm::Variable(var) => {
            let name = var.variable.s();
            let offset = variables
                .get(name)
                .copied()
                .or_else(|| {
                    name.rsplit_once('#')
                        .and_then(|(base, _)| variables.get(base).copied())
                })
                .ok_or_else(|| {
                    CompileError::UnsupportedConstruct(format!(
                        "Unknown variable in array size: {name}"
                    ))
                })?;
            output.push_str(&format!(
                "    mov rsi, qword ptr [rbp - 8 - {}]\n",
                offset - 8
            ));
            Ok("rsi".to_string())
        }
        _ => Err(CompileError::UnsupportedConstruct(format!(
            "Unsupported size argument type: {size_term:?}"
        ))),
    }
}

fn register_array_metadata(
    var_name: &str,
    entry: &str,
    variable_arrays: &mut HashMap<String, String>,
) {
    variable_arrays.insert(var_name.to_string(), entry.to_string());
    if let Some((base, _)) = var_name.rsplit_once('#') {
        variable_arrays
            .entry(base.to_string())
            .or_insert_with(|| entry.to_string());
    }
}

fn allocate_builtin_array(
    var_name: &str,
    size_term: &ProcTerm<PhaseParse>,
    element_type: &str,
    output: &mut String,
    stack_offset: &mut i32,
    variables: &mut HashMap<String, i32>,
    variable_arrays: &mut HashMap<String, String>,
) -> Result<(), CompileError> {
    let element_size = crate::arrays::get_type_size(element_type);
    let size_arg = resolve_size_argument(size_term, variables, output)?;

    crate::arrays::generate_builtin_array_allocation_with_var(
        var_name,
        &size_arg,
        element_size,
        output,
        stack_offset,
        variables,
    )?;

    let entry = format!("builtin:{element_type}");
    register_array_metadata(var_name, &entry, variable_arrays);
    Ok(())
}

fn resolve_element_type_from_arg(
    arg: &ProcTerm<PhaseParse>,
    builtins: &HashMap<String, String>,
) -> Result<String, CompileError> {
    match arg {
        ProcTerm::Variable(var) => Ok(builtins
            .get(var.variable.s())
            .cloned()
            .unwrap_or_else(|| var.variable.s().to_string())),
        _ => Err(CompileError::UnsupportedConstruct(format!(
            "Unsupported element type argument for array_new_with_size: {arg:?}"
        ))),
    }
}

pub fn compile_builtin_array_new_with_size_apply(
    apply: &ProcTermApply<PhaseParse>,
    var_name: &str,
    builtins: &HashMap<String, String>,
    output: &mut String,
    stack_offset: &mut i32,
    variables: &mut HashMap<String, i32>,
    variable_arrays: &mut HashMap<String, String>,
) -> Result<(), CompileError> {
    if apply.args.len() < 2 {
        return Err(CompileError::UnsupportedConstruct(
            "array_new_with_size expects element type and size arguments".to_string(),
        ));
    }

    let element_type = resolve_element_type_from_arg(&apply.args[0], builtins)?;
    let size_arg = &apply.args[1];

    allocate_builtin_array(
        var_name,
        size_arg,
        &element_type,
        output,
        stack_offset,
        variables,
        variable_arrays,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn compile_proc_constructor_call_with_var(
    constructor_call: &ProcTermConstructorCall<PhaseParse>,
    var_name: &str,
    arrays: &HashMap<String, ArrayInfo>,
    builtins: &HashMap<String, String>,
    output: &mut String,
    stack_offset: &mut i32,
    variables: &mut HashMap<String, i32>,
    variable_arrays: &mut HashMap<String, String>,
) -> Result<(), CompileError> {
    let type_name = constructor_call.type_name.s();
    let method_name = constructor_call.method.s();
    let normalized_method_name = method_name.trim_start_matches('_');

    // Handle builtin Array::new_with_size
    if let Some(bi) = builtins.get(type_name)
        && bi == "Array"
        && normalized_method_name == "new_with_size"
    {
        let element_type = resolve_element_type(constructor_call, builtins);
        let size_arg = constructor_call.args.first().ok_or_else(|| {
            CompileError::UnsupportedConstruct(
                "Missing size argument for array constructor".to_string(),
            )
        })?;

        allocate_builtin_array(
            var_name,
            size_arg,
            &element_type,
            output,
            stack_offset,
            variables,
            variable_arrays,
        )?;
        return Ok(());
    }

    let constructor_name = format!("{type_name}::{method_name}");

    if normalized_method_name == "new_with_size" {
        // Look up array information
        if let Some(array_info) = arrays.get(type_name).cloned() {
            // Register the variable to array type mapping
            variable_arrays.insert(var_name.to_string(), type_name.to_string());
            if let Some((base, _)) = var_name.rsplit_once('#') {
                variable_arrays
                    .entry(base.to_string())
                    .or_insert_with(|| type_name.to_string());
            }
            // Get the size argument
            let size_arg = if !constructor_call.args.is_empty()
                && let Some(arg) = constructor_call.args.first()
            {
                match arg {
                    ProcTerm::Number(num) => num.number.s().to_string(),
                    ProcTerm::Variable(var) => {
                        if let Some(&offset) = variables.get(var.variable.s()) {
                            // Load variable value into rsi for use by SoA allocation
                            output.push_str(&format!(
                                "    mov rsi, qword ptr [rbp - 8 - {}]\n",
                                offset - 8
                            ));
                            "rsi".to_string()
                        } else {
                            return Err(CompileError::UnsupportedConstruct(format!(
                                "Unknown variable in array size: {}",
                                var.variable.s()
                            )));
                        }
                    }
                    _ => {
                        return Err(CompileError::UnsupportedConstruct(format!(
                            "Unsupported size argument type: {arg:?}"
                        )));
                    }
                }
            } else {
                return Err(CompileError::UnsupportedConstruct(
                    "Missing size argument for array constructor".to_string(),
                ));
            };

            // Generate Structure of Arrays allocation using variable name
            crate::arrays::generate_soa_allocation_with_var(
                var_name,
                &array_info,
                &size_arg,
                output,
                stack_offset,
                variables,
            )?;

            Ok(())
        } else {
            Err(CompileError::UnsupportedConstruct(format!(
                "Array type not found: {type_name}"
            )))
        }
    } else {
        Err(CompileError::UnsupportedConstruct(format!(
            "Constructor call not yet implemented: {constructor_name}"
        )))
    }
}

pub fn compile_proc_constructor_call(
    constructor_call: &ProcTermConstructorCall<PhaseParse>,
    arrays: &HashMap<String, ArrayInfo>,
    // Note: builtins are not needed for expression context for now
    output: &mut String,
    stack_offset: &mut i32,
    variables: &mut HashMap<String, i32>,
) -> Result<(), CompileError> {
    let type_name = constructor_call.type_name.s();
    let method_name = constructor_call.method.s();
    let normalized_method_name = method_name.trim_start_matches('_');
    let constructor_name = format!("{type_name}::{method_name}");

    if normalized_method_name == "new_with_size" {
        // Look up array information
        if let Some(array_info) = arrays.get(type_name).cloned() {
            // Get the size argument
            let size_arg = if !constructor_call.args.is_empty()
                && let Some(arg) = constructor_call.args.first()
            {
                match arg {
                    ProcTerm::Number(num) => num.number.s().to_string(),
                    ProcTerm::Variable(var) => {
                        if let Some(&offset) = variables.get(var.variable.s()) {
                            // Load variable value into rsi for use by SoA allocation
                            output.push_str(&format!(
                                "    mov rsi, qword ptr [rbp - 8 - {}]\n",
                                offset - 8
                            ));
                            "rsi".to_string()
                        } else {
                            return Err(CompileError::UnsupportedConstruct(format!(
                                "Unknown variable in array size: {}",
                                var.variable.s()
                            )));
                        }
                    }
                    _ => {
                        return Err(CompileError::UnsupportedConstruct(format!(
                            "Unsupported size argument type: {arg:?}"
                        )));
                    }
                }
            } else {
                return Err(CompileError::UnsupportedConstruct(
                    "Missing size argument for array constructor".to_string(),
                ));
            };

            // Generate Structure of Arrays allocation
            crate::arrays::generate_soa_allocation(
                type_name,
                &array_info,
                &size_arg,
                output,
                stack_offset,
                variables,
                &mut HashMap::new(),
            )?;

            Ok(())
        } else {
            Err(CompileError::UnsupportedConstruct(format!(
                "Array type not found: {type_name}"
            )))
        }
    } else {
        Err(CompileError::UnsupportedConstruct(format!(
            "Constructor call not yet implemented: {constructor_name}"
        )))
    }
}

/// Resolve the element type for a constructor call like `Array T::new_with_size`.
/// Falls back to the raw name when the builtin alias is unknown.
fn resolve_element_type(
    constructor_call: &ProcTermConstructorCall<PhaseParse>,
    builtins: &HashMap<String, String>,
) -> String {
    constructor_call
        .type_args
        .first()
        .map(|arg| {
            builtins
                .get(arg.s())
                .cloned()
                .unwrap_or_else(|| arg.s().to_string())
        })
        // Default to u64-sized elements when no type argument is present
        .unwrap_or_else(|| "u64".to_string())
}
