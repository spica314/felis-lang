use crate::error::CompileError;
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
