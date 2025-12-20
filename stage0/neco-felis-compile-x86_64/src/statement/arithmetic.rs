use crate::{ArrayInfo, error::CompileError};
use neco_felis_syn::*;
use std::collections::HashMap;

use super::memory;

// Direct arithmetic compilation functions that return results in rax
pub fn compile_u64_add_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_add expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into rax
    memory::load_proc_argument_into_register(arg1, "rax", variables, output)?;
    // Load second argument into rbx and add to rax
    memory::load_proc_argument_into_register(arg2, "rbx", variables, output)?;
    output.push_str("    add rax, rbx\n");

    Ok(())
}

pub fn compile_u64_sub_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_sub expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into rax
    memory::load_proc_argument_into_register(arg1, "rax", variables, output)?;
    // Load second argument into rbx and subtract from rax
    memory::load_proc_argument_into_register(arg2, "rbx", variables, output)?;
    output.push_str("    sub rax, rbx\n");

    Ok(())
}

pub fn compile_u64_mul_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_mul expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into rax
    memory::load_proc_argument_into_register(arg1, "rax", variables, output)?;
    // Load second argument into rbx and multiply with rax
    memory::load_proc_argument_into_register(arg2, "rbx", variables, output)?;
    output.push_str("    imul rax, rbx\n");

    Ok(())
}

pub fn compile_u64_div_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_div expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into rax
    memory::load_proc_argument_into_register(arg1, "rax", variables, output)?;
    // Load second argument into rbx
    memory::load_proc_argument_into_register(arg2, "rbx", variables, output)?;
    // Clear rdx for division
    output.push_str("    xor rdx, rdx\n");
    // Divide rax by rbx, result in rax
    output.push_str("    div rbx\n");

    Ok(())
}

pub fn compile_u64_mod_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_mod expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into rax
    memory::load_proc_argument_into_register(arg1, "rax", variables, output)?;
    // Load second argument into rbx
    memory::load_proc_argument_into_register(arg2, "rbx", variables, output)?;
    // Clear rdx for division
    output.push_str("    xor rdx, rdx\n");
    // Divide rax by rbx, remainder in rdx
    output.push_str("    div rbx\n");
    // For modulo, result is in rdx, move to rax
    output.push_str("    mov rax, rdx\n");

    Ok(())
}

pub fn compile_u64_eq_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_eq expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into rax
    memory::load_proc_argument_into_register(arg1, "rax", variables, output)?;
    // Load second argument into rbx
    memory::load_proc_argument_into_register(arg2, "rbx", variables, output)?;
    // Compare the values
    output.push_str("    cmp rax, rbx\n");
    // Set rax to 1 if equal, 0 if not equal
    output.push_str("    sete al\n");
    output.push_str("    movzx rax, al\n");

    Ok(())
}

pub fn compile_f32_add_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "f32_add expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into xmm0
    memory::load_f32_proc_argument_into_register(
        arg1,
        "xmm0",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    // Load second argument into xmm1 and add to xmm0
    memory::load_f32_proc_argument_into_register(
        arg2,
        "xmm1",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    output.push_str("    addss xmm0, xmm1\n");
    // Move result to rax (as 32-bit value)
    output.push_str("    movd eax, xmm0\n");
    output.push_str("    mov rax, rax\n"); // Clear upper 32 bits

    Ok(())
}

pub fn compile_f32_sub_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "f32_sub expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into xmm0
    memory::load_f32_proc_argument_into_register(
        arg1,
        "xmm0",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    // Load second argument into xmm1 and subtract from xmm0
    memory::load_f32_proc_argument_into_register(
        arg2,
        "xmm1",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    output.push_str("    subss xmm0, xmm1\n");
    // Move result to rax (as 32-bit value)
    output.push_str("    movd eax, xmm0\n");
    output.push_str("    mov rax, rax\n"); // Clear upper 32 bits

    Ok(())
}

pub fn compile_f32_mul_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "f32_mul expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into xmm0
    memory::load_f32_proc_argument_into_register(
        arg1,
        "xmm0",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    // Load second argument into xmm1 and multiply with xmm0
    memory::load_f32_proc_argument_into_register(
        arg2,
        "xmm1",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    output.push_str("    mulss xmm0, xmm1\n");
    // Move result to rax (as 32-bit value)
    output.push_str("    movd eax, xmm0\n");
    output.push_str("    mov rax, rax\n"); // Clear upper 32 bits

    Ok(())
}

pub fn compile_f32_div_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "f32_div expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into xmm0
    memory::load_f32_proc_argument_into_register(
        arg1,
        "xmm0",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    // Load second argument into xmm1 and divide xmm0 by xmm1
    memory::load_f32_proc_argument_into_register(
        arg2,
        "xmm1",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    output.push_str("    divss xmm0, xmm1\n");
    // Move result to rax (as 32-bit value)
    output.push_str("    movd eax, xmm0\n");
    output.push_str("    mov rax, rax\n"); // Clear upper 32 bits

    Ok(())
}

pub fn compile_f32_to_u64_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 1 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "f32_to_u64 expects 1 argument, got {}",
            apply.args.len()
        )));
    }

    let arg = &apply.args[0];

    // Load f32 argument into xmm0
    memory::load_f32_proc_argument_into_register(
        arg,
        "xmm0",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;

    // Convert f32 to u64
    output.push_str("    cvttss2si rax, xmm0\n");

    Ok(())
}

pub fn compile_u64_to_f32_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    _arrays: &HashMap<String, ArrayInfo>,
    _variable_arrays: &HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 1 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_to_f32 expects 1 argument, got {}",
            apply.args.len()
        )));
    }

    let arg = &apply.args[0];

    // Load u64 argument into rax
    memory::load_proc_argument_into_register(arg, "rax", variables, output)?;

    // Convert u64 to f32
    output.push_str("    cvtsi2ss xmm0, rax\n");
    // Move result to rax (as 32-bit value)
    output.push_str("    movd eax, xmm0\n");
    output.push_str("    mov rax, rax\n"); // Clear upper 32 bits

    Ok(())
}

pub fn compile_u64_direct(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 1 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64 expects 1 argument, got {}",
            apply.args.len()
        )));
    }

    let arg = &apply.args[0];

    // Load argument value
    memory::load_proc_argument_into_register(arg, "rax", variables, output)?;

    Ok(())
}

pub fn compile_f32_direct(
    apply: &ProcTermApply<PhaseParse>,
    _variables: &HashMap<String, i32>,
    output: &mut String,
) -> Result<(), CompileError> {
    if apply.args.len() != 1 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "f32 expects 1 argument, got {}",
            apply.args.len()
        )));
    }

    let arg = &apply.args[0];

    // Load f32 argument
    match arg {
        ProcTerm::Number(num) => {
            let number_str = num.number.s();
            let float_value = number_str.parse::<f32>().unwrap_or(0.0);
            output.push_str(&format!("    mov eax, 0x{:08x}\n", float_value.to_bits()));
            output.push_str("    mov rax, rax\n"); // Clear upper 32 bits
        }
        _ => {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32 expects numeric literal, got: {arg:?}"
            )));
        }
    }

    Ok(())
}

pub fn compile_f32_arithmetic_for_let(
    apply: &ProcTermApply<PhaseParse>,
    builtin: &str,
    variables: &HashMap<String, i32>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &HashMap<String, String>,
    output: &mut String,
    offset: i32,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "{builtin} expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into xmm0
    memory::load_f32_proc_argument_into_register(
        arg1,
        "xmm0",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;
    // Load second argument into xmm1
    memory::load_f32_proc_argument_into_register(
        arg2,
        "xmm1",
        variables,
        arrays,
        variable_arrays,
        output,
    )?;

    // Perform the operation
    match builtin {
        "f32_add" => output.push_str("    addss xmm0, xmm1\n"),
        "f32_sub" => output.push_str("    subss xmm0, xmm1\n"),
        "f32_mul" => output.push_str("    mulss xmm0, xmm1\n"),
        "f32_div" => output.push_str("    divss xmm0, xmm1\n"),
        _ => {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Unknown f32 operation: {builtin}"
            )));
        }
    }

    // Store result from xmm0 to the variable's stack location as f32
    output.push_str(&format!(
        "    movss dword ptr [rbp - 8 - {}], xmm0\n",
        offset - 8
    ));

    Ok(())
}

pub fn compile_u64_mod_for_let(
    apply: &ProcTermApply<PhaseParse>,
    variables: &HashMap<String, i32>,
    output: &mut String,
    offset: i32,
) -> Result<(), CompileError> {
    if apply.args.len() != 2 {
        return Err(CompileError::UnsupportedConstruct(format!(
            "u64_mod expects 2 arguments, got {}",
            apply.args.len()
        )));
    }

    let arg1 = &apply.args[0];
    let arg2 = &apply.args[1];

    // Load first argument into rax
    memory::load_proc_argument_into_register(arg1, "rax", variables, output)?;
    // Load second argument into rbx
    memory::load_proc_argument_into_register(arg2, "rbx", variables, output)?;
    // Clear rdx for division
    output.push_str("    xor rdx, rdx\n");
    // Divide rax by rbx, remainder in rdx
    output.push_str("    div rbx\n");
    // Store remainder from rdx directly to the variable's stack location
    output.push_str(&format!(
        "    mov qword ptr [rbp - 8 - {}], rdx\n",
        offset - 8
    ));

    Ok(())
}
