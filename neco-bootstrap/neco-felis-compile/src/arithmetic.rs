use crate::{AssemblyCompiler, CompileError};
use neco_felis_syn::*;

/// U64 arithmetic operations for let statements
impl AssemblyCompiler {
    pub fn compile_u64_add_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // u64_add expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_add expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into rax
        self.load_argument_into_register(arg1, "rax")?;

        // Load second argument into rbx and add to rax
        self.load_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    add rax, rbx\n");

        // Store result from rax to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_sub_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // u64_sub expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_sub expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into rax
        self.load_argument_into_register(arg1, "rax")?;

        // Load second argument into rbx and subtract from rax
        self.load_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    sub rax, rbx\n");

        // Store result from rax to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_mul_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // u64_mul expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_mul expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into rax
        self.load_argument_into_register(arg1, "rax")?;

        // Load second argument into rbx and multiply with rax
        self.load_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    imul rax, rbx\n");

        // Store result from rax to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_div_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // u64_div expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_div expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into rax
        self.load_argument_into_register(arg1, "rax")?;

        // Load second argument into rbx
        self.load_argument_into_register(arg2, "rbx")?;

        // Clear rdx for division
        self.output.push_str("    xor rdx, rdx\n");
        // Divide rax by rbx, result in rax
        self.output.push_str("    div rbx\n");

        // Store result from rax to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_mod_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // u64_mod expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_mod expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into rax
        self.load_argument_into_register(arg1, "rax")?;

        // Load second argument into rbx
        self.load_argument_into_register(arg2, "rbx")?;

        // Clear rdx for division
        self.output.push_str("    xor rdx, rdx\n");
        // Divide rax by rbx, remainder in rdx
        self.output.push_str("    div rbx\n");

        // Store remainder from rdx to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rdx\n",
            offset - 8
        ));

        Ok(())
    }

    /// F32 arithmetic operations for let statements
    pub fn compile_f32_add_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // f32_add expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_add expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into xmm0
        self.load_f32_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1 and add to xmm0
        self.load_f32_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    addss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_sub_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // f32_sub expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_sub expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into xmm0
        self.load_f32_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1 and subtract from xmm0
        self.load_f32_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    subss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_mul_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // f32_mul expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_mul expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into xmm0
        self.load_f32_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1 and multiply with xmm0
        self.load_f32_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    mulss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_div_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // f32_div expects exactly 2 arguments
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_div expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        // Load first argument into xmm0
        self.load_f32_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1
        self.load_f32_argument_into_register(arg2, "xmm1")?;

        // Divide xmm0 by xmm1
        self.output.push_str("    divss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_to_u64_let(
        &mut self,
        apply: &TermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        // f32_to_u64 expects exactly 1 argument
        if apply.args.len() != 1 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_to_u64 expects 1 argument, got {}",
                apply.args.len()
            )));
        }

        let arg = &apply.args[0];

        // Load f32 argument into xmm0
        self.load_f32_argument_into_register(arg, "xmm0")?;

        // Convert f32 to u64
        self.output.push_str("    cvttss2si rax, xmm0\n");

        // Store result from rax to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    /// U64 arithmetic operations for proc statements
    pub fn compile_u64_add_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_add expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    add rax, rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_sub_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_sub expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    sub rax, rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_mul_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_mul expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    imul rax, rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_div_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_div expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    xor rdx, rdx\n"); // Clear rdx for unsigned division
        self.output.push_str("    div rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_mod_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
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

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    xor rdx, rdx\n"); // Clear rdx for unsigned division
        self.output.push_str("    div rbx\n");
        // For modulo, result is in rdx
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rdx\n",
            offset - 8
        ));

        Ok(())
    }

    /// F32 arithmetic operations for proc statements
    pub fn compile_f32_add_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
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
        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1 and add to xmm0
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    addss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_sub_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
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
        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1 and subtract from xmm0
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    subss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_mul_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
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
        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1 and multiply with xmm0
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    mulss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_div_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
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
        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;

        // Load second argument into xmm1 and divide xmm0 by xmm1
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    divss xmm0, xmm1\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_to_u64_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 1 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_to_u64 expects 1 argument, got {}",
                apply.args.len()
            )));
        }

        let arg = &apply.args[0];

        // Load f32 argument into xmm0
        self.load_f32_proc_argument_into_register(arg, "xmm0")?;

        // Convert f32 to u64 (this is a simplified conversion)
        self.output.push_str("    cvttss2si rax, xmm0\n");

        // Store result from rax to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    /// U64 arithmetic operations for assignment statements
    pub fn compile_u64_add_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_add expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    add rax, rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_sub_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_sub expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    sub rax, rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_mul_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_mul expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    imul rax, rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_div_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_div expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    xor rdx, rdx\n");
        self.output.push_str("    div rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_u64_mod_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
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

        self.load_proc_argument_into_register(arg1, "rax")?;
        self.load_proc_argument_into_register(arg2, "rbx")?;
        self.output.push_str("    xor rdx, rdx\n");
        self.output.push_str("    div rbx\n");
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rdx\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_add_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_add expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    addss xmm0, xmm1\n");
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_sub_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_sub expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    subss xmm0, xmm1\n");
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_mul_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_mul expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    mulss xmm0, xmm1\n");
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_div_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 2 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_div expects 2 arguments, got {}",
                apply.args.len()
            )));
        }

        let arg1 = &apply.args[0];
        let arg2 = &apply.args[1];

        self.load_f32_proc_argument_into_register(arg1, "xmm0")?;
        self.load_f32_proc_argument_into_register(arg2, "xmm1")?;
        self.output.push_str("    divss xmm0, xmm1\n");
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    pub fn compile_f32_to_u64_assign_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 1 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "f32_to_u64 expects 1 argument, got {}",
                apply.args.len()
            )));
        }

        let arg = &apply.args[0];

        // Load f32 argument into xmm0
        self.load_f32_proc_argument_into_register(arg, "xmm0")?;

        // Convert f32 to u64
        self.output.push_str("    cvttss2si rax, xmm0\n");

        // Store result from rax to the variable's stack location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    /// Helper methods for loading arguments into registers
    pub fn load_argument_into_register(
        &mut self,
        arg: &Term<PhaseParse>,
        register: &str,
    ) -> Result<(), CompileError> {
        match arg {
            Term::Number(num) => {
                let number_value = self.parse_number(num.number.s());
                self.output
                    .push_str(&format!("    mov {register}, {number_value}\n"));
            }
            Term::Variable(var) => {
                let var_name = var.variable.s();
                if let Some(&var_offset) = self.variables.get(var_name) {
                    self.output.push_str(&format!(
                        "    mov {register}, qword ptr [rbp - 8 - {}]\n",
                        var_offset - 8
                    ));
                } else {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "Unknown variable: {var_name}"
                    )));
                }
            }
            Term::Paren(paren) => {
                // Handle parenthesized expressions by delegating to the inner term
                self.load_argument_into_register(&paren.term, register)?;
            }
            _ => {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Unsupported argument type: {arg:?}"
                )));
            }
        }
        Ok(())
    }

    pub fn load_f32_argument_into_register(
        &mut self,
        arg: &Term<PhaseParse>,
        register: &str,
    ) -> Result<(), CompileError> {
        match arg {
            Term::Number(num) => {
                let number_str = num.number.s();
                if let Some(float_value) = number_str.strip_suffix("f32") {
                    // Use direct encoding (this is a simplified approach)
                    self.output.push_str(&format!(
                        "    mov eax, {}\n",
                        Self::float_to_hex(float_value.parse::<f32>().unwrap_or(0.0))
                    ));
                    self.output.push_str(&format!("    movd {register}, eax\n"));
                } else {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "Expected f32 number, got: {number_str}"
                    )));
                }
            }
            Term::Variable(var) => {
                let var_name = var.variable.s();
                if let Some(&var_offset) = self.variables.get(var_name) {
                    self.output.push_str(&format!(
                        "    movss {register}, dword ptr [rbp - 8 - {}]\n",
                        var_offset - 8
                    ));
                } else {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "Unknown variable: {var_name}"
                    )));
                }
            }
            Term::Paren(paren) => {
                // Handle parenthesized expressions by delegating to the inner term
                self.load_f32_argument_into_register(&paren.term, register)?;
            }
            _ => {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Unsupported f32 argument type: {arg:?}"
                )));
            }
        }
        Ok(())
    }

    pub fn load_f32_proc_argument_into_register(
        &mut self,
        arg: &ProcTerm<PhaseParse>,
        register: &str,
    ) -> Result<(), CompileError> {
        match arg {
            ProcTerm::Number(num) => {
                let number_str = num.number.s();
                if let Some(float_value) = number_str.strip_suffix("f32") {
                    // Use direct encoding (this is a simplified approach)
                    self.output.push_str(&format!(
                        "    mov eax, {}\n",
                        Self::float_to_hex(float_value.parse::<f32>().unwrap_or(0.0))
                    ));
                    self.output.push_str(&format!("    movd {register}, eax\n"));
                } else {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "Expected f32 number, got: {number_str}"
                    )));
                }
                Ok(())
            }
            ProcTerm::Variable(var) => {
                let var_name = var.variable.s();
                if let Some(&var_offset) = self.variables.get(var_name) {
                    self.output.push_str(&format!(
                        "    movss {register}, dword ptr [rbp - 8 - {}]\n",
                        var_offset - 8
                    ));
                    Ok(())
                } else {
                    Err(CompileError::UnsupportedConstruct(format!(
                        "Unknown variable: {var_name}"
                    )))
                }
            }
            ProcTerm::Paren(paren) => {
                // Handle parenthesized expressions
                self.load_f32_proc_argument_into_register(&paren.proc_term, register)
            }
            ProcTerm::FieldAccess(field_access) => {
                // Handle field access like points.x for f32 loading
                let object_name = field_access.object.s();
                let field_name = field_access.field.s();

                // Check if this is a Structure of Arrays (SoA) access
                let soa_ptr_var_name = format!("{object_name}_{field_name}_ptr");
                if let Some(&ptr_offset) = self.variables.get(&soa_ptr_var_name) {
                    // This is SoA access - load the field array pointer
                    self.output.push_str(&format!(
                        "    mov rax, qword ptr [rbp - 8 - {}]\n",
                        ptr_offset - 8
                    ));

                    // Handle index if present
                    if let Some(index_term) = &field_access.index {
                        // Get array info for element size calculation
                        if let Some(array_type_name) = self.variable_arrays.get(object_name)
                            && let Some(array_info) = self.arrays.get(array_type_name)
                        {
                            let element_size = crate::arrays::get_element_size(
                                &array_info.field_types,
                                &array_info.field_names,
                                field_name,
                            )?;

                            match &**index_term {
                                ProcTerm::Number(num) => {
                                    let index = crate::arrays::parse_number(num.number.s());
                                    let offset = index.parse::<usize>().unwrap_or(0) * element_size;
                                    if offset > 0 {
                                        self.output.push_str(&format!("    add rax, {offset}\n"));
                                    }
                                }
                                ProcTerm::Variable(var) => {
                                    if let Some(&var_offset) = self.variables.get(var.variable.s())
                                    {
                                        self.output.push_str(&format!(
                                            "    mov rbx, qword ptr [rbp - 8 - {}]\n",
                                            var_offset - 8
                                        ));
                                        self.output
                                            .push_str(&format!("    mov rcx, {element_size}\n"));
                                        self.output.push_str("    imul rbx, rcx\n");
                                        self.output.push_str("    add rax, rbx\n");
                                    }
                                }
                                _ => {}
                            }
                        }
                    }

                    // At this point, rax should contain the address of the f32 value
                    // Load the f32 value from that address into an XMM register
                    self.output.push_str("    movss xmm0, dword ptr [rax]\n");
                    Ok(())
                } else {
                    Err(CompileError::UnsupportedConstruct(format!(
                        "Field access not supported in f32 context: {object_name}.{field_name}"
                    )))
                }
            }
            ProcTerm::MethodChain(method_chain) => {
                // Handle field access like points.x 0 for f32 loading
                let object_name = method_chain.object.s();
                let field_name = method_chain.field.s();

                // Check if this is a Structure of Arrays (SoA) access
                let soa_ptr_var_name = format!("{object_name}_{field_name}_ptr");
                if let Some(&ptr_offset) = self.variables.get(&soa_ptr_var_name) {
                    // This is SoA access - load the field array pointer
                    self.output.push_str(&format!(
                        "    mov rax, qword ptr [rbp - 8 - {}]\n",
                        ptr_offset - 8
                    ));

                    // Handle index if present
                    if let Some(index_term) = &method_chain.index {
                        // Get array info for element size calculation
                        if let Some(array_type_name) = self.variable_arrays.get(object_name)
                            && let Some(array_info) = self.arrays.get(array_type_name)
                        {
                            let element_size = crate::arrays::get_element_size(
                                &array_info.field_types,
                                &array_info.field_names,
                                field_name,
                            )?;

                            match &**index_term {
                                ProcTerm::Number(num) => {
                                    let index = crate::arrays::parse_number(num.number.s());
                                    let offset = index.parse::<usize>().unwrap_or(0) * element_size;
                                    if offset > 0 {
                                        self.output.push_str(&format!("    add rax, {offset}\n"));
                                    }
                                }
                                ProcTerm::Variable(var) => {
                                    if let Some(&var_offset) = self.variables.get(var.variable.s())
                                    {
                                        self.output.push_str(&format!(
                                            "    mov rbx, qword ptr [rbp - 8 - {}]\n",
                                            var_offset - 8
                                        ));
                                        self.output
                                            .push_str(&format!("    mov rcx, {element_size}\n"));
                                        self.output.push_str("    imul rbx, rcx\n");
                                        self.output.push_str("    add rax, rbx\n");
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

                    // Load f32 value from SoA array into XMM register
                    self.output
                        .push_str(&format!("    movss {register}, dword ptr [rax]\n"));

                    Ok(())
                } else if let Some(var_offset) = self.variables.get(object_name) {
                    // Fall back to struct-based access for non-SoA variables
                    self.output.push_str(&format!(
                        "    mov rax, qword ptr [rbp - 8 - {}]\n",
                        var_offset - 8
                    ));

                    // Calculate field offset
                    let field_offset = match field_name {
                        "x" => 0,
                        "y" => 4,
                        "z" => 8,
                        _ => 0,
                    };

                    // Load f32 value directly into XMM register
                    if field_offset == 0 {
                        self.output
                            .push_str(&format!("    movss {register}, dword ptr [rax]\n"));
                    } else {
                        self.output.push_str(&format!(
                            "    movss {register}, dword ptr [rax + {field_offset}]\n"
                        ));
                    }

                    Ok(())
                } else {
                    Err(CompileError::UnsupportedConstruct(format!(
                        "Unknown variable: {object_name}"
                    )))
                }
            }
            ProcTerm::Dereference(dereference) => {
                // Handle dereference operation: expr.*
                // First, get the address from the base term (should be a reference)
                // This should compile to something that puts the address in a register
                self.compile_proc_term(&dereference.term)?;

                // At this point, rax should contain the address of the f32 value
                // Load the f32 value from that address into the XMM register
                self.output
                    .push_str(&format!("    movss {register}, dword ptr [rax]\n"));

                Ok(())
            }
            _ => Err(CompileError::UnsupportedConstruct(format!(
                "Unsupported f32 argument type in ProcTerm: {arg:?}"
            ))),
        }
    }

    /// Utility methods
    pub fn float_to_hex(f: f32) -> String {
        format!("0x{:08x}", f.to_bits())
    }

    /// Convert u64 to f32
    pub fn compile_u64_to_f32_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 1 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "u64_to_f32 expects 1 argument, got {}",
                apply.args.len()
            )));
        }

        let arg = &apply.args[0];

        // Load u64 argument into rax
        self.load_proc_argument_into_register(arg, "rax")?;

        // Convert u64 to f32
        self.output.push_str("    cvtsi2ss xmm0, rax\n");

        // Store result from xmm0 to the variable's stack location
        self.output.push_str(&format!(
            "    movss dword ptr [rbp - 8 - {}], xmm0\n",
            offset - 8
        ));

        Ok(())
    }

    /// Create u64 value from literal
    pub fn compile_u64_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 1 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "__u64 expects 1 argument, got {}",
                apply.args.len()
            )));
        }

        let arg = &apply.args[0];

        // Load argument value
        self.load_proc_argument_into_register(arg, "rax")?;

        // Store to variable location
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], rax\n",
            offset - 8
        ));

        Ok(())
    }

    /// Create f32 value from literal
    pub fn compile_f32_let_proc(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
        offset: i32,
    ) -> Result<(), CompileError> {
        if apply.args.len() != 1 {
            return Err(CompileError::UnsupportedConstruct(format!(
                "__f32 expects 1 argument, got {}",
                apply.args.len()
            )));
        }

        let arg = &apply.args[0];

        // Load f32 argument
        match arg {
            ProcTerm::Number(num) => {
                let number_str = num.number.s();
                let float_value = number_str.parse::<f32>().unwrap_or(0.0);
                self.output.push_str(&format!(
                    "    mov eax, {}\n",
                    Self::float_to_hex(float_value)
                ));
                self.output.push_str(&format!(
                    "    mov dword ptr [rbp - 8 - {}], eax\n",
                    offset - 8
                ));
            }
            _ => {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "__f32 expects numeric literal, got: {arg:?}"
                )));
            }
        }

        Ok(())
    }

    /// GPU thread ID builtins - these just return 0 in CPU mode
    pub fn compile_ctaid_x_let_proc(&mut self, offset: i32) -> Result<(), CompileError> {
        // In CPU mode, return 0
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], 0\n",
            offset - 8
        ));
        Ok(())
    }

    pub fn compile_ntid_x_let_proc(&mut self, offset: i32) -> Result<(), CompileError> {
        // In CPU mode, return 1 (single thread)
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], 1\n",
            offset - 8
        ));
        Ok(())
    }

    pub fn compile_tid_x_let_proc(&mut self, offset: i32) -> Result<(), CompileError> {
        // In CPU mode, return 0
        self.output.push_str(&format!(
            "    mov qword ptr [rbp - 8 - {}], 0\n",
            offset - 8
        ));
        Ok(())
    }
}
