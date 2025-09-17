use crate::error::CompileError;
use neco_felis_syn::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct PtxCompiler {
    pub ptx_output: String,
    pub ptx_functions: Vec<String>,
    pub ptx_registers: HashMap<String, String>, // Maps variable names to PTX registers
    pub ptx_next_u64_reg: usize,
    pub ptx_next_u32_reg: usize,
    pub ptx_next_f32_reg: usize,
    pub variables: HashMap<String, i32>,
    pub builtins: HashMap<String, String>,
    pub ptx_proc_map: HashMap<String, ItemProc<PhaseParse>>, // available #ptx procs for inlining
    pub inlining: bool,
    pub inlined_return_fields: HashMap<String, String>, // field -> reg for struct return
    pub inlined_return_reg: Option<String>,             // single-register return for non-struct
}

impl Default for PtxCompiler {
    fn default() -> Self {
        Self {
            ptx_output: String::new(),
            ptx_functions: Vec::new(),
            ptx_registers: HashMap::new(),
            ptx_next_u64_reg: 4, // Start from %rd4 (1-3 are for params)
            ptx_next_u32_reg: 1,
            ptx_next_f32_reg: 1,
            variables: HashMap::new(),
            builtins: HashMap::new(),
            ptx_proc_map: HashMap::new(),
            inlining: false,
            inlined_return_fields: HashMap::new(),
            inlined_return_reg: None,
        }
    }
}

impl PtxCompiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn compile_ptx_proc(&mut self, proc: &ItemProc<PhaseParse>) -> Result<(), CompileError> {
        // Reset PTX state for this function
        self.ptx_registers.clear();
        self.ptx_next_u64_reg = 4; // Start from %rd4 (1-3 are for params)
        self.ptx_next_u32_reg = 1;
        self.ptx_next_f32_reg = 1;

        // Add function name to PTX functions list
        self.ptx_functions.push(proc.name.s().to_string());

        // Start PTX function
        self.ptx_output.push_str("    	// .globl	f\n");
        self.ptx_output
            .push_str(&format!(".visible .entry {}(\n", proc.name.s()));

        // Extract parameter names and types
        let param_names = self.extract_proc_parameters(&proc.ty);

        // Handle parameters
        if param_names.is_empty() {
            // No parameters
        } else if param_names.len() == 1 {
            // For now, assume single array parameter with struct elements
            // Generate parameter declarations for SoA fields
            self.ptx_output.push_str("    .param .u64 ps_r,\n");
            self.ptx_output.push_str("    .param .u64 ps_g,\n");
            self.ptx_output.push_str("    .param .u64 ps_b\n");
        } else {
            return Err(CompileError::UnsupportedConstruct(
                "PTX kernels with multiple parameters not yet supported".to_string(),
            ));
        }

        self.ptx_output.push_str(")\n{\n");

        // Generate PTX body
        let has_params = !param_names.is_empty();
        self.compile_ptx_proc_block(&proc.proc_block, has_params)?;

        self.ptx_output.push_str("    ret;\n");
        self.ptx_output.push_str("}\n\n");

        self.variables.clear();
        Ok(())
    }

    /// Extract parameter names from a procedure type signature
    fn extract_proc_parameters(&self, ty: &Term<PhaseParse>) -> Vec<String> {
        let mut params = Vec::new();
        Self::extract_params_recursive(ty, &mut params);
        params
    }

    /// Recursively extract parameters from dependent arrow types
    fn extract_params_recursive(term: &Term<PhaseParse>, params: &mut Vec<String>) {
        match term {
            Term::ArrowDep(arrow_dep) => {
                // Extract parameter name from dependent arrow (x : A) -> B
                params.push(arrow_dep.from.variable.s().to_string());
                // Continue with the return type to find more parameters
                Self::extract_params_recursive(&arrow_dep.to, params);
            }
            Term::ArrowNodep(arrow_nodep) => {
                // Check if this is a unit type () -> something
                if let Term::Unit(_) = &*arrow_nodep.from {
                    // This is () -> B, so no parameters from this arrow
                    Self::extract_params_recursive(&arrow_nodep.to, params);
                } else {
                    // For non-dependent arrows A -> B, we don't have parameter names
                    // This shouldn't happen in well-formed procedure signatures, but handle it gracefully
                    params.push(format!("param_{}", params.len()));
                    // Continue with the return type
                    Self::extract_params_recursive(&arrow_nodep.to, params);
                }
            }
            // Base case: we've reached the return type (including unit type)
            _ => {}
        }
    }

    pub fn compile_ptx_proc_block(
        &mut self,
        block: &ItemProcBlock<PhaseParse>,
        has_params: bool,
    ) -> Result<(), CompileError> {
        // Initialize PTX registers for parameters with higher limit
        self.ptx_output.push_str("    .reg .b64 %rd<100>;\n");
        self.ptx_output.push_str("    .reg .b32 %r<100>;\n");
        self.ptx_output.push_str("    .reg .b32 %f<100>;\n");
        self.ptx_output.push('\n');

        // Load parameters only if there are any
        if has_params {
            self.ptx_output.push_str("    ld.param.u64 %rd1, [ps_r];\n");
            self.ptx_output.push_str("    ld.param.u64 %rd2, [ps_g];\n");
            self.ptx_output.push_str("    ld.param.u64 %rd3, [ps_b];\n");
            self.ptx_output
                .push_str("    cvta.to.global.u64 %rd1, %rd1;\n");
            self.ptx_output
                .push_str("    cvta.to.global.u64 %rd2, %rd2;\n");
            self.ptx_output
                .push_str("    cvta.to.global.u64 %rd3, %rd3;\n");
            self.ptx_output.push('\n');
        }

        // Compile statements
        self.compile_ptx_statements(&block.statements)?;

        Ok(())
    }

    pub fn compile_ptx_statements(
        &mut self,
        statements: &Statements<PhaseParse>,
    ) -> Result<(), CompileError> {
        match statements {
            Statements::Then(then) => {
                self.compile_ptx_statement(&then.head)?;
                match &*then.tail {
                    Statements::Nil => Ok(()),
                    _ => self.compile_ptx_statements(&then.tail),
                }
            }
            Statements::Statement(statement) => self.compile_ptx_statement(statement),
            Statements::Nil => Ok(()),
        }
    }

    pub fn compile_ptx_statement(
        &mut self,
        statement: &Statement<PhaseParse>,
    ) -> Result<(), CompileError> {
        match statement {
            Statement::Let(let_stmt) => {
                // Compile the let statement for PTX
                let var_name = let_stmt.variable_name().to_string();
                match &*let_stmt.value {
                    ProcTerm::StructValue(struct_value) => {
                        // Bind fields of struct literal to registers under var_name.field
                        for field in &struct_value.fields {
                            let reg = self.compile_ptx_proc_term(&field.value)?;
                            self.ptx_registers
                                .insert(format!("{}.{}", var_name, field.name.s()), reg);
                        }
                        self.variables.insert(var_name, self.variables.len() as i32);
                        Ok(())
                    }
                    ProcTerm::Apply(apply) => {
                        // Inline user PTX procs
                        if let ProcTerm::Variable(fv) = &*apply.f {
                            let fname = fv.variable.s();
                            if !self.builtins.contains_key(fname)
                                && self.ptx_proc_map.contains_key(fname)
                            {
                                self.inline_ptx_proc_call(&var_name, fname, &apply.args)?;
                                return Ok(());
                            }
                        }
                        let result_reg = self.compile_ptx_proc_term(&let_stmt.value)?;
                        self.ptx_registers.insert(var_name.clone(), result_reg);
                        self.variables.insert(var_name, self.variables.len() as i32);
                        Ok(())
                    }
                    _ => {
                        let result_reg = self.compile_ptx_proc_term(&let_stmt.value)?;
                        self.ptx_registers.insert(var_name.clone(), result_reg);
                        self.variables.insert(var_name, self.variables.len() as i32);
                        Ok(())
                    }
                }
            }
            Statement::FieldAssign(field_assign) => {
                // Generate PTX code for field assignment
                // Format: array.field index <- value
                let _array_var = field_assign.method_chain.object_name();
                let field_name = field_assign.method_chain.field_name();

                // Get index register if there's an index
                let index_reg = if let Some(index) = &field_assign.method_chain.index {
                    self.compile_ptx_proc_term(index)?
                } else {
                    return Err(CompileError::UnsupportedConstruct(
                        "Field assignment without index not supported".to_string(),
                    ));
                };

                let value_reg = self.compile_ptx_proc_term(&field_assign.value)?;

                // Map field names to device pointers
                let field_ptr = match field_name {
                    "r" => "%rd1",
                    "g" => "%rd2",
                    "b" => "%rd3",
                    _ => {
                        return Err(CompileError::UnsupportedConstruct(format!(
                            "Unknown field: {field_name}"
                        )));
                    }
                };

                // Calculate address and store
                // mul.lo.u64 %rd_temp, %index_reg, 8;  // index * sizeof(u64)
                // add.u64 %rd_addr, %field_ptr, %rd_temp;
                // st.global.u64 [%rd_addr], %value_reg;

                let temp_reg = self.allocate_ptx_u64_register();
                let addr_reg = self.allocate_ptx_u64_register();

                self.ptx_output
                    .push_str(&format!("    mul.lo.u64 {temp_reg}, {index_reg}, 8;\n"));
                self.ptx_output.push_str(&format!(
                    "    add.u64 {addr_reg}, {field_ptr}, {temp_reg};\n"
                ));
                self.ptx_output
                    .push_str(&format!("    st.global.u64 [{addr_reg}], {value_reg};\n"));

                Ok(())
            }
            Statement::Return(ret) => {
                if !self.inlining {
                    return Err(CompileError::UnsupportedConstruct(
                        "return not supported in PTX kernel".to_string(),
                    ));
                }
                match &*ret.value {
                    ProcTerm::StructValue(struct_value) => {
                        self.inlined_return_fields.clear();
                        self.inlined_return_reg = None;
                        for field in &struct_value.fields {
                            let reg = self.compile_ptx_proc_term(&field.value)?;
                            self.inlined_return_fields
                                .insert(field.name.s().to_string(), reg);
                        }
                        Ok(())
                    }
                    other => {
                        // Allow scalar returns when inlining: capture the register
                        let reg = self.compile_ptx_proc_term(other)?;
                        self.inlined_return_fields.clear();
                        self.inlined_return_reg = Some(reg);
                        Ok(())
                    }
                }
            }
            _ => Err(CompileError::UnsupportedConstruct(format!(
                "PTX statement not implemented: {statement:?}"
            ))),
        }
    }

    // Helper methods for PTX register allocation
    pub fn allocate_ptx_u64_register(&mut self) -> String {
        let reg = format!("%rd{}", self.ptx_next_u64_reg);
        self.ptx_next_u64_reg += 1;
        reg
    }

    pub fn allocate_ptx_u32_register(&mut self) -> String {
        let reg = format!("%r{}", self.ptx_next_u32_reg);
        self.ptx_next_u32_reg += 1;
        reg
    }

    pub fn allocate_ptx_f32_register(&mut self) -> String {
        let reg = format!("%f{}", self.ptx_next_f32_reg);
        self.ptx_next_f32_reg += 1;
        reg
    }

    // Compile a ProcTerm to PTX and return the result register
    pub fn compile_ptx_proc_term(
        &mut self,
        proc_term: &ProcTerm<PhaseParse>,
    ) -> Result<String, CompileError> {
        use neco_felis_syn::ProcTerm;

        match proc_term {
            ProcTerm::Variable(var) => self.compile_ptx_variable(var.variable.s()),
            ProcTerm::FieldAccess(field_access) => {
                let key = format!(
                    "{}.{}",
                    field_access.object_name(),
                    field_access.field_name()
                );
                if let Some(reg) = self.ptx_registers.get(&key) {
                    Ok(reg.clone())
                } else {
                    Err(CompileError::UnsupportedConstruct(format!(
                        "Unknown PTX struct field: {key}"
                    )))
                }
            }
            ProcTerm::Number(num) => {
                // Handle integer literals
                let value_str = num.number.s();
                let value: u64 = value_str.parse().map_err(|_| {
                    CompileError::UnsupportedConstruct(format!("Invalid number: {value_str}"))
                })?;
                let reg = self.allocate_ptx_u64_register();
                self.ptx_output
                    .push_str(&format!("    mov.u64 {reg}, {value};\n"));
                Ok(reg)
            }
            ProcTerm::Apply(apply) => self.compile_ptx_proc_apply(apply),
            ProcTerm::Paren(paren) => {
                // Handle parenthesized expressions
                self.compile_ptx_proc_term(&paren.proc_term)
            }
            _ => Err(CompileError::UnsupportedConstruct(format!(
                "PTX proc term not implemented: {proc_term:?}"
            ))),
        }
    }

    // Helper to compile a variable reference
    pub fn compile_ptx_variable(&mut self, var_name: &str) -> Result<String, CompileError> {
        // Check if this is a PTX builtin function
        if let Some(builtin) = self.builtins.get(var_name) {
            match builtin.as_str() {
                "ctaid_x" => {
                    let reg = self.allocate_ptx_u32_register();
                    self.ptx_output
                        .push_str(&format!("    mov.u32 {reg}, %ctaid.x;\n"));
                    // Convert to u64
                    let u64_reg = self.allocate_ptx_u64_register();
                    self.ptx_output
                        .push_str(&format!("    cvt.u64.u32 {u64_reg}, {reg};\n"));
                    Ok(u64_reg)
                }
                "ntid_x" => {
                    let reg = self.allocate_ptx_u32_register();
                    self.ptx_output
                        .push_str(&format!("    mov.u32 {reg}, %ntid.x;\n"));
                    // Convert to u64
                    let u64_reg = self.allocate_ptx_u64_register();
                    self.ptx_output
                        .push_str(&format!("    cvt.u64.u32 {u64_reg}, {reg};\n"));
                    Ok(u64_reg)
                }
                "tid_x" => {
                    let reg = self.allocate_ptx_u32_register();
                    self.ptx_output
                        .push_str(&format!("    mov.u32 {reg}, %tid.x;\n"));
                    // Convert to u64
                    let u64_reg = self.allocate_ptx_u64_register();
                    self.ptx_output
                        .push_str(&format!("    cvt.u64.u32 {u64_reg}, {reg};\n"));
                    Ok(u64_reg)
                }
                _ => {
                    // Check if it's a variable
                    if let Some(reg) = self.ptx_registers.get(var_name) {
                        Ok(reg.clone())
                    } else {
                        Err(CompileError::UnsupportedConstruct(format!(
                            "Unknown PTX variable: {var_name}"
                        )))
                    }
                }
            }
        } else if let Some(reg) = self.ptx_registers.get(var_name) {
            Ok(reg.clone())
        } else {
            Err(CompileError::UnsupportedConstruct(format!(
                "Undefined PTX variable: {var_name}"
            )))
        }
    }

    // Compile a term to PTX and return the result register
    pub fn compile_ptx_term(&mut self, term: &Term<PhaseParse>) -> Result<String, CompileError> {
        match term {
            Term::Variable(var) => {
                let var_name = var.variable.s();

                // Check if this is a PTX builtin function
                if let Some(builtin) = self.builtins.get(var_name) {
                    match builtin.as_str() {
                        "ctaid_x" => {
                            let reg = self.allocate_ptx_u32_register();
                            self.ptx_output
                                .push_str(&format!("    mov.u32 {reg}, %ctaid.x;\n"));
                            // Convert to u64
                            let u64_reg = self.allocate_ptx_u64_register();
                            self.ptx_output
                                .push_str(&format!("    cvt.u64.u32 {u64_reg}, {reg};\n"));
                            Ok(u64_reg)
                        }
                        "ntid_x" => {
                            let reg = self.allocate_ptx_u32_register();
                            self.ptx_output
                                .push_str(&format!("    mov.u32 {reg}, %ntid.x;\n"));
                            // Convert to u64
                            let u64_reg = self.allocate_ptx_u64_register();
                            self.ptx_output
                                .push_str(&format!("    cvt.u64.u32 {u64_reg}, {reg};\n"));
                            Ok(u64_reg)
                        }
                        "tid_x" => {
                            let reg = self.allocate_ptx_u32_register();
                            self.ptx_output
                                .push_str(&format!("    mov.u32 {reg}, %tid.x;\n"));
                            // Convert to u64
                            let u64_reg = self.allocate_ptx_u64_register();
                            self.ptx_output
                                .push_str(&format!("    cvt.u64.u32 {u64_reg}, {reg};\n"));
                            Ok(u64_reg)
                        }
                        _ => {
                            // Check if it's a variable
                            if let Some(reg) = self.ptx_registers.get(var_name) {
                                Ok(reg.clone())
                            } else {
                                Err(CompileError::UnsupportedConstruct(format!(
                                    "Unknown PTX variable: {var_name}"
                                )))
                            }
                        }
                    }
                } else if let Some(reg) = self.ptx_registers.get(var_name) {
                    Ok(reg.clone())
                } else {
                    Err(CompileError::UnsupportedConstruct(format!(
                        "Undefined variable: {var_name}"
                    )))
                }
            }
            Term::Number(num) => {
                // Handle integer literals
                let value_str = num.number.s();
                let value: u64 = value_str.parse().map_err(|_| {
                    CompileError::UnsupportedConstruct(format!("Invalid number: {value_str}"))
                })?;
                let reg = self.allocate_ptx_u64_register();
                self.ptx_output
                    .push_str(&format!("    mov.u64 {reg}, {value};\n"));
                Ok(reg)
            }
            Term::Apply(apply) => {
                // Handle function application for PTX builtins
                self.compile_ptx_apply(apply)
            }
            Term::Paren(paren) => {
                // Handle parenthesized expressions
                self.compile_ptx_term(&paren.term)
            }
            _ => Err(CompileError::UnsupportedConstruct(format!(
                "PTX term not implemented: {term:?}"
            ))),
        }
    }

    pub fn compile_ptx_apply(
        &mut self,
        apply: &TermApply<PhaseParse>,
    ) -> Result<String, CompileError> {
        // Handle PTX builtin function applications
        match &*apply.f {
            Term::Variable(var) => {
                let func_name = var.variable.s();

                if let Some(builtin) = self.builtins.get(func_name) {
                    match builtin.as_str() {
                        "u64_add" => {
                            // Expect two arguments
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    add.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_add requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_sub" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    sub.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_sub requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_mul" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    mul.lo.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_mul requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_div" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    div.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_div requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_mod" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    rem.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_mod requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_to_f32" => {
                            if apply.args.len() == 1 {
                                let arg_reg = self.compile_ptx_term(&apply.args[0])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    cvt.rn.f32.u64 {result_reg}, {arg_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_to_f32 requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32_to_u64" => {
                            if apply.args.len() == 1 {
                                let arg_reg = self.compile_ptx_term(&apply.args[0])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    cvt.rzi.u64.f32 {result_reg}, {arg_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_to_u64 requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32" => {
                            // Handle float literal
                            if apply.args.len() == 1 {
                                if let Term::Number(num) = &apply.args[0] {
                                    let value_str = num.number.s();
                                    // Try to parse as float directly
                                    let float_val: f32 = value_str.parse().unwrap_or_else(|_| {
                                        // If parsing fails, try as integer
                                        let int_val: u64 = value_str.parse().unwrap_or(0);
                                        int_val as f32
                                    });
                                    let result_reg = self.allocate_ptx_f32_register();

                                    // Convert float to its bit representation
                                    let bits = float_val.to_bits();
                                    self.ptx_output.push_str(&format!(
                                        "    mov.b32 {result_reg}, 0x{bits:08x};\n"
                                    ));
                                    Ok(result_reg)
                                } else {
                                    Err(CompileError::UnsupportedConstruct(
                                        "f32 requires a number literal".to_string(),
                                    ))
                                }
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "f32 requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32_sqrt_approx" => {
                            if apply.args.len() == 1 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    sqrt.approx.f32 {result_reg}, {arg1_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_sqrt_approx requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32_add" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    add.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_add requires two arguments".to_string(),
                                ))
                            }
                        }
                        "f32_sub" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    sub.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_sub requires two arguments".to_string(),
                                ))
                            }
                        }
                        "f32_mul" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    mul.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_mul requires two arguments".to_string(),
                                ))
                            }
                        }
                        "f32_div" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    div.approx.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_div requires two arguments".to_string(),
                                ))
                            }
                        }
                        _ => Err(CompileError::UnsupportedConstruct(format!(
                            "Unknown PTX builtin: {builtin}"
                        ))),
                    }
                } else {
                    // Regular function application
                    let f_reg = self.compile_ptx_term(&apply.f)?;
                    // Compile all arguments
                    for arg in &apply.args {
                        let _arg_reg = self.compile_ptx_term(arg)?;
                    }

                    // For now, just return the function register
                    // This is a placeholder for actual function call handling
                    Ok(f_reg)
                }
            }
            _ => {
                // Try to compile the function term
                let f_reg = self.compile_ptx_term(&apply.f)?;
                // Compile all arguments
                for arg in &apply.args {
                    let _arg_reg = self.compile_ptx_term(arg)?;
                }
                Ok(f_reg)
            }
        }
    }

    pub fn compile_ptx_proc_apply(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
    ) -> Result<String, CompileError> {
        use neco_felis_syn::ProcTerm;

        // Handle PTX builtin function applications
        match &*apply.f {
            ProcTerm::Variable(var) => {
                let func_name = var.variable.s();

                if let Some(builtin) = self.builtins.get(func_name) {
                    match builtin.as_str() {
                        "u64_add" => {
                            // Expect two arguments
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    add.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_add requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_sub" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    sub.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_sub requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_mul" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    mul.lo.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_mul requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_div" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    div.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_div requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_mod" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    rem.u64 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_mod requires two arguments".to_string(),
                                ))
                            }
                        }
                        "u64_to_f32" => {
                            if apply.args.len() == 1 {
                                let arg_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    cvt.rn.f32.u64 {result_reg}, {arg_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__u64_to_f32 requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32_to_u64" => {
                            if apply.args.len() == 1 {
                                let arg_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let result_reg = self.allocate_ptx_u64_register();
                                self.ptx_output.push_str(&format!(
                                    "    cvt.rzi.u64.f32 {result_reg}, {arg_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_to_u64 requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32" => {
                            // Handle float literal
                            if apply.args.len() == 1 {
                                if let ProcTerm::Number(num) = &apply.args[0] {
                                    let value_str = num.number.s();
                                    // Try to parse as float directly
                                    let float_val: f32 = value_str.parse().unwrap_or_else(|_| {
                                        // If parsing fails, try as integer
                                        let int_val: u64 = value_str.parse().unwrap_or(0);
                                        int_val as f32
                                    });
                                    let result_reg = self.allocate_ptx_f32_register();

                                    // Convert float to its bit representation
                                    let bits = float_val.to_bits();
                                    self.ptx_output.push_str(&format!(
                                        "    mov.b32 {result_reg}, 0x{bits:08x};\n"
                                    ));
                                    Ok(result_reg)
                                } else {
                                    Err(CompileError::UnsupportedConstruct(
                                        "f32 requires a number literal".to_string(),
                                    ))
                                }
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "f32 requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32_sqrt_approx" => {
                            if apply.args.len() == 1 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    sqrt.approx.f32 {result_reg}, {arg1_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_sqrt_approx requires one argument".to_string(),
                                ))
                            }
                        }
                        "f32_add" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    add.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_add requires two arguments".to_string(),
                                ))
                            }
                        }
                        "f32_sub" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    sub.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_sub requires two arguments".to_string(),
                                ))
                            }
                        }
                        "f32_mul" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    mul.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_mul requires two arguments".to_string(),
                                ))
                            }
                        }
                        "f32_div" => {
                            if apply.args.len() == 2 {
                                let arg1_reg = self.compile_ptx_proc_term(&apply.args[0])?;
                                let arg2_reg = self.compile_ptx_proc_term(&apply.args[1])?;
                                let result_reg = self.allocate_ptx_f32_register();
                                self.ptx_output.push_str(&format!(
                                    "    div.approx.f32 {result_reg}, {arg1_reg}, {arg2_reg};\n"
                                ));
                                Ok(result_reg)
                            } else {
                                Err(CompileError::UnsupportedConstruct(
                                    "__f32_div requires two arguments".to_string(),
                                ))
                            }
                        }
                        _ => Err(CompileError::UnsupportedConstruct(format!(
                            "Unknown PTX builtin: {builtin}"
                        ))),
                    }
                } else if self.ptx_proc_map.contains_key(func_name) {
                    Err(CompileError::UnsupportedConstruct(
                        "PTX user proc call must be in a let-binding to inline".to_string(),
                    ))
                } else {
                    Err(CompileError::UnsupportedConstruct(format!(
                        "Unknown PTX function: {func_name}"
                    )))
                }
            }
            _ => {
                // Try to compile the function term
                let f_reg = self.compile_ptx_proc_term(&apply.f)?;
                // Compile all arguments
                for arg in &apply.args {
                    let _arg_reg = self.compile_ptx_proc_term(arg)?;
                }
                Ok(f_reg)
            }
        }
    }

    fn inline_ptx_proc_call(
        &mut self,
        var_name: &str,
        func_name: &str,
        args: &[ProcTerm<PhaseParse>],
    ) -> Result<(), CompileError> {
        let callee = self
            .ptx_proc_map
            .get(func_name)
            .ok_or_else(|| {
                CompileError::UnsupportedConstruct(format!("Unknown PTX callee: {func_name}"))
            })?
            .clone();

        // Extract parameter names
        let param_names = self.extract_proc_parameters(&callee.ty);
        if param_names.len() != args.len() {
            return Err(CompileError::UnsupportedConstruct(format!(
                "PTX inline call arity mismatch: expected {}, got {}",
                param_names.len(),
                args.len()
            )));
        }

        // Prepare a sub-compiler for inlining
        let mut sub = PtxCompiler::new();
        sub.builtins = self.builtins.clone();
        sub.ptx_proc_map = self.ptx_proc_map.clone();
        sub.inlining = true;
        // Share register allocation state to avoid reusing caller's registers
        sub.ptx_next_u64_reg = self.ptx_next_u64_reg;
        sub.ptx_next_u32_reg = self.ptx_next_u32_reg;
        sub.ptx_next_f32_reg = self.ptx_next_f32_reg;

        // Bind param fields: struct literal or variable carrying struct fields
        for (param_name, arg) in param_names.iter().zip(args.iter()) {
            match arg {
                ProcTerm::StructValue(sv) => {
                    for fld in &sv.fields {
                        let reg = self.compile_ptx_proc_term(&fld.value)?;
                        sub.ptx_registers
                            .insert(format!("{}.{}", param_name, fld.name.s()), reg);
                    }
                }
                ProcTerm::Variable(v) => {
                    let mut inserted = false;
                    for f in ["x", "y", "z"] {
                        let src_key = format!("{}.{}", v.variable.s(), f);
                        if let Some(reg) = self.ptx_registers.get(&src_key) {
                            sub.ptx_registers
                                .insert(format!("{param_name}.{f}"), reg.clone());
                            inserted = true;
                        }
                    }
                    if !inserted {
                        let src_key = v.variable.s();
                        if let Some(reg) = self.ptx_registers.get(src_key) {
                            sub.ptx_registers
                                .insert(param_name.to_string(), reg.clone());
                        }
                    }
                }
                _ => {
                    return Err(CompileError::UnsupportedConstruct(
                        "PTX inline supports only struct args or struct vars".to_string(),
                    ));
                }
            }
        }

        // Compile callee body without prologue
        sub.compile_ptx_statements(&callee.proc_block.statements)?;

        // Append generated code
        self.ptx_output.push_str(&sub.ptx_output);
        // Advance caller's register counters to include callee's allocations
        self.ptx_next_u64_reg = sub.ptx_next_u64_reg;
        self.ptx_next_u32_reg = sub.ptx_next_u32_reg;
        self.ptx_next_f32_reg = sub.ptx_next_f32_reg;

        // Expect struct return captured
        if !sub.inlined_return_fields.is_empty() {
            for (field, reg) in sub.inlined_return_fields.iter() {
                self.ptx_registers
                    .insert(format!("{var_name}.{field}"), reg.clone());
            }
        } else if let Some(reg) = sub.inlined_return_reg {
            // Scalar return: bind directly to LHS variable
            self.ptx_registers.insert(var_name.to_string(), reg);
        } else {
            return Err(CompileError::UnsupportedConstruct(
                "PTX inline callee did not produce a return value".to_string(),
            ));
        }
        Ok(())
    }
}
