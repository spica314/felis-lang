use crate::identifier::compose_param_symbol;
use crate::param_usage::collect_param_field_usage;
use neco_felis_syn::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct ParamLoad {
    key: String,
    param_symbol: String,
}

#[derive(Debug, Clone)]
pub enum PtxCompileError {
    UnsupportedConstruct(String),
}

impl std::fmt::Display for PtxCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PtxCompileError::UnsupportedConstruct(msg) => {
                write!(f, "PTX unsupported construct: {msg}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use neco_felis_syn::{File, FileIdGenerator, Item, ItemProc, ItemUseBuiltin, Parse, Token};
    use std::collections::HashMap;
    use std::path::PathBuf;

    fn load_ptx_fixture() -> (
        HashMap<String, String>,
        HashMap<String, ItemProc<PhaseParse>>,
    ) {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let sample_path = manifest_dir.join("../../testcases/felis/single/ptx_proc_call.fe");
        let source =
            std::fs::read_to_string(sample_path).expect("read sample felis file for PTX test");

        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let tokens = Token::lex(&source, file_id);
        let mut idx = 0;
        let file = File::parse(&tokens, &mut idx)
            .expect("parse file")
            .expect("ast from file");
        assert_eq!(idx, tokens.len(), "parser should consume all tokens");

        let mut builtins = HashMap::new();
        let mut procs = HashMap::new();

        for item in file.items() {
            match item {
                Item::UseBuiltin(ItemUseBuiltin {
                    builtin_name, name, ..
                }) => {
                    builtins.insert(name.s().to_string(), builtin_name.s().to_string());
                }
                Item::Proc(proc_item) if proc_item.ptx_modifier.is_some() => {
                    procs.insert(proc_item.name.s().to_string(), (**proc_item).clone());
                }
                _ => {}
            }
        }

        (builtins, procs)
    }

    #[test]
    fn compile_ptx_proc_generates_expected_param_loads() {
        let (builtins, procs) = load_ptx_fixture();
        let mut compiler = PtxCompiler::new();
        let env = PtxCompileEnv::new(&builtins, &procs);

        let proc_f = procs.get("f").expect("fixture should provide #ptx proc f");

        compiler
            .compile_ptx_proc(proc_f, &env)
            .expect("compile PTX proc f");

        let output = &compiler.ptx_output;

        assert!(
            output.contains(".visible .entry f("),
            "PTX output should include entry declaration: {output}"
        );
        assert!(
            output.contains(".param .u64 ps_r"),
            "PTX output should load ps.r parameter fields: {output}"
        );
        assert!(
            output.contains(".param .u64 ps_g"),
            "PTX output should load ps.g parameter fields: {output}"
        );
        assert!(
            output.contains(".param .u64 ps_b"),
            "PTX output should load ps.b parameter fields: {output}"
        );
        assert!(
            output.contains("st.global.u64"),
            "PTX output should perform global stores: {output}"
        );
        assert!(
            compiler.ptx_functions.contains(&"f".to_string()),
            "compiler should record generated function name"
        );
    }

    #[test]
    fn compile_ptx_proc_inlines_ptx_callees() {
        let (builtins, procs) = load_ptx_fixture();
        let mut compiler = PtxCompiler::new();
        let env = PtxCompileEnv::new(&builtins, &procs);

        let proc_f = procs.get("f").expect("fixture should provide #ptx proc f");

        compiler
            .compile_ptx_proc(proc_f, &env)
            .expect("compile PTX proc f");

        let output = &compiler.ptx_output;

        assert!(
            output.contains("mul.f32"),
            "expected inline vec3_mul PTX body to be present: {output}"
        );
        assert!(
            !output.contains("vec3_mul"),
            "inlined PTX should not reference the callee name directly: {output}"
        );
    }
}

impl std::error::Error for PtxCompileError {}

pub struct PtxCompileEnv<'a> {
    pub builtins: &'a HashMap<String, String>,
    pub available_procs: &'a HashMap<String, ItemProc<PhaseParse>>,
}

impl<'a> PtxCompileEnv<'a> {
    pub fn new(
        builtins: &'a HashMap<String, String>,
        available_procs: &'a HashMap<String, ItemProc<PhaseParse>>,
    ) -> Self {
        Self {
            builtins,
            available_procs,
        }
    }
}

#[derive(Debug)]
pub struct PtxCompiler {
    pub ptx_output: String,
    pub ptx_functions: Vec<String>,
    pub ptx_registers: HashMap<String, String>, // Maps variable names to PTX registers
    pub ptx_next_u64_reg: usize,
    pub ptx_next_u32_reg: usize,
    pub ptx_next_f32_reg: usize,
    pub variables: HashMap<String, i32>,
    builtins: HashMap<String, String>,
    ptx_proc_map: HashMap<String, ItemProc<PhaseParse>>, // available #ptx procs for inlining
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

    fn configure_from_env(&mut self, env: &PtxCompileEnv<'_>) {
        self.builtins = env.builtins.clone();
        self.ptx_proc_map = env.available_procs.clone();
    }

    pub fn compile_ptx_proc(
        &mut self,
        proc: &ItemProc<PhaseParse>,
        env: &PtxCompileEnv<'_>,
    ) -> Result<(), PtxCompileError> {
        self.configure_from_env(env);

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

        let param_loads = if param_names.is_empty() {
            Vec::new()
        } else if param_names.len() == 1 {
            let mut loads = Vec::new();
            let field_usage = collect_param_field_usage(&proc.proc_block.statements, &param_names);
            let param = &param_names[0];
            if let Some(fields) = field_usage.get(param) {
                if fields.is_empty() {
                    let symbol = compose_param_symbol(param, None);
                    loads.push(ParamLoad {
                        key: param.clone(),
                        param_symbol: symbol,
                    });
                } else {
                    for field in fields {
                        let symbol = compose_param_symbol(param, Some(field));
                        loads.push(ParamLoad {
                            key: format!("{param}.{field}"),
                            param_symbol: symbol,
                        });
                    }
                }
            } else {
                let symbol = compose_param_symbol(param, None);
                loads.push(ParamLoad {
                    key: param.clone(),
                    param_symbol: symbol,
                });
            }
            loads
        } else {
            return Err(PtxCompileError::UnsupportedConstruct(
                "PTX kernels with multiple parameters not yet supported".to_string(),
            ));
        };

        if !param_loads.is_empty() {
            for (idx, load) in param_loads.iter().enumerate() {
                let suffix = if idx + 1 == param_loads.len() {
                    ""
                } else {
                    ","
                };
                self.ptx_output.push_str(&format!(
                    "    .param .u64 {}{}\n",
                    load.param_symbol, suffix
                ));
            }
        }

        self.ptx_output.push_str(")\n{\n");

        // Generate PTX body
        self.compile_ptx_proc_block(&proc.proc_block, &param_loads)?;

        self.ptx_output.push_str("    ret;\n");
        self.ptx_output.push_str("}\n\n");

        self.variables.clear();
        Ok(())
    }

    /// Extract parameter names from a procedure type signature
    pub(crate) fn extract_proc_parameters(&self, ty: &Term<PhaseParse>) -> Vec<String> {
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

    fn compile_ptx_proc_block(
        &mut self,
        block: &ItemProcBlock<PhaseParse>,
        param_loads: &[ParamLoad],
    ) -> Result<(), PtxCompileError> {
        // Initialize PTX registers for parameters with higher limit
        self.ptx_output.push_str("    .reg .b64 %rd<100>;\n");
        self.ptx_output.push_str("    .reg .b32 %r<100>;\n");
        self.ptx_output.push_str("    .reg .b32 %f<100>;\n");
        self.ptx_output.push('\n');

        // Load parameters only if there are any
        if !param_loads.is_empty() {
            for load in param_loads {
                let reg = self.allocate_ptx_u64_register();
                self.ptx_output.push_str(&format!(
                    "    ld.param.u64 {reg}, [{}];\n",
                    load.param_symbol
                ));
                self.ptx_output
                    .push_str(&format!("    cvta.to.global.u64 {reg}, {reg};\n"));
                self.ptx_registers.insert(load.key.clone(), reg);
            }
            self.ptx_output.push('\n');
        }

        // Compile statements
        self.compile_ptx_statements(&block.statements)?;

        Ok(())
    }

    pub fn compile_ptx_statements(
        &mut self,
        statements: &Statements<PhaseParse>,
    ) -> Result<(), PtxCompileError> {
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
    ) -> Result<(), PtxCompileError> {
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
                let object_name = field_assign.method_chain.object_name();
                let field_name = field_assign.method_chain.field_name();

                // Get index register if there's an index
                let index_reg = if let Some(index) = &field_assign.method_chain.index {
                    self.compile_ptx_proc_term(index)?
                } else {
                    return Err(PtxCompileError::UnsupportedConstruct(
                        "Field assignment without index not supported".to_string(),
                    ));
                };

                let value_reg = self.compile_ptx_proc_term(&field_assign.value)?;

                // Map field names to device pointers
                let key = format!("{object_name}.{field_name}");
                let field_ptr = self.ptx_registers.get(&key).cloned().ok_or_else(|| {
                    PtxCompileError::UnsupportedConstruct(format!(
                        "Unknown PTX field pointer: {key}"
                    ))
                })?;

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
                    return Err(PtxCompileError::UnsupportedConstruct(
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
            _ => Err(PtxCompileError::UnsupportedConstruct(format!(
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
    ) -> Result<String, PtxCompileError> {
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
                    Err(PtxCompileError::UnsupportedConstruct(format!(
                        "Unknown PTX struct field: {key}"
                    )))
                }
            }
            ProcTerm::Number(num) => {
                // Handle integer literals
                let value_str = num.number.s();
                let value: u64 = value_str.parse().map_err(|_| {
                    PtxCompileError::UnsupportedConstruct(format!("Invalid number: {value_str}"))
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
            _ => Err(PtxCompileError::UnsupportedConstruct(format!(
                "PTX proc term not implemented: {proc_term:?}"
            ))),
        }
    }

    // Helper to compile a variable reference
    pub fn compile_ptx_variable(&mut self, var_name: &str) -> Result<String, PtxCompileError> {
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
                        Err(PtxCompileError::UnsupportedConstruct(format!(
                            "Unknown PTX variable: {var_name}"
                        )))
                    }
                }
            }
        } else if let Some(reg) = self.ptx_registers.get(var_name) {
            Ok(reg.clone())
        } else {
            Err(PtxCompileError::UnsupportedConstruct(format!(
                "Undefined PTX variable: {var_name}"
            )))
        }
    }

    pub fn compile_ptx_proc_apply(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
    ) -> Result<String, PtxCompileError> {
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                    Err(PtxCompileError::UnsupportedConstruct(
                                        "f32 requires a number literal".to_string(),
                                    ))
                                }
                            } else {
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
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
                                Err(PtxCompileError::UnsupportedConstruct(
                                    "__f32_div requires two arguments".to_string(),
                                ))
                            }
                        }
                        _ => Err(PtxCompileError::UnsupportedConstruct(format!(
                            "Unknown PTX builtin: {builtin}"
                        ))),
                    }
                } else if self.ptx_proc_map.contains_key(func_name) {
                    Err(PtxCompileError::UnsupportedConstruct(
                        "PTX user proc call must be in a let-binding to inline".to_string(),
                    ))
                } else {
                    Err(PtxCompileError::UnsupportedConstruct(format!(
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
    ) -> Result<(), PtxCompileError> {
        let callee = self
            .ptx_proc_map
            .get(func_name)
            .ok_or_else(|| {
                PtxCompileError::UnsupportedConstruct(format!("Unknown PTX callee: {func_name}"))
            })?
            .clone();

        // Extract parameter names
        let param_names = self.extract_proc_parameters(&callee.ty);
        if param_names.len() != args.len() {
            return Err(PtxCompileError::UnsupportedConstruct(format!(
                "PTX inline call arity mismatch: expected {}, got {}",
                param_names.len(),
                args.len()
            )));
        }

        // Prepare a sub-compiler for inlining
        let mut sub = PtxCompiler::new();
        let env = PtxCompileEnv::new(&self.builtins, &self.ptx_proc_map);
        sub.configure_from_env(&env);
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
                    let var_name = v.variable.s();
                    let prefix = format!("{var_name}.");
                    let mut inserted = false;
                    for (key, reg) in self.ptx_registers.iter() {
                        if let Some(field_name) = key.strip_prefix(&prefix) {
                            if field_name.is_empty() || field_name.contains('.') {
                                continue;
                            }
                            sub.ptx_registers
                                .insert(format!("{param_name}.{field_name}"), reg.clone());
                            inserted = true;
                        }
                    }
                    if !inserted && let Some(reg) = self.ptx_registers.get(var_name) {
                        sub.ptx_registers
                            .insert(param_name.to_string(), reg.clone());
                        inserted = true;
                    }
                    if !inserted {
                        return Err(PtxCompileError::UnsupportedConstruct(
                            "PTX inline supports only struct args or struct vars".to_string(),
                        ));
                    }
                }
                _ => {
                    return Err(PtxCompileError::UnsupportedConstruct(
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
            return Err(PtxCompileError::UnsupportedConstruct(
                "PTX inline callee did not produce a return value".to_string(),
            ));
        }
        Ok(())
    }
}
