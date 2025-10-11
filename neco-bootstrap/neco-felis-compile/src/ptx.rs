use crate::error::CompileError;
use neco_felis_syn::*;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
struct ParamLoad {
    key: String,
    param_symbol: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn sanitize_ptx_identifier_rewrites_invalid_sequences() {
        assert_eq!(sanitize_ptx_identifier("thread.id"), "thread_id");
        assert_eq!(sanitize_ptx_identifier("1alpha"), "_1alpha");
        assert_eq!(sanitize_ptx_identifier("__ok__"), "__ok__");
        assert_eq!(sanitize_ptx_identifier(""), "_");
        assert_eq!(
            sanitize_ptx_identifier("pixel data/with-hyphen"),
            "pixel_data_with_hyphen"
        );
    }

    #[test]
    fn compose_param_symbol_sanitizes_components() {
        assert_eq!(
            compose_param_symbol("pixel data", Some("first-field!")),
            "pixel_data_first_field_"
        );
        assert_eq!(compose_param_symbol("pixel data", None), "pixel_data");
    }

    #[test]
    fn collect_param_field_usage_extracts_unique_fields() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let source = std::fs::read_to_string("../../testcases/felis/single/ptx_proc_call.fe")
            .expect("read sample felis file");
        let tokens = Token::lex(&source, file_id);

        let mut idx = 0;
        let file = File::parse(&tokens, &mut idx)
            .expect("parse file")
            .expect("ast from file");
        assert_eq!(idx, tokens.len(), "parser should consume all tokens");

        let ptx_proc = file
            .items()
            .iter()
            .find_map(|item| match item {
                Item::Proc(proc_item)
                    if proc_item.ptx_modifier.is_some() && proc_item.name.s() == "f" =>
                {
                    Some(proc_item)
                }
                _ => None,
            })
            .expect("locate #ptx proc f");

        let compiler = PtxCompiler::new();
        let params = compiler.extract_proc_parameters(&ptx_proc.ty);
        assert_eq!(params, vec!["ps".to_string()]);

        let usage = collect_param_field_usage(&ptx_proc.proc_block.statements, &params);
        assert_eq!(
            usage.len(),
            1,
            "only ps parameter should have tracked fields"
        );

        let fields = usage.get("ps").expect("fields for ps");
        assert_eq!(fields.len(), 3, "expected unique field entries for ps");

        let mut sorted_fields = fields.clone();
        sorted_fields.sort();
        assert_eq!(
            sorted_fields,
            vec!["b".to_string(), "g".to_string(), "r".to_string()]
        );

        let unique: HashSet<_> = fields.iter().collect();
        assert_eq!(
            unique.len(),
            fields.len(),
            "duplicate field names should be eliminated"
        );
    }
}

fn sanitize_ptx_identifier(raw: &str) -> String {
    let mut s = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            s.push(ch);
        } else {
            s.push('_');
        }
    }
    if s.is_empty()
        || !(s
            .chars()
            .next()
            .map(|c| c.is_ascii_alphabetic() || c == '_')
            .unwrap_or(false))
    {
        s.insert(0, '_');
    }
    s
}

fn compose_param_symbol(object: &str, field: Option<&str>) -> String {
    match field {
        Some(field_name) => {
            format!(
                "{}_{}",
                sanitize_ptx_identifier(object),
                sanitize_ptx_identifier(field_name)
            )
        }
        None => sanitize_ptx_identifier(object),
    }
}

fn add_param_field_usage(usage: &mut HashMap<String, Vec<String>>, object: &str, field: &str) {
    let entry = usage.entry(object.to_string()).or_default();
    if !entry.iter().any(|existing| existing == field) {
        entry.push(field.to_string());
    }
}

fn collect_param_fields_from_proc_term(
    proc_term: &ProcTerm<PhaseParse>,
    param_set: &HashSet<&str>,
    usage: &mut HashMap<String, Vec<String>>,
) {
    match proc_term {
        ProcTerm::FieldAccess(field_access) => {
            let object = field_access.object_name();
            if param_set.contains(object) {
                add_param_field_usage(usage, object, field_access.field_name());
            }
        }
        ProcTerm::MethodChain(method_chain) => {
            let object = method_chain.object_name();
            if param_set.contains(object) {
                add_param_field_usage(usage, object, method_chain.field_name());
            }
            if let Some(index) = &method_chain.index {
                collect_param_fields_from_proc_term(index, param_set, usage);
            }
        }
        ProcTerm::Apply(apply) => {
            collect_param_fields_from_proc_term(&apply.f, param_set, usage);
            for arg in &apply.args {
                collect_param_fields_from_proc_term(arg, param_set, usage);
            }
        }
        ProcTerm::StructValue(struct_value) => {
            for field in &struct_value.fields {
                collect_param_fields_from_proc_term(&field.value, param_set, usage);
            }
        }
        ProcTerm::Paren(paren) => {
            collect_param_fields_from_proc_term(&paren.proc_term, param_set, usage);
        }
        ProcTerm::If(proc_if) => {
            collect_param_fields_from_statements(&proc_if.condition, param_set, usage);
            collect_param_fields_from_statements(&proc_if.then_body, param_set, usage);
            if let Some(else_clause) = &proc_if.else_clause {
                collect_param_fields_from_statements(&else_clause.else_body, param_set, usage);
            }
        }
        ProcTerm::ConstructorCall(constructor_call) => {
            for arg in &constructor_call.args {
                collect_param_fields_from_proc_term(arg, param_set, usage);
            }
        }
        ProcTerm::Dereference(deref) => {
            collect_param_fields_from_proc_term(&deref.term, param_set, usage);
        }
        ProcTerm::Ext(_)
        | ProcTerm::Variable(_)
        | ProcTerm::Unit(_)
        | ProcTerm::Number(_)
        | ProcTerm::Struct(_) => {}
    }
}

fn collect_param_fields_from_statement(
    statement: &Statement<PhaseParse>,
    param_set: &HashSet<&str>,
    usage: &mut HashMap<String, Vec<String>>,
) {
    match statement {
        Statement::FieldAssign(assign) => {
            let object = assign.method_chain.object_name();
            if param_set.contains(object) {
                add_param_field_usage(usage, object, assign.method_chain.field_name());
            }
            if let Some(index) = &assign.method_chain.index {
                collect_param_fields_from_proc_term(index, param_set, usage);
            }
            collect_param_fields_from_proc_term(&assign.value, param_set, usage);
        }
        Statement::Let(let_stmt) => {
            collect_param_fields_from_proc_term(&let_stmt.value, param_set, usage);
        }
        Statement::LetMut(let_mut_stmt) => {
            collect_param_fields_from_proc_term(&let_mut_stmt.value, param_set, usage);
        }
        Statement::Assign(assign_stmt) => {
            collect_param_fields_from_proc_term(&assign_stmt.value, param_set, usage);
        }
        Statement::Loop(loop_stmt) => {
            collect_param_fields_from_statements(&loop_stmt.body, param_set, usage);
        }
        Statement::Return(return_stmt) => {
            collect_param_fields_from_proc_term(&return_stmt.value, param_set, usage);
        }
        Statement::Expr(proc_term) => {
            collect_param_fields_from_proc_term(proc_term, param_set, usage);
        }
        Statement::CallPtx(_) | Statement::Break(_) | Statement::Ext(_) => {}
    }
}

fn collect_param_fields_from_statements(
    statements: &Statements<PhaseParse>,
    param_set: &HashSet<&str>,
    usage: &mut HashMap<String, Vec<String>>,
) {
    match statements {
        Statements::Then(then) => {
            collect_param_fields_from_statement(&then.head, param_set, usage);
            collect_param_fields_from_statements(&then.tail, param_set, usage);
        }
        Statements::Statement(statement) => {
            collect_param_fields_from_statement(statement, param_set, usage);
        }
        Statements::Nil => {}
    }
}

fn collect_param_field_usage(
    statements: &Statements<PhaseParse>,
    param_names: &[String],
) -> HashMap<String, Vec<String>> {
    if param_names.is_empty() {
        return HashMap::new();
    }
    let param_set: HashSet<&str> = param_names.iter().map(|s| s.as_str()).collect();
    let mut usage: HashMap<String, Vec<String>> = HashMap::new();
    collect_param_fields_from_statements(statements, &param_set, &mut usage);
    usage
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
            return Err(CompileError::UnsupportedConstruct(
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

    fn compile_ptx_proc_block(
        &mut self,
        block: &ItemProcBlock<PhaseParse>,
        param_loads: &[ParamLoad],
    ) -> Result<(), CompileError> {
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
                let object_name = field_assign.method_chain.object_name();
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
                let key = format!("{object_name}.{field_name}");
                let field_ptr = self.ptx_registers.get(&key).cloned().ok_or_else(|| {
                    CompileError::UnsupportedConstruct(format!("Unknown PTX field pointer: {key}"))
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
                        return Err(CompileError::UnsupportedConstruct(
                            "PTX inline supports only struct args or struct vars".to_string(),
                        ));
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
