use crate::{
    compile_options::CompileOptions, error::CompileError, ptx::PtxCompiler,
    statement::StatementCompiler,
};
use neco_felis_syn::*;
use std::collections::{BTreeSet, HashMap};

#[derive(Debug, Clone)]
pub struct ArrayInfo {
    #[allow(dead_code)]
    pub element_type: String,
    pub field_names: Vec<String>,
    pub field_types: Vec<String>,
    #[allow(dead_code)]
    pub dimension: usize,
    #[allow(dead_code)]
    pub size: Option<usize>,
}

pub struct AssemblyCompiler {
    pub output: String,
    pub entrypoint: Option<String>,
    pub builtins: HashMap<String, String>,
    pub variables: HashMap<String, i32>,
    pub reference_variables: HashMap<String, String>, // Maps reference var -> original var
    pub stack_offset: i32,
    pub arrays: HashMap<String, ArrayInfo>,
    pub variable_arrays: HashMap<String, String>,
    pub loop_stack: Vec<String>,
    pub compile_options: CompileOptions,
    pub ptx_output: String,
    pub ptx_functions: Vec<String>,
    pub ptx_registers: HashMap<String, String>, // Maps variable names to PTX registers
    pub ptx_next_u64_reg: usize,
    pub ptx_next_u32_reg: usize,
    pub ptx_next_f32_reg: usize,
    // PTX helpers for inlining and selective emission
    pub ptx_all_procs: HashMap<String, ItemProc<PhaseParse>>, // all #ptx procs by name
    pub ptx_kernels_to_emit: std::collections::HashSet<String>, // names used by #call_ptx
    pub struct_fields: HashMap<String, Vec<String>>, // Tracks struct literal field order by variable
}

impl AssemblyCompiler {
    pub fn new(compile_options: CompileOptions) -> Self {
        Self {
            output: String::new(),
            entrypoint: None,
            builtins: HashMap::new(),
            variables: HashMap::new(),
            reference_variables: HashMap::new(),
            stack_offset: 0,
            arrays: HashMap::new(),
            variable_arrays: HashMap::new(),
            loop_stack: Vec::new(),
            compile_options,
            ptx_output: String::new(),
            ptx_functions: Vec::new(),
            ptx_registers: HashMap::new(),
            ptx_next_u64_reg: 4, // Start from %rd4 (1-3 are for params)
            ptx_next_u32_reg: 1,
            ptx_next_f32_reg: 1,
            ptx_all_procs: HashMap::new(),
            ptx_kernels_to_emit: std::collections::HashSet::new(),
            struct_fields: HashMap::new(),
        }
    }

    pub fn compile_file(&mut self, file: &File<PhaseParse>) -> Result<String, CompileError> {
        if !self.compile_options.use_ptx {
            self.output.push_str(".intel_syntax noprefix\n");
            self.output.push_str(".section .text\n");
            self.output.push_str(".globl _start\n\n");

            for item in file.items() {
                self.compile_item(item)?;
            }

            if let Some(entrypoint) = &self.entrypoint {
                self.output.push_str("_start:\n");
                self.output.push_str(&format!("    call {entrypoint}\n"));
                self.output.push_str("    mov rax, 60\n");
                self.output.push_str("    mov rdi, 0\n");
                self.output.push_str("    syscall\n");
            } else {
                return Err(CompileError::EntrypointNotFound);
            }

            Ok(self.output.clone())
        } else {
            self.output.push_str(".intel_syntax noprefix\n");

            // ptx template
            let s = r#"
    .text
    .globl	__cu_device
    .bss
    .align 4
    .type	__cu_device, @object
    .size	__cu_device, 4
__cu_device:
    .zero	4
    .globl	__cu_context
    .align 8
    .type	__cu_context, @object
    .size	__cu_context, 8
__cu_context:
    .zero	8
    .globl	__cu_module
    .align 8
    .type	__cu_module, @object
    .size	__cu_module, 8
__cu_module:
    .zero	8
    .globl	__cu_function
    .align 8
    .type	__cu_function, @object
    .size	__cu_function, 8
__cu_function:
    .zero	8
    .globl	__cu_device_ptr
    .align 8
    .type	__cu_device_ptr, @object
    .size	__cu_device_ptr, 8
__cu_device_ptr:
    .zero	8
"#;
            self.output.push_str(s);

            self.output.push_str(".section .text\n");
            self.output.push_str(".globl main\n\n");

            // Pre-scan: collect PTX procs and determine which ones are launched via #call_ptx
            self.collect_ptx_context(file);

            for item in file.items() {
                self.compile_item(item)?;
            }

            if let Some(entrypoint) = &self.entrypoint {
                self.output.push_str("main:\n");

                self.output.push_str("    push rbp\n");
                self.output.push_str("    mov rbp, rsp\n");
                self.output.push_str("    sub rsp, 256\n"); // Space for kernel params and device ptrs

                let s = r#"
    # cuInit(0)
    mov     edi, 0
    call    cuInit@PLT
    test    eax, eax
    jz      cuda_init_ok
    # cuInit failed - exit with CUresult status code
    mov     edi, 5     # Move CUresult to exit code
    mov     rax, 231     # sys_exit_group
    syscall
cuda_init_ok:
    # cuDeviceGet(&cu_device, 0)
    mov	    esi, 0
    lea	    rdi, __cu_device[rip]
    call    cuDeviceGet@PLT
    test    eax, eax
    jz      cuda_device_ok
    # cuDeviceGet failed - exit with CUresult status code
    mov     edi, 6     # Move CUresult to exit code
    mov     rax, 231     # sys_exit_group
    syscall
cuda_device_ok:
    # cuCtxCreate_v2(&cu_context, 0, cu_device)
    mov	    eax, DWORD PTR __cu_device[rip]
    mov	    edx, eax
    mov	    esi, 0
    lea	    rdi, __cu_context[rip]
    call    cuCtxCreate_v2@PLT
    test    eax, eax
    jz      cuda_context_ok
    # cuCtxCreate_v2 failed - exit with CUresult status code
    mov     edi, 7     # Move CUresult to exit code
    mov     rax, 231     # sys_exit_group
    syscall
cuda_context_ok:
"#;
                self.output.push_str(s);

                self.output.push_str(&format!("    call {entrypoint}\n"));
                self.output.push_str("    mov rax, 60\n");
                self.output.push_str("    mov rdi, 0\n");
                self.output.push_str("    syscall\n");
                self.output.push_str("    mov rsp, rbp\n");
                self.output.push_str("    pop rbp\n");
            } else {
                return Err(CompileError::EntrypointNotFound);
            }

            // Add CUDA error messages in rodata section
            self.output.push_str("\n.section .rodata\n");

            // Add CUDA error messages (always needed when use_ptx is true)
            self.output.push_str("cuda_init_error:\n");
            self.output.push_str("    .asciz \"CUDA init failed\\n\"\n");
            self.output.push_str("cuda_device_error:\n");
            self.output
                .push_str("    .asciz \"CUDA device failed\\n\"\n");
            self.output.push_str("cuda_context_error:\n");
            self.output
                .push_str("    .asciz \"CUDA context failed\\n\"\n");
            self.output.push_str("cuda_module_error:\n");
            self.output
                .push_str("    .asciz \"PTX module load failed\\n\"\n");
            self.output.push_str("cuda_function_error:\n");
            self.output
                .push_str("    .asciz \"PTX function get failed\\n\"\n");
            self.output.push_str("cuda_launch_error:\n");
            self.output
                .push_str("    .asciz \"PTX kernel launch failed\\n\"\n");

            // Add PTX code as data
            if !self.ptx_output.is_empty() {
                // Validate PTX code with ptxas before including it
                if let Err(e) = self.validate_ptx_code() {
                    eprintln!("PTX validation failed: {e}");
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "PTX validation failed: {e}"
                    )));
                }
                // Add PTX function names
                for func_name in &self.ptx_functions {
                    self.output
                        .push_str(&format!("ptx_function_name_{func_name}:\n"));
                    self.output
                        .push_str(&format!("    .asciz \"{func_name}\"\n"));
                }

                // Add PTX code
                for func_name in &self.ptx_functions {
                    self.output.push_str(&format!("ptx_code_{func_name}:\n"));
                    self.output.push_str("    .asciz \"");

                    // Escape the PTX code for assembly string literal
                    let ptx_lines: Vec<&str> = self.ptx_output.lines().collect();
                    for line in ptx_lines {
                        self.output.push_str(&format!("{line}\\n"));
                    }

                    self.output.push_str("\"\n");
                }

                // Add device pointer storage in BSS
                self.output.push_str("\n.section .bss\n");
                self.output.push_str(".align 8\n");
                for i in 1..=10 {
                    // Support up to 10 device pointers
                    self.output.push_str(&format!("device_ptr_{i}:\n"));
                    self.output.push_str("    .zero 8\n");
                }
            }

            Ok(self.output.clone())
        }
    }

    pub fn compile_item(&mut self, item: &Item<PhaseParse>) -> Result<(), CompileError> {
        match item {
            Item::Entrypoint(entrypoint) => {
                self.entrypoint = Some(entrypoint.name.s().to_string());
                Ok(())
            }
            Item::UseBuiltin(use_builtin) => {
                self.builtins.insert(
                    use_builtin.name.s().to_string(),
                    use_builtin.builtin_name.s().to_string(),
                );
                Ok(())
            }
            Item::Proc(proc) => self.compile_proc(proc),
            Item::Struct(_struct_def) => {
                // Struct type definitions are not needed for code generation yet
                // Accept and ignore them to allow programs with user-defined structs
                Ok(())
            }
            _ => Err(CompileError::UnsupportedConstruct(format!("{item:?}"))),
        }
    }

    pub fn compile_proc(&mut self, proc: &ItemProc<PhaseParse>) -> Result<(), CompileError> {
        // Check if this is a PTX procedure
        if proc.ptx_modifier.is_some() {
            // eprintln!("DEBUG: Compiling PTX procedure: {}", proc.name.s());
            return self.compile_ptx_proc(proc);
        }
        eprintln!("DEBUG: Compiling regular procedure: {}", proc.name.s());

        // Extract parameter names from the function type
        let param_names = self.extract_proc_parameters(&proc.ty);
        let param_count = param_names.len();
        let let_count = self.count_let_variables_in_proc_block(&proc.proc_block);
        let has_ptx_calls = self.has_ptx_calls_in_proc_block(&proc.proc_block);

        // Temporary fix: main2 always gets 256 bytes for PTX calls
        let total_stack_space = if proc.name.s() == "main2" {
            eprintln!("DEBUG: main2 procedure detected, allocating 256 bytes for PTX");
            256
        } else if has_ptx_calls {
            // PTX calls require 256 bytes for kernel parameters and device pointers
            eprintln!(
                "DEBUG: Procedure {} has PTX calls, allocating 256 bytes",
                proc.name.s()
            );
            std::cmp::max(256, (param_count + let_count as usize) * 8)
        } else {
            eprintln!(
                "DEBUG: Procedure {} has no PTX calls, allocating {} bytes",
                proc.name.s(),
                (param_count + let_count as usize) * 8
            );
            (param_count + let_count as usize) * 8
        };

        self.output.push_str(&format!("{}:\n", proc.name.s()));

        self.output.push_str("    push rbp\n");
        self.output.push_str("    mov rbp, rsp\n");

        if total_stack_space > 0 {
            eprintln!(
                "DEBUG: Actually writing sub rsp, {} for {}",
                total_stack_space,
                proc.name.s()
            );
            self.output
                .push_str(&format!("    sub rsp, {total_stack_space}\n"));
        } else {
            eprintln!(
                "DEBUG: Skipping stack allocation for {} (size={})",
                proc.name.s(),
                total_stack_space
            );
        }

        self.stack_offset = 0;

        // Store parameters from registers to stack and register them as variables
        for (i, param_name) in param_names.iter().enumerate() {
            self.stack_offset += 8;
            let offset = self.stack_offset;
            self.variables.insert(param_name.clone(), offset);

            // Store parameter from register to stack
            match i {
                0 => self.output.push_str(&format!(
                    "    mov qword ptr [rbp - 8 - {}], rdi\n",
                    offset - 8
                )),
                1 => self.output.push_str(&format!(
                    "    mov qword ptr [rbp - 8 - {}], rsi\n",
                    offset - 8
                )),
                // For now, only support up to 2 parameters
                _ => {
                    return Err(CompileError::UnsupportedConstruct(
                        "More than 2 parameters not supported".to_string(),
                    ));
                }
            }
        }

        self.compile_proc_block(&proc.proc_block)?;

        if total_stack_space > 0 {
            self.output
                .push_str(&format!("    add rsp, {total_stack_space}\n"));
        }

        self.output.push_str("    mov rsp, rbp\n");
        self.output.push_str("    pop rbp\n");

        self.output.push_str("    ret\n\n");
        self.variables.clear();
        self.struct_fields.clear();

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

    pub fn compile_proc_block(
        &mut self,
        block: &ItemProcBlock<PhaseParse>,
    ) -> Result<(), CompileError> {
        self.compile_statements(&block.statements)
    }

    pub fn compile_statements(
        &mut self,
        statements: &Statements<PhaseParse>,
    ) -> Result<(), CompileError> {
        match statements {
            Statements::Then(then) => {
                self.scan_statement_for_structs(&then.head);

                // Handle CallPtx statements directly in AssemblyCompiler
                if let Statement::CallPtx(call_ptx) = then.head.as_ref() {
                    self.compile_call_ptx(call_ptx)?;
                } else {
                    StatementCompiler::compile_statement(
                        &then.head,
                        &mut self.variables,
                        &mut self.reference_variables,
                        &self.builtins,
                        &self.arrays,
                        &mut self.variable_arrays,
                        &mut self.stack_offset,
                        &mut self.output,
                    )?;
                }
                match &*then.tail {
                    Statements::Nil => Ok(()),
                    _ => self.compile_statements(&then.tail),
                }
            }
            Statements::Statement(statement) => {
                self.scan_statement_for_structs(statement);

                // Handle CallPtx statements directly in AssemblyCompiler
                if let Statement::CallPtx(call_ptx) = statement.as_ref() {
                    self.compile_call_ptx(call_ptx)
                } else {
                    StatementCompiler::compile_statement(
                        statement,
                        &mut self.variables,
                        &mut self.reference_variables,
                        &self.builtins,
                        &self.arrays,
                        &mut self.variable_arrays,
                        &mut self.stack_offset,
                        &mut self.output,
                    )
                }
            }
            Statements::Nil => Ok(()),
        }
    }

    fn record_struct_literal_fields(
        &mut self,
        var_name: &str,
        struct_value: &ProcTermStructValue<PhaseParse>,
    ) {
        let field_names = struct_value
            .fields
            .iter()
            .map(|field| field.name.s().to_string())
            .collect::<Vec<_>>();
        self.struct_fields.insert(var_name.to_string(), field_names);
    }

    fn scan_statement_for_structs(&mut self, statement: &Statement<PhaseParse>) {
        match statement {
            Statement::Let(let_stmt) => {
                if let ProcTerm::StructValue(struct_value) = &*let_stmt.value {
                    self.record_struct_literal_fields(let_stmt.variable_name(), struct_value);
                }
            }
            Statement::Loop(loop_stmt) => {
                self.scan_statements_for_structs(&loop_stmt.body);
            }
            Statement::Expr(proc_term) => self.scan_proc_term_for_structs(proc_term),
            Statement::Return(return_stmt) => self.scan_proc_term_for_structs(&return_stmt.value),
            Statement::Assign(assign_stmt) => self.scan_proc_term_for_structs(&assign_stmt.value),
            Statement::FieldAssign(field_assign) => {
                self.scan_proc_term_for_structs(&field_assign.value);
                if let Some(index) = &field_assign.method_chain.index {
                    self.scan_proc_term_for_structs(index);
                }
            }
            Statement::CallPtx(_)
            | Statement::LetMut(_)
            | Statement::Break(_)
            | Statement::Ext(_) => {}
        }
    }

    fn scan_statements_for_structs(&mut self, statements: &Statements<PhaseParse>) {
        match statements {
            Statements::Then(then) => {
                self.scan_statement_for_structs(&then.head);
                self.scan_statements_for_structs(&then.tail);
            }
            Statements::Statement(statement) => self.scan_statement_for_structs(statement),
            Statements::Nil => {}
        }
    }

    fn scan_proc_term_for_structs(&mut self, proc_term: &ProcTerm<PhaseParse>) {
        match proc_term {
            ProcTerm::If(if_expr) => {
                self.scan_statements_for_structs(&if_expr.condition);
                self.scan_statements_for_structs(&if_expr.then_body);
                if let Some(else_clause) = &if_expr.else_clause {
                    self.scan_statements_for_structs(&else_clause.else_body);
                }
            }
            ProcTerm::Apply(apply) => {
                self.scan_proc_term_for_structs(&apply.f);
                for arg in &apply.args {
                    self.scan_proc_term_for_structs(arg);
                }
            }
            ProcTerm::ConstructorCall(constructor_call) => {
                for arg in &constructor_call.args {
                    self.scan_proc_term_for_structs(arg);
                }
            }
            ProcTerm::MethodChain(method_chain) => {
                if let Some(index) = &method_chain.index {
                    self.scan_proc_term_for_structs(index);
                }
            }
            ProcTerm::Paren(paren) => self.scan_proc_term_for_structs(&paren.proc_term),
            ProcTerm::Dereference(deref) => self.scan_proc_term_for_structs(&deref.term),
            ProcTerm::StructValue(_)
            | ProcTerm::Struct(_)
            | ProcTerm::Variable(_)
            | ProcTerm::Unit(_)
            | ProcTerm::Number(_)
            | ProcTerm::FieldAccess(_)
            | ProcTerm::Ext(_) => {}
        }
    }

    fn struct_field_has_array_metadata(&self, base: &str, field: &str) -> bool {
        let size_key = format!("{base}_{field}_size");
        let ptr_key = format!("{base}_{field}_ptr");
        self.variables.contains_key(&size_key) || self.variables.contains_key(&ptr_key)
    }

    fn collect_struct_array_fields(&self, base: &str) -> Vec<String> {
        if let Some(ordered) = self.struct_fields.get(base) {
            let filtered = ordered
                .iter()
                .filter(|field| self.struct_field_has_array_metadata(base, field))
                .cloned()
                .collect::<Vec<_>>();
            if !filtered.is_empty() {
                return filtered;
            }
        }

        let prefix = format!("{base}_");
        let mut candidates = BTreeSet::new();
        for key in self.variables.keys() {
            if let Some(rest) = key.strip_prefix(&prefix) {
                if rest.is_empty() {
                    continue;
                }
                if rest.ends_with("_size") || rest.ends_with("_ptr") {
                    continue;
                }
                if self.struct_field_has_array_metadata(base, rest) {
                    candidates.insert(rest.to_string());
                }
            }
        }
        candidates.into_iter().collect()
    }

    pub fn compile_proc_term(
        &mut self,
        proc_term: &ProcTerm<PhaseParse>,
    ) -> Result<(), CompileError> {
        crate::statement::expressions::compile_proc_term(
            proc_term,
            &self.variables,
            &self.reference_variables,
            &self.builtins,
            &self.arrays,
            &mut HashMap::new(), // We don't modify variable_arrays in proc terms
            &mut self.output,
        )
    }

    pub fn compile_proc_variable(
        &mut self,
        var: &ProcTermVariable<PhaseParse>,
    ) -> Result<(), CompileError> {
        crate::statement::expressions::compile_proc_variable(var, &self.variables, &mut self.output)
    }

    pub fn compile_proc_apply(
        &mut self,
        apply: &ProcTermApply<PhaseParse>,
    ) -> Result<(), CompileError> {
        crate::statement::expressions::compile_proc_apply(
            apply,
            &self.variables,
            &self.builtins,
            &self.arrays,
            &self.variable_arrays,
            &mut self.output,
        )
    }

    pub fn compile_proc_method_chain(
        &mut self,
        method_chain: &ProcTermMethodChain<PhaseParse>,
    ) -> Result<(), CompileError> {
        crate::statement::memory::compile_proc_method_chain(
            method_chain,
            &self.variables,
            &self.arrays,
            &self.variable_arrays,
            &mut self.output,
        )
    }

    pub fn compile_proc_dereference(
        &mut self,
        dereference: &ProcTermDereference<PhaseParse>,
    ) -> Result<(), CompileError> {
        crate::statement::memory::compile_proc_dereference(
            dereference,
            &self.variables,
            &self.reference_variables,
            &self.builtins,
            &self.arrays,
            &mut HashMap::new(), // We don't modify variable_arrays in dereferences
            &mut self.output,
        )
    }

    pub fn compile_proc_constructor_call_with_var(
        &mut self,
        constructor_call: &ProcTermConstructorCall<PhaseParse>,
        var_name: &str,
    ) -> Result<(), CompileError> {
        crate::statement::constructors::compile_proc_constructor_call_with_var(
            constructor_call,
            var_name,
            &self.arrays,
            &self.builtins,
            &mut self.output,
            &mut self.stack_offset,
            &mut self.variables,
            &mut self.variable_arrays,
        )
    }

    pub fn compile_proc_constructor_call(
        &mut self,
        constructor_call: &ProcTermConstructorCall<PhaseParse>,
    ) -> Result<(), CompileError> {
        crate::statement::constructors::compile_proc_constructor_call(
            constructor_call,
            &self.arrays,
            &mut self.output,
            &mut self.stack_offset,
            &mut self.variables,
        )
    }

    pub fn compile_proc_syscall(
        &mut self,
        args: &[ProcTerm<PhaseParse>],
    ) -> Result<(), CompileError> {
        crate::syscall::SyscallCompiler::compile_proc_syscall(
            args,
            &self.variables,
            &mut self.output,
        )
    }

    pub fn parse_number(&self, number_str: &str) -> String {
        if number_str.ends_with("u64") {
            number_str.trim_end_matches("u64").to_string()
        } else {
            number_str.to_string()
        }
    }

    pub fn load_proc_argument_into_register(
        &mut self,
        arg: &ProcTerm<PhaseParse>,
        register: &str,
    ) -> Result<(), CompileError> {
        crate::syscall::SyscallCompiler::load_proc_argument_into_register(
            arg,
            register,
            &self.variables,
            &mut self.output,
        )
    }

    pub fn count_let_variables_in_proc_block(&self, block: &ItemProcBlock<PhaseParse>) -> i32 {
        crate::statement::utils::count_let_variables_in_statements(&block.statements)
    }

    pub fn has_ptx_calls_in_proc_block(&self, block: &ItemProcBlock<PhaseParse>) -> bool {
        crate::statement::utils::has_ptx_calls_in_statements(&block.statements)
    }

    pub fn compile_ptx_proc(&mut self, proc: &ItemProc<PhaseParse>) -> Result<(), CompileError> {
        eprintln!(
            "DEBUG: Starting PTX compilation for function: {}",
            proc.name.s()
        );
        // Skip emitting PTX for helper procs not launched via #call_ptx
        if !self.ptx_kernels_to_emit.contains(proc.name.s()) {
            // Still record in the map for inlining use
            self.ptx_all_procs
                .insert(proc.name.s().to_string(), proc.clone());
            eprintln!(
                "DEBUG: Skipping PTX emission for helper proc {} (will inline if called)",
                proc.name.s()
            );
            return Ok(());
        }

        let mut ptx_compiler = PtxCompiler::new();
        ptx_compiler.builtins = self.builtins.clone();
        // Provide all PTX procs for inlining
        ptx_compiler.ptx_proc_map = self.ptx_all_procs.clone();

        ptx_compiler.compile_ptx_proc(proc)?;

        eprintln!(
            "DEBUG: PTX compilation successful. PTX output length: {}",
            ptx_compiler.ptx_output.len()
        );
        eprintln!("DEBUG: PTX functions: {:?}", ptx_compiler.ptx_functions);

        // Add PTX header directives if this is the first PTX function
        if self.ptx_output.is_empty() {
            self.ptx_output.push_str(".version 8.8\n");
            self.ptx_output.push_str(".target sm_52\n");
            self.ptx_output.push_str(".address_size 64\n\n");
            eprintln!("DEBUG: Added PTX header directives");
        }

        // Update our state with PTX compiler results
        self.ptx_output.push_str(&ptx_compiler.ptx_output);
        self.ptx_functions.extend(ptx_compiler.ptx_functions);

        eprintln!("DEBUG: Final PTX output length: {}", self.ptx_output.len());
        eprintln!(
            "DEBUG: PTX output first 100 chars: {:?}",
            &self.ptx_output[..self.ptx_output.len().min(100)]
        );
        std::fs::write("./a.ptx", &self.ptx_output).unwrap();

        Ok(())
    }

    /// Compile a #call_ptx statement
    pub fn compile_call_ptx(
        &mut self,
        call_ptx: &StatementCallPtx<PhaseParse>,
    ) -> Result<(), CompileError> {
        let function_name = call_ptx.function_name.s();

        // Ensure this is a known PTX function
        if !self.ptx_functions.contains(&function_name.to_string()) {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Unknown PTX function: {function_name}"
            )));
        }

        // Handle arguments
        let (has_array, array_var_name, array_info) = {
            // Extract array name from the argument
            let array_var_name = call_ptx.arg.s();

            // Get array info
            if let Some(array_type_name) = self.variable_arrays.get(array_var_name) {
                let array_info = self.arrays.get(array_type_name).ok_or_else(|| {
                    CompileError::UnsupportedConstruct(format!(
                        "Unknown array type: {array_type_name}"
                    ))
                })?;
                (true, array_var_name, array_info.clone())
            } else {
                // Fallback: treat argument as a struct holding builtin Arrays in fields
                let field_names = self.collect_struct_array_fields(array_var_name);
                if field_names.is_empty() {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "Unknown array variable: {array_var_name}"
                    )));
                }
                (
                    true,
                    array_var_name,
                    ArrayInfo {
                        element_type: "u64".to_string(),
                        field_types: field_names.iter().map(|_| "u64".to_string()).collect(),
                        field_names,
                        dimension: 1,
                        size: None,
                    },
                )
            }
        };

        // Generate CUDA API calls
        self.output.push_str("    # call_ptx implementation\n");

        // self.output.push_str("    sub rsp, 8\n");

        // Load PTX module if not already loaded
        self.output.push_str("    # Load PTX module\n");
        // self.output.push_str(&format!("    mov rax, QWORD PTR ptx_code_{function_name}[rip]\n"));
        // self.output.push_str("    mov rsi, rax\n");
        self.output
            .push_str(&format!("    lea rsi, ptx_code_{function_name}[rip]\n"));
        self.output.push_str("    lea rdi, __cu_module[rip]\n");
        self.output.push_str("    call cuModuleLoadData@PLT\n");
        self.output.push_str("    test eax, eax\n");
        self.output.push_str("    jz module_load_ok\n");
        self.output
            .push_str("    # cuModuleLoadData failed - exit with CUresult status code\n");
        self.output
            .push_str("    mov     edi, 8     # Move CUresult to exit code\n");
        self.output
            .push_str("    mov     rax, 231     # sys_exit_group\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("module_load_ok:\n");

        // Get function from module
        self.output.push_str("    # Get function from module\n");
        self.output.push_str("    lea rdi, __cu_function[rip]\n");
        self.output
            .push_str("    mov rsi, QWORD PTR __cu_module[rip]\n");
        self.output.push_str(&format!(
            "    lea rdx, ptx_function_name_{function_name}[rip]\n"
        ));
        self.output.push_str("    call cuModuleGetFunction@PLT\n");
        self.output.push_str("    test eax, eax\n");
        self.output.push_str("    jz function_get_ok\n");
        self.output
            .push_str("    # cuModuleGetFunction failed - exit with CUresult status code\n");
        self.output
            .push_str("    mov     edi, 9     # Move CUresult to exit code\n");
        self.output
            .push_str("    mov     rax, 231     # sys_exit_group\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("function_get_ok:\n");

        let field_count = array_info.field_names.len();

        if has_array {
            if field_count > 10 {
                return Err(CompileError::UnsupportedConstruct(
                    "PTX kernel argument struct exceeds supported field count (10)".to_string(),
                ));
            }

            // Allocate device memory for each field
            for (i, field_name) in array_info.field_names.iter().enumerate() {
                self.output.push_str(&format!(
                    "    # Allocate device memory for field {field_name}\n"
                ));
                self.output
                    .push_str(&format!("    lea rdi, device_ptr_{}[rip]\n", i + 1));

                // Calculate size based on array size (8-byte elements)
                if let Some(&soff) = self
                    .variables
                    .get(&format!("{array_var_name}_{field_name}_size"))
                {
                    self.output.push_str(&format!(
                        "    mov rsi, QWORD PTR [rbp - 8 - {}]\n",
                        soff - 8
                    ));
                    self.output.push_str("    shl rsi, 3\n");
                } else if let Some(&soff) = self.variables.get(&format!("{array_var_name}_size")) {
                    self.output.push_str(&format!(
                        "    mov rsi, QWORD PTR [rbp - 8 - {}]\n",
                        soff - 8
                    ));
                    self.output.push_str("    shl rsi, 3\n");
                } else {
                    self.output.push_str("    mov rsi, 524288\n"); // Fallback
                }
                self.output.push_str("    call cuMemAlloc_v2@PLT\n");
                self.output.push_str("    test eax, eax\n");
                self.output.push_str(&format!("    jz mem_alloc_ok_{i}\n"));
                self.output
                    .push_str("    # cuMemAlloc_v2 failed - exit with CUresult status code\n");
                self.output
                    .push_str("    mov     edi, 10     # Move CUresult to exit code\n");
                self.output
                    .push_str("    mov     rax, 231     # sys_exit_group\n");
                self.output.push_str("    syscall\n");
                self.output.push_str(&format!("mem_alloc_ok_{i}:\n"));
            }

            // Copy data to device
            for (i, field_name) in array_info.field_names.iter().enumerate() {
                self.output
                    .push_str(&format!("    # Copy {field_name} data to device\n"));
                self.output.push_str(&format!(
                    "    mov rdi, QWORD PTR device_ptr_{}[rip]\n",
                    i + 1
                ));

                // Get host pointer for this field
                let field_ptr_var = format!("{array_var_name}_{field_name}_ptr");
                if let Some(&offset) = self.variables.get(&field_ptr_var) {
                    self.output.push_str(&format!(
                        "    mov rsi, QWORD PTR [rbp - 8 - {}]\n",
                        offset - 8
                    ));
                } else if let Some(&offset) = self
                    .variables
                    .get(&format!("{array_var_name}_{field_name}"))
                {
                    self.output.push_str(&format!(
                        "    mov rsi, QWORD PTR [rbp - 8 - {}]\n",
                        offset - 8
                    ));
                }

                // Size in bytes
                if let Some(&soff) = self
                    .variables
                    .get(&format!("{array_var_name}_{field_name}_size"))
                {
                    self.output.push_str(&format!(
                        "    mov rdx, QWORD PTR [rbp - 8 - {}]\n",
                        soff - 8
                    ));
                    self.output.push_str("    shl rdx, 3\n");
                } else if let Some(&soff) = self.variables.get(&format!("{array_var_name}_size")) {
                    self.output.push_str(&format!(
                        "    mov rdx, QWORD PTR [rbp - 8 - {}]\n",
                        soff - 8
                    ));
                    self.output.push_str("    shl rdx, 3\n");
                } else {
                    self.output.push_str("    mov rdx, 524288\n");
                }
                self.output.push_str("    call cuMemcpyHtoD_v2@PLT\n");
                self.output.push_str("    test eax, eax\n");
                self.output
                    .push_str(&format!("    jz mem_copy_htod_ok_{i}\n"));
                self.output
                    .push_str("    # cuMemcpyHtoD_v2 failed - exit with CUresult status code\n");
                self.output
                    .push_str("    mov     edi, 11     # Move CUresult to exit code\n");
                self.output
                    .push_str("    mov     rax, 231     # sys_exit_group\n");
                self.output.push_str("    syscall\n");
                self.output.push_str(&format!("mem_copy_htod_ok_{i}:\n"));
            }

            // Set up kernel parameters
            self.output.push_str("    # Set up kernel parameters\n");
            for i in 1..=field_count {
                self.output.push_str(&format!(
                    "    lea rax, device_ptr_{}[rip]\n",
                    field_count + 1 - i
                ));
                self.output.push_str(&format!(
                    "    mov QWORD PTR [rbp - 8 - {}], rax\n",
                    200 + (i - 1) * 8
                ));
            }
        }

        // Launch kernel
        self.output.push_str("    # Launch kernel\n");

        self.output.push_str("    sub rsp, 8\n");

        self.output
            .push_str("    mov rdi, QWORD PTR __cu_function[rip]\n");

        // Grid dimensions
        self.output
            .push_str(&format!("    mov rsi, {}\n", call_ptx.grid_dim_x.s()));
        self.output
            .push_str(&format!("    mov rdx, {}\n", call_ptx.grid_dim_y.s()));
        self.output
            .push_str(&format!("    mov rcx, {}\n", call_ptx.grid_dim_z.s()));

        // Block dimensions
        self.output
            .push_str(&format!("    mov r8, {}\n", call_ptx.block_dim_x.s()));
        self.output
            .push_str(&format!("    mov r9, {}\n", call_ptx.block_dim_y.s()));

        // Extra (reverse stack order)
        self.output.push_str("    push 0\n");

        // Kernel params (reverse stack order)
        if has_array && field_count > 0 {
            let params_base_offset = 200 + (field_count.saturating_sub(1) * 8);
            self.output
                .push_str(&format!("    lea rax, [rbp - 8 - {params_base_offset}]\n"));
            self.output.push_str("    push rax\n");
        } else {
            self.output.push_str("    push 0\n"); // NULL params
        }

        // Shared memory and stream (reverse stack order)
        self.output.push_str("    push 0\n"); // sharedMemBytes
        self.output.push_str("    push 0\n"); // stream
        self.output
            .push_str(&format!("    push {}\n", call_ptx.block_dim_z.s()));

        self.output.push_str("    call cuLaunchKernel@PLT\n");
        self.output.push_str("    add rsp, 48\n"); // Clean up stack
        self.output.push_str("    test eax, eax\n");
        self.output.push_str("    jz kernel_launch_ok\n");
        self.output
            .push_str("    # cuLaunchKernel failed - exit with CUresult status code\n");
        self.output
            .push_str("    mov     edi, 12     # Move CUresult to exit code\n");
        self.output
            .push_str("    mov     rax, 231     # sys_exit_group\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("kernel_launch_ok:\n");

        // Synchronize
        self.output.push_str("    call cuCtxSynchronize@PLT\n");
        self.output.push_str("    test eax, eax\n");
        self.output.push_str("    jz ctx_sync_ok\n");
        self.output
            .push_str("    # cuCtxSynchronize failed - exit with CUresult status code\n");
        self.output
            .push_str("    mov     edi, 13     # Move CUresult to exit code\n");
        self.output
            .push_str("    mov     rax, 231     # sys_exit_group\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("ctx_sync_ok:\n");

        if has_array {
            // Copy results back
            for (i, field_name) in array_info.field_names.iter().enumerate() {
                self.output
                    .push_str(&format!("    # Copy {field_name} data back from device\n"));

                // Get host pointer for this field: prefer *_ptr; fallback to direct field slot
                let field_ptr_var = format!("{array_var_name}_{field_name}_ptr");
                if let Some(&offset) = self.variables.get(&field_ptr_var) {
                    self.output.push_str(&format!(
                        "    mov rdi, QWORD PTR [rbp - 8 - {}]\n",
                        offset - 8
                    ));
                } else if let Some(&offset) = self
                    .variables
                    .get(&format!("{array_var_name}_{field_name}"))
                {
                    self.output.push_str(&format!(
                        "    mov rdi, QWORD PTR [rbp - 8 - {}]\n",
                        offset - 8
                    ));
                }

                self.output.push_str(&format!(
                    "    mov rsi, QWORD PTR device_ptr_{}[rip]\n",
                    i + 1
                ));
                // Determine copy size: prefer struct field specific size, then global size
                if let Some(&soff) = self
                    .variables
                    .get(&format!("{array_var_name}_{field_name}_size"))
                {
                    self.output.push_str(&format!(
                        "    mov rdx, QWORD PTR [rbp - 8 - {}]\n",
                        soff - 8
                    ));
                    self.output.push_str("    shl rdx, 3\n");
                } else if let Some(&soff) = self.variables.get(&format!("{array_var_name}_size")) {
                    self.output.push_str(&format!(
                        "    mov rdx, QWORD PTR [rbp - 8 - {}]\n",
                        soff - 8
                    ));
                    self.output.push_str("    shl rdx, 3\n");
                } else {
                    self.output.push_str("    mov rdx, 524288\n"); // Fallback
                }
                self.output.push_str("    call cuMemcpyDtoH_v2@PLT\n");
                self.output.push_str("    test eax, eax\n");
                self.output
                    .push_str(&format!("    jz mem_copy_dtoh_ok_{i}\n"));
                self.output
                    .push_str("    # cuMemcpyDtoH_v2 failed - exit with CUresult status code\n");
                self.output
                    .push_str("    mov     edi, 14     # Move CUresult to exit code\n");
                self.output
                    .push_str("    mov     rax, 231     # sys_exit_group\n");
                self.output.push_str("    syscall\n");
                self.output.push_str(&format!("mem_copy_dtoh_ok_{i}:\n"));
            }

            // Free device memory
            let field_count = array_info.field_names.len();
            for i in 1..=field_count {
                self.output
                    .push_str(&format!("    # Free device memory {i}\n"));
                self.output
                    .push_str(&format!("    mov rdi, QWORD PTR device_ptr_{i}[rip]\n"));
                self.output.push_str("    call cuMemFree_v2@PLT\n");
                self.output.push_str("    test eax, eax\n");
                self.output.push_str(&format!("    jz mem_free_ok_{i}\n"));
                self.output
                    .push_str("    # cuMemFree_v2 failed - exit with CUresult status code\n");
                self.output
                    .push_str("    mov     edi, 15     # Move CUresult to exit code\n");
                self.output
                    .push_str("    mov     rax, 231     # sys_exit_group\n");
                self.output.push_str("    syscall\n");
                self.output.push_str(&format!("mem_free_ok_{i}:\n"));
            }
        }

        Ok(())
    }

    fn collect_ptx_context(&mut self, file: &File<PhaseParse>) {
        self.ptx_all_procs.clear();
        self.ptx_kernels_to_emit.clear();
        // Map all PTX procs by name
        for item in file.items() {
            if let Item::Proc(p) = item
                && p.ptx_modifier.is_some()
            {
                self.ptx_all_procs
                    .insert(p.name.s().to_string(), p.as_ref().clone());
            }
        }
        // Collect kernel names used by #call_ptx
        for item in file.items() {
            if let Item::Proc(p) = item {
                self.collect_ptx_calls_in_statements(&p.proc_block.statements);
            }
        }
        eprintln!(
            "DEBUG: PTX context collected. helpers = {:?}, kernels = {:?}",
            self.ptx_all_procs.keys().collect::<Vec<_>>(),
            self.ptx_kernels_to_emit
        );
    }

    fn collect_ptx_calls_in_statements(&mut self, statements: &Statements<PhaseParse>) {
        match statements {
            Statements::Then(then) => {
                self.collect_ptx_calls_in_statement(&then.head);
                self.collect_ptx_calls_in_statements(&then.tail);
            }
            Statements::Statement(stmt) => self.collect_ptx_calls_in_statement(stmt),
            Statements::Nil => {}
        }
    }

    fn collect_ptx_calls_in_statement(&mut self, statement: &Statement<PhaseParse>) {
        if let Statement::CallPtx(call_ptx) = statement {
            self.ptx_kernels_to_emit
                .insert(call_ptx.function_name.s().to_string());
        }
    }

    /// Validate PTX code using ptxas
    fn validate_ptx_code(&self) -> Result<(), String> {
        use std::io::Write;
        use std::process::Command;
        use tempfile::NamedTempFile;

        // Create a temporary file to write the PTX code
        let mut temp_file =
            NamedTempFile::new().map_err(|e| format!("Failed to create temporary file: {e}"))?;

        // Write PTX code to temporary file
        temp_file
            .write_all(self.ptx_output.as_bytes())
            .map_err(|e| format!("Failed to write PTX code to temporary file: {e}"))?;

        // Get the path to the temporary file
        let temp_path = temp_file.path();

        // Run ptxas to validate the PTX code
        let output = Command::new("ptxas")
            .arg("--compile-only")  // Only compile, don't generate output
            .arg("--gpu-name=sm_52")  // Target architecture
            .arg(temp_path)
            .output()
            .map_err(|e| format!("Failed to run ptxas: {e}. Make sure CUDA toolkit is installed and ptxas is in PATH."))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            return Err(format!(
                "ptxas validation failed:\nSTDOUT: {stdout}\nSTDERR: {stderr}"
            ));
        }

        eprintln!("PTX validation successful");
        Ok(())
    }
}
