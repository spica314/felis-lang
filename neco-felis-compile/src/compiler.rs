use crate::{
    compile_options::CompileOptions, error::CompileError, ptx::PtxCompiler,
    statement::StatementCompiler,
};
use neco_felis_syn::*;
use std::collections::HashMap;

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
    # cuInit failed - print error and exit
    mov     rax, 1
    mov     rdi, 2
    mov     rsi, offset cuda_init_error
    mov     rdx, 17
    syscall
    mov     rax, 60
    mov     rdi, 1
    syscall
cuda_init_ok:
    # cuDeviceGet(&cu_device, 0)
    mov	    esi, 0
    lea	    rdi, __cu_device[rip]
    call    cuDeviceGet@PLT
    test    eax, eax
    jz      cuda_device_ok
    # cuDeviceGet failed - print error and exit
    mov     rax, 1
    mov     rdi, 2
    mov     rsi, offset cuda_device_error
    mov     rdx, 20
    syscall
    mov     rax, 60
    mov     rdi, 2
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
    # cuCtxCreate_v2 failed - print error and exit
    mov     rax, 1
    mov     rdi, 2
    mov     rsi, offset cuda_context_error
    mov     rdx, 21
    syscall
    mov     rax, 60
    mov     rdi, 3
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
                    self.output.push_str("    .asciz \"\\\n");

                    // Escape the PTX code for assembly string literal
                    let ptx_lines: Vec<&str> = self.ptx_output.lines().collect();
                    for line in ptx_lines {
                        self.output.push_str(&format!("{line}\\n\\\n"));
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
            Item::Array(array) => crate::arrays::compile_array(array, &mut self.arrays),
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

    pub fn compile_proc_field_access(
        &mut self,
        field_access: &ProcTermFieldAccess<PhaseParse>,
    ) -> Result<(), CompileError> {
        crate::statement::memory::compile_proc_field_access(
            field_access,
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
        let mut ptx_compiler = PtxCompiler::new();
        ptx_compiler.builtins = self.builtins.clone();

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
        let (has_array, array_var_name, array_info) = if !call_ptx.args.is_empty() {
            // Extract array name from the argument
            let array_var_name = match &call_ptx.args[0] {
                ProcTerm::Variable(var) => var.variable.s(),
                _ => {
                    return Err(CompileError::UnsupportedConstruct(
                        "call_ptx expects array variable as argument".to_string(),
                    ));
                }
            };

            // Get array info
            let array_type_name = self.variable_arrays.get(array_var_name).ok_or_else(|| {
                CompileError::UnsupportedConstruct(format!(
                    "Unknown array variable: {array_var_name}"
                ))
            })?;

            let array_info = self.arrays.get(array_type_name).ok_or_else(|| {
                CompileError::UnsupportedConstruct(format!("Unknown array type: {array_type_name}"))
            })?;

            (true, array_var_name, array_info.clone())
        } else {
            // No arguments - create dummy info
            (
                false,
                "",
                ArrayInfo {
                    element_type: String::new(),
                    field_names: vec![],
                    field_types: vec![],
                    dimension: 1,
                    size: None,
                },
            )
        };

        // Generate CUDA API calls
        self.output.push_str("    # call_ptx implementation\n");

        // Load PTX module if not already loaded
        self.output.push_str("    # Load PTX module\n");
        self.output
            .push_str(&format!("    lea rdi, ptx_code_{function_name}[rip]\n"));
        self.output.push_str("    lea rsi, __cu_module[rip]\n");
        self.output.push_str("    call cuModuleLoadData@PLT\n");
        self.output.push_str("    test eax, eax\n");
        self.output.push_str("    jz module_load_ok\n");
        self.output
            .push_str("    # cuModuleLoadData failed - print error and exit\n");
        self.output.push_str("    mov     rax, 1\n");
        self.output.push_str("    mov     rdi, 2\n");
        self.output
            .push_str("    mov     rsi, offset cuda_module_error\n");
        self.output.push_str("    mov     rdx, 21\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("    mov     rax, 60\n");
        self.output.push_str("    mov     rdi, 4\n");
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
            .push_str("    # cuModuleGetFunction failed - print error and exit\n");
        self.output.push_str("    mov     rax, 1\n");
        self.output.push_str("    mov     rdi, 2\n");
        self.output
            .push_str("    mov     rsi, offset cuda_function_error\n");
        self.output.push_str("    mov     rdx, 22\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("    mov     rax, 60\n");
        self.output.push_str("    mov     rdi, 5\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("function_get_ok:\n");

        if has_array {
            // Allocate device memory for each field
            let field_count = array_info.field_names.len();
            for (i, field_name) in array_info.field_names.iter().enumerate() {
                self.output.push_str(&format!(
                    "    # Allocate device memory for field {field_name}\n"
                ));
                self.output
                    .push_str(&format!("    lea rdi, device_ptr_{}[rip]\n", i + 1));

                // Calculate size based on array size and element type
                // For now, assume 65536 elements and 8 bytes per element
                self.output.push_str("    mov rsi, 524288\n"); // 65536 * 8
                self.output.push_str("    call cuMemAlloc_v2@PLT\n");
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
                }

                self.output.push_str("    mov rdx, 524288\n"); // Size
                self.output.push_str("    call cuMemcpyHtoD_v2@PLT\n");
            }

            // Set up kernel parameters
            self.output.push_str("    # Set up kernel parameters\n");
            for i in 1..=field_count {
                self.output
                    .push_str(&format!("    lea rax, device_ptr_{i}[rip]\n"));
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
        self.output
            .push_str(&format!("    push {}\n", call_ptx.block_dim_z.s()));

        // Extra (reverse stack order)
        self.output.push_str("    push 0\n");

        // Kernel params (reverse stack order)
        if has_array && !array_info.field_names.is_empty() {
            self.output.push_str("    lea rax, [rbp - 8 -  216]\n");
            self.output.push_str("    push rax\n");
        } else {
            self.output.push_str("    push 0\n"); // NULL params
        }

        // Shared memory and stream (reverse stack order)
        self.output.push_str("    push 0\n"); // sharedMemBytes
        self.output.push_str("    push 0\n"); // stream

        self.output.push_str("    call cuLaunchKernel@PLT\n");
        self.output.push_str("    add rsp, 48\n"); // Clean up stack
        self.output.push_str("    test eax, eax\n");
        self.output.push_str("    jz kernel_launch_ok\n");
        self.output
            .push_str("    # cuLaunchKernel failed - print error and exit\n");
        self.output.push_str("    mov     rax, 1\n");
        self.output.push_str("    mov     rdi, 2\n");
        self.output
            .push_str("    mov     rsi, offset cuda_launch_error\n");
        self.output.push_str("    mov     rdx, 22\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("    mov     rax, 60\n");
        self.output.push_str("    mov     rdi, 6\n");
        self.output.push_str("    syscall\n");
        self.output.push_str("kernel_launch_ok:\n");

        // Synchronize
        self.output.push_str("    call cuCtxSynchronize@PLT\n");

        if has_array {
            // Copy results back
            for (i, field_name) in array_info.field_names.iter().enumerate() {
                self.output
                    .push_str(&format!("    # Copy {field_name} data back from device\n"));

                // Get host pointer for this field
                let field_ptr_var = format!("{array_var_name}_{field_name}_ptr");
                if let Some(&offset) = self.variables.get(&field_ptr_var) {
                    self.output.push_str(&format!(
                        "    mov rdi, QWORD PTR [rbp - 8 - {}]\n",
                        offset - 8
                    ));
                }

                self.output.push_str(&format!(
                    "    mov rsi, QWORD PTR device_ptr_{}[rip]\n",
                    i + 1
                ));
                self.output.push_str("    mov rdx, 524288\n"); // Size
                self.output.push_str("    call cuMemcpyDtoH_v2@PLT\n");
            }

            // Free device memory
            let field_count = array_info.field_names.len();
            for i in 1..=field_count {
                self.output
                    .push_str(&format!("    # Free device memory {i}\n"));
                self.output
                    .push_str(&format!("    mov rdi, QWORD PTR device_ptr_{i}[rip]\n"));
                self.output.push_str("    call cuMemFree_v2@PLT\n");
            }
        }

        Ok(())
    }
}
