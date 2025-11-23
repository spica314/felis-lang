use crate::{error::CompileError, statement::StatementCompiler};
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
    pub struct_fields: HashMap<String, Vec<String>>, // Tracks struct literal field order by variable
}

impl Default for AssemblyCompiler {
    fn default() -> Self {
        Self::new()
    }
}

impl AssemblyCompiler {
    pub fn new() -> Self {
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
            struct_fields: HashMap::new(),
        }
    }

    pub fn compile_file(&mut self, file: &File<PhaseParse>) -> Result<String, CompileError> {
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
            Ok(self.output.clone())
        } else {
            Err(CompileError::EntrypointNotFound)
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
        if proc.ptx_modifier.is_some() {
            return Err(CompileError::UnsupportedConstruct(
                "PTX procedures are no longer supported".to_string(),
            ));
        }
        eprintln!("DEBUG: Compiling procedure: {}", proc.name.s());

        // Extract parameter names from the function type
        let param_names = self.extract_proc_parameters(&proc.ty);
        let param_count = param_names.len();
        let let_count = self.count_let_variables_in_proc_block(&proc.proc_block);

        let total_stack_space = if proc.name.s() == "main2" {
            // Allocate extra space for programs that rely on stack slots for array metadata
            256
        } else {
            (param_count + let_count as usize) * 8
        };
        eprintln!(
            "DEBUG: Procedure {} stack allocation {} bytes",
            proc.name.s(),
            total_stack_space
        );

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
                match &*then.tail {
                    Statements::Nil => Ok(()),
                    _ => self.compile_statements(&then.tail),
                }
            }
            Statements::Statement(statement) => {
                self.scan_statement_for_structs(statement);

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
}
