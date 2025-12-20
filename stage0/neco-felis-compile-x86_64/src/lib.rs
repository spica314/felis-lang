use neco_felis_syn::*;

// Module declarations
pub mod arrays;
pub mod compiler;
pub mod control_flow;
pub mod error;
pub mod statement;
mod symbol_rewriter;
pub mod syscall;
mod typing;

// Re-exports
pub use compiler::{ArrayInfo, AssemblyCompiler};
pub use error::CompileError;

/// Main public API function to compile a file to assembly
pub fn compile_to_assembly(file: &File<PhaseParse>) -> Result<String, CompileError> {
    let resolved_file = neco_felis_resolve::resolve_file(file)
        .map_err(|err| CompileError::NameResolution(err.to_string()))?;

    if let Err(err) = typing::check_types(&resolved_file) {
        panic!("type error: {err}");
    }

    let mut lowered = file.clone();
    symbol_rewriter::apply_symbol_ids(&mut lowered, &resolved_file)?;

    let mut compiler = AssemblyCompiler::new();
    compiler.compile_file(&lowered)
}

pub fn compile_file_to_assembly(file_path: &str) -> Result<String, Box<dyn std::error::Error>> {
    let mut file_id_generator = FileIdGenerator::new();
    let file_id = file_id_generator.generate_file_id();
    let source = std::fs::read_to_string(file_path)?;
    let tokens = Token::lex(&source, file_id);

    let mut i = 0;
    let file = File::parse(&tokens, &mut i)?.ok_or("Failed to parse file")?;
    if i != tokens.len() {
        return Err(format!("Failed to parse file. token at {} / {}", i, tokens.len()).into());
    }

    let assembly = compile_to_assembly(&file)?;
    Ok(assembly)
}

impl AssemblyCompiler {
    /// Compile a return statement
    pub fn compile_return(
        &mut self,
        return_stmt: &StatementReturn<PhaseParse>,
    ) -> Result<(), CompileError> {
        // Compile the return value expression
        self.compile_proc_term(&return_stmt.value)?;

        // For now, return statements don't generate any specific assembly
        // The return value is already in the correct register/stack location
        // from the previous expression compilation
        Ok(())
    }
}

#[cfg(test)]
mod tests;
