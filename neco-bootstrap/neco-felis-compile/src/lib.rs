use neco_felis_syn::*;

// Module declarations
pub mod arrays;
pub mod compile_options;
pub mod compiler;
pub mod control_flow;
pub mod error;
pub mod ptx;
pub mod statement;
mod symbol_rewriter;
pub mod syscall;

// Re-exports
pub use compiler::{ArrayInfo, AssemblyCompiler};
pub use error::CompileError;

use crate::compile_options::CompileOptions;

/// Main public API function to compile a file to assembly
pub fn compile_to_assembly(
    file: &File<PhaseParse>,
    compile_options: CompileOptions,
) -> Result<String, CompileError> {
    let renamed_file = neco_felis_rename::rename_file(file)
        .map_err(|err| CompileError::NameResolution(err.to_string()))?;

    let mut lowered = file.clone();
    symbol_rewriter::apply_symbol_ids(&mut lowered, &renamed_file)?;

    let mut compiler = AssemblyCompiler::new(compile_options);
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

    let compile_options = CompileOptions { use_ptx: false };
    let assembly = compile_to_assembly(&file, compile_options)?;
    Ok(assembly)
}

pub fn compile_file_to_assembly_with_ptx(
    file_path: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut file_id_generator = FileIdGenerator::new();
    let file_id = file_id_generator.generate_file_id();
    let source = std::fs::read_to_string(file_path)?;
    let tokens = Token::lex(&source, file_id);

    let mut i = 0;
    let file = File::parse(&tokens, &mut i)?.ok_or("Failed to parse file")?;
    if i != tokens.len() {
        return Err(format!("Failed to parse file. token at {} / {}", i, tokens.len()).into());
    }

    let compile_options = CompileOptions { use_ptx: true };
    let assembly = compile_to_assembly(&file, compile_options)?;
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
