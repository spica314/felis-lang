pub mod arithmetic;
pub mod constructors;
pub mod control_flow;
pub mod expressions;
pub mod memory;
pub mod utils;
pub mod variables;

use crate::{ArrayInfo, error::CompileError};
use neco_felis_syn::*;
use std::collections::HashMap;

pub struct StatementCompiler;

impl StatementCompiler {
    #[allow(clippy::too_many_arguments)]
    pub fn compile_statement(
        statement: &Statement<PhaseParse>,
        variables: &mut HashMap<String, i32>,
        reference_variables: &mut HashMap<String, String>,
        builtins: &HashMap<String, String>,
        arrays: &HashMap<String, ArrayInfo>,
        variable_arrays: &mut HashMap<String, String>,
        stack_offset: &mut i32,
        output: &mut String,
    ) -> Result<(), CompileError> {
        match statement {
            Statement::Let(let_stmt) => variables::compile_let_statement(
                let_stmt,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statement::LetMut(let_mut_stmt) => variables::compile_let_mut_statement(
                let_mut_stmt,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statement::Assign(assign_stmt) => variables::compile_assign_statement(
                assign_stmt,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statement::FieldAssign(field_assign_stmt) => variables::compile_field_assign_statement(
                field_assign_stmt,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statement::Expr(proc_term) => expressions::compile_proc_term(
                proc_term,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statement::Return(return_stmt) => variables::compile_return_statement(
                return_stmt,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statement::Loop(loop_stmt) => control_flow::compile_loop_statement(
                loop_stmt,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statement::Break(_) => Err(CompileError::UnsupportedConstruct(
                "Break statement outside of loop context".to_string(),
            )),
            _ => Err(CompileError::UnsupportedConstruct(format!("{statement:?}"))),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn compile_statements(
        statements: &Statements<PhaseParse>,
        variables: &mut HashMap<String, i32>,
        reference_variables: &mut HashMap<String, String>,
        builtins: &HashMap<String, String>,
        arrays: &HashMap<String, ArrayInfo>,
        variable_arrays: &mut HashMap<String, String>,
        stack_offset: &mut i32,
        output: &mut String,
    ) -> Result<(), CompileError> {
        match statements {
            Statements::Then(then) => {
                // Compile the head statement
                Self::compile_statement(
                    &then.head,
                    variables,
                    reference_variables,
                    builtins,
                    arrays,
                    variable_arrays,
                    stack_offset,
                    output,
                )?;

                // Compile the tail statements
                Self::compile_statements(
                    &then.tail,
                    variables,
                    reference_variables,
                    builtins,
                    arrays,
                    variable_arrays,
                    stack_offset,
                    output,
                )
            }
            Statements::Statement(statement) => Self::compile_statement(
                statement,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                output,
            ),
            Statements::Nil => Ok(()),
        }
    }
}
