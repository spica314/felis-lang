use crate::{ArrayInfo, error::CompileError};
use neco_felis_syn::*;
use std::collections::HashMap;

use super::StatementCompiler;

#[allow(clippy::too_many_arguments)]
pub fn compile_proc_if(
    if_expr: &ProcTermIf<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    output: &mut String,
) -> Result<(), CompileError> {
    // Generate unique labels for this if statement
    static LABEL_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let label_id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    let else_label = format!("if_else_{label_id}");
    let end_label = format!("if_end_{label_id}");

    // Create mutable copies for if statement compilation
    let mut local_variables = variables.clone();
    let mut local_reference_variables = reference_variables.clone();

    // Compile the condition
    StatementCompiler::compile_statements(
        &if_expr.condition,
        &mut local_variables,
        &mut local_reference_variables,
        builtins,
        arrays,
        variable_arrays,
        &mut 0, // stack_offset - not used in condition compilation
        output,
    )?;

    // Compare the condition result with 0 (false)
    output.push_str("    cmp rax, 0\n");

    if if_expr.else_clause.is_some() {
        // If we have an else clause, jump to else on zero (false condition)
        output.push_str(&format!("    je {else_label}\n"));
    } else {
        // If no else clause, jump to end on zero (false condition)
        output.push_str(&format!("    je {end_label}\n"));
    }

    // Compile the then body
    StatementCompiler::compile_statements(
        &if_expr.then_body,
        &mut local_variables.clone(),
        &mut local_reference_variables.clone(),
        builtins,
        arrays,
        variable_arrays,
        &mut 0, // stack_offset - not used in if body compilation
        output,
    )?;

    // Jump to end after then body (skip else)
    if if_expr.else_clause.is_some() {
        output.push_str(&format!("    jmp {end_label}\n"));

        // Else label
        output.push_str(&format!("{else_label}:\n"));

        // Compile the else body
        if let Some(else_clause) = &if_expr.else_clause {
            StatementCompiler::compile_statements(
                &else_clause.else_body,
                &mut local_variables.clone(),
                &mut local_reference_variables.clone(),
                builtins,
                arrays,
                variable_arrays,
                &mut 0, // stack_offset - not used in if body compilation
                output,
            )?;
        }
    }

    // End label
    output.push_str(&format!("{end_label}:\n"));

    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub fn compile_loop_statement(
    loop_stmt: &StatementLoop<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &mut HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    output: &mut String,
) -> Result<(), CompileError> {
    // Generate unique labels for this loop
    static LOOP_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let loop_id = LOOP_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    let loop_start_label = format!("loop_start_{loop_id}");
    let loop_end_label = format!("loop_end_{loop_id}");

    // Loop start label
    output.push_str(&format!("{loop_start_label}:\n"));

    // Compile the loop body with break label context
    compile_statements_with_break(
        &loop_stmt.body,
        variables,
        reference_variables,
        builtins,
        arrays,
        variable_arrays,
        stack_offset,
        &loop_end_label,
        output,
    )?;

    // Jump back to the start of the loop
    output.push_str(&format!("    jmp {loop_start_label}\n"));

    // Loop end label (for breaks)
    output.push_str(&format!("{loop_end_label}:\n"));

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_proc_if_with_break(
    if_expr: &ProcTermIf<PhaseParse>,
    variables: &HashMap<String, i32>,
    reference_variables: &HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    break_label: &str,
    output: &mut String,
) -> Result<(), CompileError> {
    // Generate unique labels for this if statement
    static LABEL_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let label_id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    let else_label = format!("if_break_else_{label_id}");
    let end_label = format!("if_break_end_{label_id}");

    // Create mutable copies for if statement compilation
    let mut local_variables = variables.clone();
    let mut local_reference_variables = reference_variables.clone();

    // Compile the condition
    compile_statements_with_break(
        &if_expr.condition,
        &mut local_variables,
        &mut local_reference_variables,
        builtins,
        arrays,
        variable_arrays,
        &mut 0, // stack_offset - not used in condition compilation
        break_label,
        output,
    )?;

    // Compare the condition result with 0 (false)
    output.push_str("    cmp rax, 0\n");

    if if_expr.else_clause.is_some() {
        // If we have an else clause, jump to else on zero (false condition)
        output.push_str(&format!("    je {else_label}\n"));
    } else {
        // If no else clause, jump to end on zero (false condition)
        output.push_str(&format!("    je {end_label}\n"));
    }

    // Compile the then body
    compile_statements_with_break(
        &if_expr.then_body,
        &mut local_variables.clone(),
        &mut local_reference_variables.clone(),
        builtins,
        arrays,
        variable_arrays,
        &mut 0, // stack_offset - not used in if body compilation
        break_label,
        output,
    )?;

    // Jump to end after then body (skip else)
    if if_expr.else_clause.is_some() {
        output.push_str(&format!("    jmp {end_label}\n"));

        // Else label
        output.push_str(&format!("{else_label}:\n"));

        // Compile the else body
        if let Some(else_clause) = &if_expr.else_clause {
            compile_statements_with_break(
                &else_clause.else_body,
                &mut local_variables.clone(),
                &mut local_reference_variables.clone(),
                builtins,
                arrays,
                variable_arrays,
                &mut 0, // stack_offset - not used in if body compilation
                break_label,
                output,
            )?;
        }
    }

    // End label
    output.push_str(&format!("{end_label}:\n"));

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_statements_with_break(
    statements: &Statements<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &mut HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    break_label: &str,
    output: &mut String,
) -> Result<(), CompileError> {
    match statements {
        Statements::Then(then) => {
            // Compile the head statement
            compile_statement_with_break(
                &then.head,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                break_label,
                output,
            )?;

            // Compile the tail statements
            compile_statements_with_break(
                &then.tail,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                stack_offset,
                break_label,
                output,
            )
        }
        Statements::Statement(statement) => compile_statement_with_break(
            statement,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
            stack_offset,
            break_label,
            output,
        ),
        Statements::Nil => Ok(()),
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_statement_with_break(
    statement: &Statement<PhaseParse>,
    variables: &mut HashMap<String, i32>,
    reference_variables: &mut HashMap<String, String>,
    builtins: &HashMap<String, String>,
    arrays: &HashMap<String, ArrayInfo>,
    variable_arrays: &mut HashMap<String, String>,
    stack_offset: &mut i32,
    break_label: &str,
    output: &mut String,
) -> Result<(), CompileError> {
    match statement {
        Statement::Break(_) => {
            output.push_str(&format!("    jmp {break_label}\n"));
            Ok(())
        }
        Statement::Expr(ProcTerm::If(if_expr)) => {
            // Handle if expressions specially to pass break context
            compile_proc_if_with_break(
                if_expr,
                variables,
                reference_variables,
                builtins,
                arrays,
                variable_arrays,
                break_label,
                output,
            )
        }
        // For other statements, delegate to the normal compile_statement
        _ => StatementCompiler::compile_statement(
            statement,
            variables,
            reference_variables,
            builtins,
            arrays,
            variable_arrays,
            stack_offset,
            output,
        ),
    }
}
