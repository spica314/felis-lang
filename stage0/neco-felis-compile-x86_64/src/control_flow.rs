use crate::{AssemblyCompiler, CompileError};
use neco_felis_syn::*;

/// Compile an if statement with condition checking and branching
pub fn compile_proc_if(
    compiler: &mut AssemblyCompiler,
    if_expr: &ProcTermIf<PhaseParse>,
) -> Result<(), CompileError> {
    static mut LABEL_COUNTER: u32 = 0;
    let label_id = unsafe {
        LABEL_COUNTER += 1;
        LABEL_COUNTER
    };

    let end_label = format!("if_end_{label_id}");
    let else_label = format!("if_else_{label_id}");

    // Compile condition
    match &*if_expr.condition {
        Statements::Statement(statement)
            if matches!(**statement, Statement::Expr(ProcTerm::Apply(_))) =>
        {
            let Statement::Expr(ProcTerm::Apply(apply)) = &**statement else {
                unreachable!()
            };
            // Handle builtin equality checks like __u64_eq
            if let ProcTerm::Variable(var) = &*apply.f
                && let Some(builtin) = compiler.builtins.get(var.variable.s())
                && builtin == "u64_eq"
            {
                if apply.args.len() != 2 {
                    return Err(CompileError::UnsupportedConstruct(format!(
                        "u64_eq expects 2 arguments, got {}",
                        apply.args.len()
                    )));
                }

                // Load first argument into rax
                compiler.load_proc_argument_into_register(&apply.args[0], "rax")?;
                // Load second argument into rbx
                compiler.load_proc_argument_into_register(&apply.args[1], "rbx")?;

                // Compare the values
                compiler.output.push_str("    cmp rax, rbx\n");

                // Jump to else label if not equal (condition is false)
                if if_expr.else_clause.is_some() {
                    compiler.output.push_str(&format!("    jne {else_label}\n"));
                } else {
                    compiler.output.push_str(&format!("    jne {end_label}\n"));
                }
            } else {
                return Err(CompileError::UnsupportedConstruct(format!(
                    "Unsupported condition in if: {:?}",
                    if_expr.condition
                )));
            }
        }
        _ => {
            return Err(CompileError::UnsupportedConstruct(format!(
                "Unsupported condition type in if: {:?}",
                if_expr.condition
            )));
        }
    }

    // Compile then body
    compiler.compile_statements(&if_expr.then_body)?;

    // If there's an else clause, jump to end after then body
    if if_expr.else_clause.is_some() {
        compiler.output.push_str(&format!("    jmp {end_label}\n"));

        // Else label
        compiler.output.push_str(&format!("{else_label}:\n"));

        // Compile else body
        if let Some(else_clause) = &if_expr.else_clause {
            compiler.compile_statements(&else_clause.else_body)?;
        }
    }

    // End label
    compiler.output.push_str(&format!("{end_label}:\n"));

    Ok(())
}

/// Compile a loop statement with start and end labels
pub fn compile_loop(
    compiler: &mut AssemblyCompiler,
    loop_stmt: &StatementLoop<PhaseParse>,
) -> Result<(), CompileError> {
    static mut LOOP_COUNTER: u32 = 0;
    let loop_id = unsafe {
        LOOP_COUNTER += 1;
        LOOP_COUNTER
    };

    let loop_start_label = format!("loop_start_{loop_id}");
    let loop_end_label = format!("loop_end_{loop_id}");

    // Push the end label onto the loop stack for break statements
    compiler.loop_stack.push(loop_end_label.clone());

    // Loop start label
    compiler.output.push_str(&format!("{loop_start_label}:\n"));

    // Compile loop body
    compiler.compile_statements(&loop_stmt.body)?;

    // Jump back to start of loop
    compiler
        .output
        .push_str(&format!("    jmp {loop_start_label}\n"));

    // Loop end label (for break statements)
    compiler.output.push_str(&format!("{loop_end_label}:\n"));

    // Pop the loop from the stack
    compiler.loop_stack.pop();

    Ok(())
}

/// Compile a break statement that jumps to the innermost loop's end label
pub fn compile_break(
    compiler: &mut AssemblyCompiler,
    _break_stmt: &StatementBreak<PhaseParse>,
) -> Result<(), CompileError> {
    // Get the innermost loop's end label
    if let Some(loop_end_label) = compiler.loop_stack.last() {
        compiler
            .output
            .push_str(&format!("    jmp {loop_end_label}\n"));
        Ok(())
    } else {
        Err(CompileError::UnsupportedConstruct(
            "break statement outside of loop".to_string(),
        ))
    }
}
