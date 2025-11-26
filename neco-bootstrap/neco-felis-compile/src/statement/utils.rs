use neco_felis_syn::*;

pub fn count_let_variables_in_statements(statements: &Statements<PhaseParse>) -> i32 {
    match statements {
        Statements::Then(then) => {
            count_let_variables_in_statement(&then.head)
                + count_let_variables_in_statements(&then.tail)
        }
        Statements::Statement(statement) => count_let_variables_in_statement(statement),
        Statements::Nil => 0,
    }
}

pub fn count_let_variables_in_statement(statement: &Statement<PhaseParse>) -> i32 {
    match statement {
        Statement::Let(_) => 1,
        Statement::LetMut(_) => 2, // let mut uses 2 stack slots: one for value, one for reference
        Statement::Expr(proc_term) => count_let_variables_in_proc_term(proc_term),
        _ => 0,
    }
}

pub fn count_let_variables_in_proc_term(_proc_term: &ProcTerm<PhaseParse>) -> i32 {
    0
}
