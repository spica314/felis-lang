// bad: (x: Undef) -> x -> Sort 0
//    (Undef is not defined)
use std::collections::BTreeMap;

use neco_typing::*;

fn var(variable_id: usize) -> Term {
    Term::Variable(TermVariable { variable_id })
}

fn sort(u: usize) -> Term {
    Term::Sort(TermSort { u })
}

fn forall(variable_id: usize, variable_ty: Term, body: Term) -> Term {
    Term::Forall(TermForall {
        variable_id,
        variable_ty: Box::new(variable_ty),
        body: Box::new(body),
    })
}

fn arrow(from: Term, to: Term) -> Term {
    Term::Arrow(TermArrow {
        from: Box::new(from),
        to: Box::new(to),
    })
}

#[test]
fn check_rejects_undefined_in_forall_var_ty() {
    let x_id = 2;
    let term = forall(x_id, var(99), arrow(var(x_id), sort(0)));
    let definitions = Definitions {
        types: BTreeMap::new(),
        variables: BTreeMap::from([(1, (Box::new(sort(0)), Box::new(term)))]),
    };

    assert!(!check(&definitions));
}
