// BadType: (x: Sort 1) -> x
//    (final sort is not Sort/Arrow/Forall)
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

#[test]
fn check_type_def_final_sort_rejects_non_sort() {
    let bad_type_id = 1;
    let x_id = 2;
    let invalid_ty = forall(x_id, sort(1), var(x_id));

    let bad_def = TypeDef {
        variable_id: bad_type_id,
        ty: Box::new(invalid_ty),
        constructors: vec![],
    };

    let definitions = Definitions {
        types: BTreeMap::from([(bad_type_id, bad_def)]),
        variables: BTreeMap::new(),
    };

    assert!(!check(&definitions));
}
