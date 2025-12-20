// BadType: (t: Undef) -> Sort 0
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

#[test]
fn check_rejects_undefined_in_type_def_ty() {
    let bad_type_id = 1;
    let t_id = 2;
    let ty = forall(t_id, var(99), sort(0));

    let bad_def = TypeDef {
        variable_id: bad_type_id,
        ty: Box::new(ty),
        constructors: vec![],
    };

    let definitions = Definitions {
        types: BTreeMap::from([(bad_type_id, bad_def)]),
        variables: BTreeMap::new(),
    };

    assert!(!check(&definitions));
}
