// BadType: Sort 0
//    bad_ctor: (x: Undef) -> BadType
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
fn check_rejects_undefined_in_constructor_ty() {
    let bad_type_id = 1;
    let bad_ctor_id = 2;
    let x_id = 3;
    let ctor_ty = forall(x_id, var(99), var(bad_type_id));

    let bad_def = TypeDef {
        variable_id: bad_type_id,
        ty: Box::new(sort(0)),
        constructors: vec![TypeDefConstructor {
            variable_id: bad_ctor_id,
            ty: Box::new(ctor_ty),
        }],
    };

    let definitions = Definitions {
        types: BTreeMap::from([(bad_type_id, bad_def)]),
        variables: BTreeMap::new(),
    };

    assert!(!check(&definitions));
}
