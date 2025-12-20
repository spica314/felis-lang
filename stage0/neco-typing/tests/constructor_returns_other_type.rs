// BadTypeA: Sort 0
// BadTypeB: Sort 0
//    bad_ctor: BadTypeA -> BadTypeA
//    (constructor result type is not BadTypeB)
use std::collections::BTreeMap;

use neco_typing::*;

fn var(variable_id: usize) -> Term {
    Term::Variable(TermVariable { variable_id })
}

fn sort(u: usize) -> Term {
    Term::Sort(TermSort { u })
}

fn arrow(from: Term, to: Term) -> Term {
    Term::Arrow(TermArrow {
        from: Box::new(from),
        to: Box::new(to),
    })
}

#[test]
fn check_rejects_constructor_returning_other_type() {
    let type_a_id = 1;
    let type_b_id = 2;
    let bad_ctor_id = 3;

    let type_a_def = TypeDef {
        variable_id: type_a_id,
        ty: Box::new(sort(0)),
        constructors: vec![],
    };

    let type_b_def = TypeDef {
        variable_id: type_b_id,
        ty: Box::new(sort(0)),
        constructors: vec![TypeDefConstructor {
            variable_id: bad_ctor_id,
            ty: Box::new(arrow(var(type_a_id), var(type_a_id))),
        }],
    };

    let definitions = Definitions {
        types: BTreeMap::from([(type_a_id, type_a_def), (type_b_id, type_b_def)]),
        variables: BTreeMap::new(),
    };

    assert!(!check(&definitions));
}
