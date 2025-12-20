// GoodType: Sort 0
//    ctor: GoodType
// bad : GoodType -> GoodType := \forall x: GoodType, match x { ctor => undef }
//    (undef is not defined)
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
fn check_rejects_undefined_in_match_arm_rhs() {
    let good_type_id = 1;
    let ctor_id = 2;
    let bad_id = 3;
    let x_id = 4;
    let type_def = TypeDef {
        variable_id: good_type_id,
        ty: Box::new(sort(0)),
        constructors: vec![TypeDefConstructor {
            variable_id: ctor_id,
            ty: Box::new(var(good_type_id)),
        }],
    };
    let term = Term::Match(TermMatch {
        t: Box::new(var(x_id)),
        arms: vec![TermMatchArm {
            constructor: TermVariable {
                variable_id: ctor_id,
            },
            constructor_args: vec![],
            term: Box::new(var(99)),
        }],
    });
    let term = forall(x_id, var(good_type_id), term);
    let term_ty = arrow(var(good_type_id), var(good_type_id));
    let definitions = Definitions {
        types: BTreeMap::from([(good_type_id, type_def)]),
        variables: BTreeMap::from([(bad_id, (Box::new(term_ty), Box::new(term)))]),
    };

    assert!(!check(&definitions));
}
