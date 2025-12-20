// GoodType: Sort 0
//    ctor: GoodType
// bad: match x { ctor => undef }
//    (undef is not defined)
use std::collections::BTreeMap;

use neco_typing::*;

fn var(variable_id: usize) -> Term {
    Term::Variable(TermVariable { variable_id })
}

fn sort(u: usize) -> Term {
    Term::Sort(TermSort { u })
}

#[test]
fn check_rejects_undefined_in_match_arm_rhs() {
    let good_type_id = 1;
    let ctor_id = 2;
    let type_def = TypeDef {
        variable_id: good_type_id,
        ty: Box::new(sort(0)),
        constructors: vec![TypeDefConstructor {
            variable_id: ctor_id,
            ty: Box::new(sort(0)),
        }],
    };
    let term = Term::Match(TermMatch {
        t: Box::new(var(1)),
        arms: vec![TermMatchArm {
            constructor: TermVariable {
                variable_id: ctor_id,
            },
            constructor_args: vec![],
            term: Box::new(var(99)),
        }],
    });
    let definitions = Definitions {
        types: BTreeMap::from([(good_type_id, type_def)]),
        variables: BTreeMap::from([(1, (Box::new(sort(0)), Box::new(term)))]),
    };

    assert!(!check(&definitions));
}
