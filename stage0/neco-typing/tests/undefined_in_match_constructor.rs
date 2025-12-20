// bad: Sort 0 := match x { undef => x }
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
fn check_rejects_undefined_in_match_constructor() {
    let term = Term::Match(TermMatch {
        t: Box::new(var(1)),
        arms: vec![TermMatchArm {
            constructor: TermVariable { variable_id: 99 },
            constructor_args: vec![],
            term: Box::new(var(1)),
        }],
    });
    let definitions = Definitions {
        types: BTreeMap::new(),
        variables: BTreeMap::from([(1, (Box::new(sort(0)), Box::new(term)))]),
    };

    assert!(!check(&definitions));
}
