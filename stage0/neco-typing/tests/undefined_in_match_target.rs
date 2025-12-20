// bad: Sort 0 := match undef { }
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
fn check_rejects_undefined_in_match_target() {
    let term = Term::Match(TermMatch {
        t: Box::new(var(99)),
        arms: vec![],
    });
    let definitions = Definitions {
        types: BTreeMap::new(),
        variables: BTreeMap::from([(1, (Box::new(sort(0)), Box::new(term)))]),
    };

    assert!(!check(&definitions));
}
