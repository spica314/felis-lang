// BadType: Sort 0
//    bad_ctor: Sort 0
//    (constructor result type is Sort, not BadType)
use std::collections::BTreeMap;

use neco_typing::*;

fn sort(u: usize) -> Term {
    Term::Sort(TermSort { u })
}

#[test]
fn check_rejects_constructor_returning_sort() {
    let bad_type_id = 1;
    let bad_ctor_id = 2;

    let bad_def = TypeDef {
        variable_id: bad_type_id,
        ty: Box::new(sort(0)),
        constructors: vec![TypeDefConstructor {
            variable_id: bad_ctor_id,
            ty: Box::new(sort(0)),
        }],
    };

    let definitions = Definitions {
        types: BTreeMap::from([(bad_type_id, bad_def)]),
        variables: BTreeMap::new(),
    };

    assert!(!check(&definitions));
}
