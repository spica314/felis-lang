use crate::*;

pub fn check(defs: &Definitions) -> bool {
    for (variable_id, type_def) in &defs.types {
        let r = check_type_def(defs, *variable_id, type_def);
        if !r {
            return false;
        }
    }

    for (variable_id, (term_ty, term)) in &defs.variables {
        let r = check_variable(defs, *variable_id, term_ty, term);
        if !r {
            return false;
        }
    }

    true
}

fn check_type_def(_defs: &Definitions, _variable_id: usize, _type_def: &TypeDef) -> bool {
    true
}

fn check_variable(_defs: &Definitions, _variable_id: usize, _term_ty: &Term, _term: &Term) -> bool {
    true
}
