use crate::*;
use std::collections::BTreeSet;

pub fn check(defs: &Definitions) -> bool {
    let global_ids = collect_global_ids(defs);
    for (variable_id, type_def) in &defs.types {
        let r = check_type_def(defs, *variable_id, type_def, &global_ids);
        if !r {
            return false;
        }
    }

    for (variable_id, (term_ty, term)) in &defs.variables {
        let r = check_variable(defs, *variable_id, term_ty, term, &global_ids);
        if !r {
            return false;
        }
    }

    true
}

fn check_type_def(
    _defs: &Definitions,
    _variable_id: usize,
    type_def: &TypeDef,
    global_ids: &BTreeSet<usize>,
) -> bool {
    if !check_type_def_final_sort(type_def) {
        return false;
    }

    if !check_term_variable_ids(&type_def.ty, global_ids, &Vec::new()) {
        return false;
    }

    for constructor in &type_def.constructors {
        if !check_constructor_returns_defined_type(type_def, constructor) {
            return false;
        }
        if !check_term_variable_ids(&constructor.ty, global_ids, &Vec::new()) {
            return false;
        }
    }

    true
}

fn check_variable(
    _defs: &Definitions,
    _variable_id: usize,
    term_ty: &Term,
    term: &Term,
    global_ids: &BTreeSet<usize>,
) -> bool {
    if !check_term_variable_ids(term_ty, global_ids, &Vec::new()) {
        return false;
    }

    if !check_term_variable_ids(term, global_ids, &Vec::new()) {
        return false;
    }

    true
}

fn check_type_def_final_sort(type_def: &TypeDef) -> bool {
    check_term_final_sort(&type_def.ty)
}

fn check_term_final_sort(term: &Term) -> bool {
    match term {
        Term::Sort(_) => true,
        Term::Arrow(arrow) => check_term_final_sort(&arrow.to),
        Term::Forall(forall) => check_term_final_sort(&forall.body),
        _ => false,
    }
}

fn check_constructor_returns_defined_type(
    type_def: &TypeDef,
    constructor: &TypeDefConstructor,
) -> bool {
    let result_term = constructor_result_term(&constructor.ty);
    match type_constructor_id(result_term) {
        Some(variable_id) => variable_id == type_def.variable_id,
        None => false,
    }
}

fn constructor_result_term<'a>(term: &'a Term) -> &'a Term {
    match term {
        Term::Forall(forall) => constructor_result_term(&forall.body),
        Term::Arrow(arrow) => constructor_result_term(&arrow.to),
        _ => term,
    }
}

fn type_constructor_id(term: &Term) -> Option<usize> {
    match term {
        Term::Variable(variable) => Some(variable.variable_id),
        Term::Apply(apply) => type_constructor_id(&apply.f),
        _ => None,
    }
}

/// Collects all globally defined ids, including type constructors.
fn collect_global_ids(defs: &Definitions) -> BTreeSet<usize> {
    let mut global_ids = BTreeSet::new();
    for (variable_id, type_def) in &defs.types {
        global_ids.insert(*variable_id);
        for constructor in &type_def.constructors {
            global_ids.insert(constructor.variable_id);
        }
    }
    for variable_id in defs.variables.keys() {
        global_ids.insert(*variable_id);
    }
    global_ids
}

fn check_term_variable_ids(term: &Term, global_ids: &BTreeSet<usize>, locals: &Vec<usize>) -> bool {
    match term {
        Term::Variable(variable) => {
            global_ids.contains(&variable.variable_id) || locals.contains(&variable.variable_id)
        }
        Term::Forall(forall) => {
            if !check_term_variable_ids(&forall.variable_ty, global_ids, locals) {
                return false;
            }
            let mut next_locals = locals.clone();
            next_locals.push(forall.variable_id);
            check_term_variable_ids(&forall.body, global_ids, &next_locals)
        }
        Term::Arrow(arrow) => {
            check_term_variable_ids(&arrow.from, global_ids, locals)
                && check_term_variable_ids(&arrow.to, global_ids, locals)
        }
        Term::Apply(apply) => {
            check_term_variable_ids(&apply.f, global_ids, locals)
                && check_term_variable_ids(&apply.x, global_ids, locals)
        }
        Term::Match(term_match) => {
            if !check_term_variable_ids(&term_match.t, global_ids, locals) {
                return false;
            }
            for arm in &term_match.arms {
                if !global_ids.contains(&arm.constructor.variable_id) {
                    return false;
                }
                let mut next_locals = locals.clone();
                for constructor_arg in &arm.constructor_args {
                    next_locals.push(constructor_arg.variable_id);
                }
                if !check_term_variable_ids(&arm.term, global_ids, &next_locals) {
                    return false;
                }
            }
            true
        }
        Term::Sort(_) => true,
    }
}
