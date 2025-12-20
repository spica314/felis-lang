use crate::*;

pub fn replace(t: &Term, from: usize, to: Term) -> Term {
    match t {
        Term::Variable(variable) => {
            if variable.variable_id == from {
                to.clone()
            } else {
                t.clone()
            }
        }
        Term::Forall(forall) => {
            if forall.variable_id == from {
                panic!()
            }
            let sub_ty = replace(&forall.variable_ty.clone(), from, to.clone());
            let sub_term = replace(&forall.body.clone(), from, to.clone());
            Term::Forall(TermForall {
                variable_id: forall.variable_id,
                variable_ty: Box::new(sub_ty),
                body: Box::new(sub_term),
            })
        }
        Term::Arrow(arrow) => {
            let sub_from = replace(&arrow.from.clone(), from, to.clone());
            let sub_to = replace(&arrow.to.clone(), from, to.clone());
            Term::Arrow(TermArrow {
                from: Box::new(sub_from),
                to: Box::new(sub_to),
            })
        }
        Term::Apply(apply) => {
            let sub_f = replace(&apply.f.clone(), from, to.clone());
            let sub_x = replace(&apply.x.clone(), from, to.clone());
            Term::Apply(TermApply {
                f: Box::new(sub_f),
                x: Box::new(sub_x),
            })
        }
        Term::Match(term_match) => {
            let mut arms = vec![];
            for arm in &term_match.arms {
                if arm.constructor.variable_id == from {
                    panic!();
                }
                for constructor_arg in &arm.constructor_args {
                    if constructor_arg.variable_id == from {
                        panic!();
                    }
                }

                let sub_term = replace(&arm.term.clone(), from, to.clone());
                arms.push(TermMatchArm {
                    constructor: arm.constructor.clone(),
                    constructor_args: arm.constructor_args.clone(),
                    term: Box::new(sub_term),
                })
            }
            Term::Match(TermMatch { arms: arms })
        }
        Term::Sort(_) => t.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(id: usize) -> Term {
        Term::Variable(TermVariable { variable_id: id })
    }

    #[test]
    fn replace_variable_matches() {
        let t = var(1);
        let replaced = replace(&t, 1, var(2));
        assert_eq!(replaced, var(2));
    }

    #[test]
    fn replace_variable_non_match() {
        let t = var(1);
        let replaced = replace(&t, 2, var(3));
        assert_eq!(replaced, var(1));
    }

    #[test]
    fn replace_under_forall_non_shadowing() {
        let t = Term::Forall(TermForall {
            variable_id: 10,
            variable_ty: Box::new(var(1)),
            body: Box::new(Term::Apply(TermApply {
                f: Box::new(var(1)),
                x: Box::new(var(2)),
            })),
        });
        let replaced = replace(&t, 1, var(99));
        let expected = Term::Forall(TermForall {
            variable_id: 10,
            variable_ty: Box::new(var(99)),
            body: Box::new(Term::Apply(TermApply {
                f: Box::new(var(99)),
                x: Box::new(var(2)),
            })),
        });
        assert_eq!(replaced, expected);
    }

    #[test]
    #[should_panic]
    fn replace_panics_when_forall_binds_from() {
        let t = Term::Forall(TermForall {
            variable_id: 1,
            variable_ty: Box::new(var(2)),
            body: Box::new(var(3)),
        });
        let _ = replace(&t, 1, var(99));
    }

    #[test]
    fn replace_under_match_non_shadowing() {
        let t = Term::Match(TermMatch {
            arms: vec![TermMatchArm {
                constructor: TermVariable { variable_id: 10 },
                constructor_args: vec![TermVariable { variable_id: 20 }],
                term: Box::new(Term::Apply(TermApply {
                    f: Box::new(var(1)),
                    x: Box::new(var(2)),
                })),
            }],
        });
        let replaced = replace(&t, 1, var(42));
        let expected = Term::Match(TermMatch {
            arms: vec![TermMatchArm {
                constructor: TermVariable { variable_id: 10 },
                constructor_args: vec![TermVariable { variable_id: 20 }],
                term: Box::new(Term::Apply(TermApply {
                    f: Box::new(var(42)),
                    x: Box::new(var(2)),
                })),
            }],
        });
        assert_eq!(replaced, expected);
    }

    #[test]
    fn replace_under_arrow() {
        let t = Term::Arrow(TermArrow {
            from: Box::new(var(1)),
            to: Box::new(Term::Apply(TermApply {
                f: Box::new(var(2)),
                x: Box::new(var(1)),
            })),
        });
        let replaced = replace(&t, 2, var(42));
        let expected = Term::Arrow(TermArrow {
            from: Box::new(var(1)),
            to: Box::new(Term::Apply(TermApply {
                f: Box::new(var(42)),
                x: Box::new(var(1)),
            })),
        });
        assert_eq!(replaced, expected);
    }

    #[test]
    fn replace_sort_is_noop() {
        let t = Term::Sort(TermSort { u: 1 });
        let replaced = replace(&t, 1, var(99));
        assert_eq!(replaced, t);
    }

    #[test]
    #[should_panic]
    fn replace_panics_when_match_binds_from() {
        let t = Term::Match(TermMatch {
            arms: vec![TermMatchArm {
                constructor: TermVariable { variable_id: 1 },
                constructor_args: vec![],
                term: Box::new(var(2)),
            }],
        });
        let _ = replace(&t, 1, var(99));
    }

    #[test]
    #[should_panic]
    fn replace_panics_when_match_constructor_arg_binds_from() {
        let t = Term::Match(TermMatch {
            arms: vec![TermMatchArm {
                constructor: TermVariable { variable_id: 10 },
                constructor_args: vec![TermVariable { variable_id: 1 }],
                term: Box::new(var(2)),
            }],
        });
        let _ = replace(&t, 1, var(99));
    }
}
