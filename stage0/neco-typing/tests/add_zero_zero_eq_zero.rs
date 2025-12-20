// Eq: (t: Sort 1) -> t -> t -> Sort 0
//    refl: (t: Sort 1) -> (x: t) -> Eq t x x
//
// Nat: Sort 1
//    zero : Nat
//    succ : Nat -> Nat
//
// add :=
//     \forall n: Nat,
//         \forall m: Nat,
//             match n {
//                 zero => m,
//                 succ p => succ (add p m),
//             }
//
// add_zero_zero_eq_zero := Eq Nat (add zero zero) zero
//
// proof :=
//     refl Nat zero

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

fn apply(f: Term, x: Term) -> Term {
    Term::Apply(TermApply {
        f: Box::new(f),
        x: Box::new(x),
    })
}

#[test]
fn add_zero_zero_eq_zero_returns_true() {
    let eq_id = 1;
    let refl_id = 2;
    let nat_id = 3;
    let zero_id = 4;
    let succ_id = 5;
    let add_id = 6;
    let add_zero_zero_eq_zero_id = 7;
    let proof_id = 8;
    let t_id = 10;
    let x_id = 11;
    let n_id = 12;
    let m_id = 13;
    let p_id = 14;

    let eq_ty = forall(t_id, sort(1), arrow(var(t_id), arrow(var(t_id), sort(0))));
    let refl_ty = forall(
        t_id,
        sort(1),
        forall(
            x_id,
            var(t_id),
            apply(apply(apply(var(eq_id), var(t_id)), var(x_id)), var(x_id)),
        ),
    );
    let eq_def = TypeDef {
        variable_id: eq_id,
        ty: Box::new(eq_ty),
        constructors: vec![TypeDefConstructor {
            variable_id: refl_id,
            ty: Box::new(refl_ty),
        }],
    };

    let nat_def = TypeDef {
        variable_id: nat_id,
        ty: Box::new(sort(1)),
        constructors: vec![
            TypeDefConstructor {
                variable_id: zero_id,
                ty: Box::new(var(nat_id)),
            },
            TypeDefConstructor {
                variable_id: succ_id,
                ty: Box::new(arrow(var(nat_id), var(nat_id))),
            },
        ],
    };

    let add_match = Term::Match(TermMatch {
        t: Box::new(var(n_id)),
        arms: vec![
            TermMatchArm {
                constructor: TermVariable {
                    variable_id: zero_id,
                },
                constructor_args: vec![],
                term: Box::new(var(m_id)),
            },
            TermMatchArm {
                constructor: TermVariable {
                    variable_id: succ_id,
                },
                constructor_args: vec![TermVariable { variable_id: p_id }],
                term: Box::new(apply(
                    var(succ_id),
                    apply(apply(var(add_id), var(p_id)), var(m_id)),
                )),
            },
        ],
    });
    let add_def = forall(n_id, var(nat_id), forall(m_id, var(nat_id), add_match));

    let add_zero_zero = apply(apply(var(add_id), var(zero_id)), var(zero_id));
    let add_zero_zero_eq_zero = apply(
        apply(apply(var(eq_id), var(nat_id)), add_zero_zero),
        var(zero_id),
    );
    let proof = apply(apply(var(refl_id), var(nat_id)), var(zero_id));

    let definitions = Definitions {
        types: BTreeMap::from([(eq_id, eq_def), (nat_id, nat_def)]),
        variables: BTreeMap::from([
            (add_id, Box::new(add_def)),
            (add_zero_zero_eq_zero_id, Box::new(add_zero_zero_eq_zero)),
            (proof_id, Box::new(proof)),
        ]),
    };

    assert!(check(&definitions));
}
