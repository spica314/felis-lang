mod check;
mod replace;

use std::collections::BTreeMap;

pub use check::check;
pub use replace::replace;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermVariable {
    pub variable_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermForall {
    pub variable_id: usize,
    pub variable_ty: Box<Term>,
    pub body: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermArrow {
    pub from: Box<Term>,
    pub to: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermApply {
    pub f: Box<Term>,
    pub x: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermMatch {
    pub t: Box<Term>,
    pub arms: Vec<TermMatchArm>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermMatchArm {
    pub constructor: TermVariable,
    pub constructor_args: Vec<TermVariable>,
    pub term: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermSort {
    pub u: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Variable(TermVariable),
    Forall(TermForall),
    Arrow(TermArrow),
    Apply(TermApply),
    Match(TermMatch),
    Sort(TermSort),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefConstructor {
    pub variable_id: usize,
    pub ty: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDef {
    pub variable_id: usize,
    pub ty: Box<Term>,
    pub constructors: Vec<TypeDefConstructor>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Definitions {
    pub types: BTreeMap<usize, TypeDef>,
    pub variables: BTreeMap<usize, (Box<Term>, Box<Term>)>,
}
