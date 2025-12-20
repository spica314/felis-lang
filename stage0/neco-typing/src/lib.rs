mod replace;
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
pub struct TermApply {
    pub f: Box<Term>,
    pub x: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermMatch {
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
    Apply(TermApply),
    Match(TermMatch),
    Sort(TermSort),
}
