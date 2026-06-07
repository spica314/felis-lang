use super::{Term, TypedBinder};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForallTerm {
    pub binder: TypedBinder,
    pub body: Box<Term>,
}
