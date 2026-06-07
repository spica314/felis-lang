use super::{Term, TypedBinder};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrowParameter {
    Binder(TypedBinder),
    Domain(Box<Term>),
}
