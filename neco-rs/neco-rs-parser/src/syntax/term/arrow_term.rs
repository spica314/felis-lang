use super::{ArrowParameter, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrowTerm {
    pub parameter: ArrowParameter,
    pub result: Box<Term>,
}
