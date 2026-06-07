use super::{Statement, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail: Option<Box<Term>>,
}
