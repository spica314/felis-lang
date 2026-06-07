use super::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetRefStatement {
    pub reference: String,
    pub exclusive: bool,
    pub ty: Box<Term>,
    pub source: Box<Term>,
}
