use super::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBinder {
    pub name: String,
    pub ty: Box<Term>,
}
