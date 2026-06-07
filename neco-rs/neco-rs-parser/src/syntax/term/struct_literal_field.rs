use super::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLiteralField {
    pub name: String,
    pub value: Term,
}
