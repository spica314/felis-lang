use super::{BindingPattern, LetOperator, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub binder: BindingPattern,
    pub ty: Box<Term>,
    pub operator: LetOperator,
    pub value: Box<Term>,
}
