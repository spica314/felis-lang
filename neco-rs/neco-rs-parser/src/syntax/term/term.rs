use super::{ArrowTerm, Block, ForallTerm, MatchExpression, StructLiteralField, TypedBinder};
use crate::syntax::PathExpression;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Unit,
    StringLiteral(String),
    CharLiteral(char),
    IntegerLiteral(String),
    Path(PathExpression),
    Group(Box<Term>),
    TypedBinder(TypedBinder),
    Block(Block),
    Match(MatchExpression),
    Application {
        callee: Box<Term>,
        arguments: Vec<Term>,
    },
    MethodCall {
        receiver: Box<Term>,
        method: String,
    },
    FieldAccess {
        receiver: Box<Term>,
        field: String,
    },
    StructLiteral {
        path: PathExpression,
        fields: Vec<StructLiteralField>,
    },
    Reference {
        referent: Box<Term>,
        exclusive: bool,
    },
    Arrow(ArrowTerm),
    Forall(ForallTerm),
}
