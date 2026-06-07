use super::{
    ArrowTerm, Block, ForallTerm, MatchExpression, StructLiteralField, TermParseOption,
    TypedBinder, parse::parse_arrow_term,
};
use crate::syntax::PathExpression;
use crate::{Parse, Result, Token};

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

impl Parse for Term {
    type ParseOption = TermParseOption;

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        option: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        parse_arrow_term(tokens, i, option.unwrap_or_default())
    }
}
