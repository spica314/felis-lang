use crate::{Parse, Parser, Result, TokenKind};

use super::PathExpression;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Wildcard,
    Constructor {
        path: PathExpression,
        subpatterns: Vec<Pattern>,
    },
}

impl Parse for Pattern {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        if parser.consume_punctuation(TokenKind::Underscore) {
            return Ok(Some(Self::Wildcard));
        }
        let path = PathExpression::parse(parser)?.unwrap();
        let mut subpatterns = Vec::new();
        while parser.is_pattern_start() {
            subpatterns.push(Self::parse(parser)?.unwrap());
        }
        Ok(Some(Self::Constructor { path, subpatterns }))
    }
}
