use crate::{Keyword, Parse, Parser, Result, TokenKind};

use super::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathExpression {
    pub starts_with_package: bool,
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathSegment {
    pub name: String,
    pub suffixes: Vec<Term>,
}

impl Parse for PathExpression {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let mut starts_with_package = false;
        let mut segments = Vec::new();

        if parser.consume_keyword(Keyword::Package) {
            starts_with_package = true;
            parser.expect_punctuation(TokenKind::DoubleColon)?;
        }

        segments.push(PathSegment::parse(parser)?);
        while parser.consume_punctuation(TokenKind::DoubleColon) {
            segments.push(PathSegment::parse(parser)?);
        }

        Ok(Self {
            starts_with_package,
            segments,
        })
    }
}

impl Parse for PathSegment {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let name = parser.expect_identifier()?;
        let suffixes = parse_suffixes(parser)?;
        Ok(Self { name, suffixes })
    }
}

pub(crate) fn parse_suffixes(parser: &mut Parser) -> Result<Vec<Term>> {
    let mut suffixes = Vec::new();
    while parser.consume_punctuation(TokenKind::LeftBracket) {
        suffixes.push(Term::parse(parser)?);
        parser.expect_punctuation(TokenKind::RightBracket)?;
    }
    Ok(suffixes)
}
