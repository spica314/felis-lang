use crate::{Keyword, Parse, Parser, Result, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Cfg(CfgPredicate),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CfgPredicate {
    Feature(String),
    Not(Box<CfgPredicate>),
}

impl Parse for Attribute {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let name = parser.expect_identifier()?;
        if name != "cfg" {
            return Err(parser.error_here(format!("unsupported attribute `{name}`")));
        }
        parser.expect_punctuation(TokenKind::LeftParen)?;
        let predicate = CfgPredicate::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::RightParen)?;
        parser.expect_punctuation(TokenKind::RightBracket)?;
        Ok(Some(Self::Cfg(predicate)))
    }
}

impl Parse for CfgPredicate {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let identifier = parser.expect_identifier()?;
        if identifier == "feature" {
            parser.expect_punctuation(TokenKind::Equals)?;
            let value = parser.expect_string_literal()?;
            return Ok(Some(Self::Feature(value)));
        }
        if identifier != "not" {
            return Err(parser.error_here(format!("unsupported cfg predicate `{identifier}`")));
        }
        parser.expect_punctuation(TokenKind::LeftParen)?;
        let predicate = Self::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::RightParen)?;
        Ok(Some(Self::Not(Box::new(predicate))))
    }
}

pub(crate) fn parse_attributes(parser: &mut Parser) -> Result<Vec<Attribute>> {
    let mut attributes = Vec::new();
    while parser.consume_punctuation(TokenKind::AttributeStart) {
        attributes.push(Attribute::parse(parser)?.unwrap());
    }
    Ok(attributes)
}

pub(crate) fn parse_visibility(parser: &mut Parser) -> crate::syntax::Visibility {
    if parser.consume_keyword(Keyword::Pub) {
        crate::syntax::Visibility::Public
    } else {
        crate::syntax::Visibility::Private
    }
}
