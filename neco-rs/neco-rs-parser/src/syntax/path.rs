use crate::{
    Error, Parse, Result, Token,
    lexer::{TokenDoubleColon, TokenIdentifier, TokenKeyword, TokenKeywordKind},
};

use super::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathExpression {
    pub token_keyword_package: Option<TokenKeyword>,
    pub segments: Vec<TokenIdentifier>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathSegment {
    pub name: String,
    pub suffixes: Vec<Term>,
}

impl Parse for PathExpression {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let mut segments = vec![];

        let token_keyword_package =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Package))?;
        if token_keyword_package.is_some() {
            let Some(_) = TokenDoubleColon::parse(tokens, &mut k)? else {
                return Err(Error::Message("expected `::` after `package`".to_string()));
            };
            let Some(token_identifier) = TokenIdentifier::parse(tokens, &mut k)? else {
                return Err(Error::Message(
                    "expected path segment after `package::`".to_string(),
                ));
            };
            segments.push(token_identifier);
        } else {
            let Some(token_identifier) = TokenIdentifier::parse(tokens, &mut k)? else {
                return Err(Error::Message("expected path segment".to_string()));
            };
            segments.push(token_identifier);
        }

        while let Some(_double_colon) = TokenDoubleColon::parse(tokens, &mut k)? {
            let Some(token_identifier) = TokenIdentifier::parse(tokens, &mut k)? else {
                return Err(Error::Message(
                    "expected path segment after `::`".to_string(),
                ));
            };
            segments.push(token_identifier);
        }

        *i = k;
        Ok(Some(Self {
            token_keyword_package,
            segments,
        }))
    }
}
