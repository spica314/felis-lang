use crate::lexer::{TokenKeyword, TokenKeywordKind, TokenSemicolon};
use crate::{Parse, Result, Token};

use super::expected;
use crate::syntax::PathExpression;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub path: PathExpression,
}

impl Parse for UseDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) = TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Use))?
        else {
            return Ok(None);
        };
        let Some(path) = PathExpression::parse(tokens, &mut k)? else {
            return Err(expected("path after `use`"));
        };
        let Some(_) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after use declaration"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            path,
        }))
    }
}
