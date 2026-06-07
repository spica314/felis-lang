use crate::lexer::{TokenIdentifier, TokenKeyword, TokenKeywordKind, TokenSemicolon};
use crate::{Parse, Result, Token};

use super::expected;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntryPointDeclaration {
    pub token_keyword_entrypoint: TokenKeyword,
    pub token_ident: TokenIdentifier,
    pub token_semi: TokenSemicolon,
}

impl Parse for EntryPointDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let Some(token_keyword_entrypoint) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::EntryPoint))?
        else {
            return Ok(None);
        };
        let Some(token_ident) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("entrypoint name"));
        };
        let Some(token_semi) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after entrypoint declaration"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_entrypoint,
            token_ident,
            token_semi,
        }))
    }
}
