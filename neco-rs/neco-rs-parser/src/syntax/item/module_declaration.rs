use crate::lexer::{TokenIdentifier, TokenKeyword, TokenKeywordKind, TokenSemicolon};
use crate::{Parse, Result, Token};

use super::expected;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: String,
}

impl Parse for ModuleDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) = TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Mod))?
        else {
            return Ok(None);
        };
        let Some(name) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("module name"));
        };
        let Some(_) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after module declaration"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name: name.lexeme,
        }))
    }
}
