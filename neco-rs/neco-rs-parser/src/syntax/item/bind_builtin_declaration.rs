use crate::lexer::{
    TokenIdentifier, TokenKeyword, TokenKeywordKind, TokenSemicolon, TokenStringLiteral,
};
use crate::{Parse, Result, Token};

use super::expected;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindBuiltinDeclaration {
    pub builtin_name: String,
    pub alias: String,
}

impl Parse for BindBuiltinDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::BindBuiltin))?
        else {
            return Ok(None);
        };
        let Some(builtin_name) = TokenStringLiteral::parse(tokens, &mut k)? else {
            return Err(expected("builtin name string literal"));
        };
        let Some(_) = TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::As))?
        else {
            return Err(expected("`as` after builtin name"));
        };
        let Some(alias) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("bind-builtin alias"));
        };
        let Some(_) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after bind-builtin declaration"));
        };

        *i = k;
        Ok(Some(Self {
            builtin_name: builtin_name.lexeme,
            alias: alias.lexeme,
        }))
    }
}
