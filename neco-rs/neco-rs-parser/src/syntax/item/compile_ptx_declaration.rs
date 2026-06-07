use crate::lexer::{TokenIdentifier, TokenKeyword, TokenKeywordKind, TokenSemicolon};
use crate::{Parse, Result, Token};

use super::expected;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilePtxDeclaration {
    pub function_name: String,
    pub value_name: String,
}

impl Parse for CompilePtxDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::CompilePtx))?
        else {
            return Ok(None);
        };
        let Some(function_name) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("PTX function name after `#compile_ptx`"));
        };
        let Some(_) = TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::To))?
        else {
            return Err(expected("`#to` after PTX function name"));
        };
        let Some(value_name) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("PTX value name after `#to`"));
        };
        let Some(_) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after compile_ptx declaration"));
        };

        *i = k;
        Ok(Some(Self {
            function_name: function_name.lexeme,
            value_name: value_name.lexeme,
        }))
    }
}
