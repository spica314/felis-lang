use crate::lexer::{TokenColon, TokenIdentifier};
use crate::{Parse, Result, Token};

use super::expected;
use crate::syntax::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructFieldDeclaration {
    pub token_ident: TokenIdentifier,
    pub ty: Term,
}

impl Parse for StructFieldDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let Some(token_ident) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after struct field name"));
        };
        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(expected("struct field type"));
        };

        *i = k;
        Ok(Some(Self { token_ident, ty }))
    }
}
