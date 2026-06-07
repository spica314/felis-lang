use super::Term;
use super::parse::expected;
use crate::lexer::{TokenColon, TokenIdentifier};
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBinder {
    pub name: String,
    pub ty: Box<Term>,
}

impl Parse for TypedBinder {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let Some(name) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(expected("type after typed binder colon"));
        };
        *i = k;
        Ok(Some(Self {
            name: name.lexeme,
            ty: Box::new(ty),
        }))
    }
}
