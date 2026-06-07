use crate::lexer::TokenColon;
use crate::{Parse, Result, Token};

use super::{DeclaredName, expected};
use crate::syntax::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorDeclaration {
    pub name: DeclaredName,
    pub ty: Term,
}

impl Parse for ConstructorDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let Some(name) = DeclaredName::parse(tokens, i)? else {
            return Ok(None);
        };
        let Some(_) = TokenColon::parse(tokens, i)? else {
            return Err(expected("`:` after constructor name"));
        };
        let Some(ty) = Term::parse(tokens, i)? else {
            return Err(expected("constructor type"));
        };
        Ok(Some(Self { name, ty }))
    }
}
