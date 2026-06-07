use crate::lexer::{TokenIdentifier, TokenUnderscore};
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingPattern {
    Name(String),
    Wildcard,
}

impl Parse for BindingPattern {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        if TokenUnderscore::parse(tokens, i)?.is_some() {
            return Ok(Some(Self::Wildcard));
        }
        let Some(name) = TokenIdentifier::parse(tokens, i)? else {
            return Ok(None);
        };
        Ok(Some(Self::Name(name.lexeme)))
    }
}
