use crate::lexer::{TokenIdentifier, TokenLeftBracket, TokenRightBracket};
use crate::{Parse, Result, Token};

use super::expected;
use crate::syntax::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredName {
    pub name: String,
    pub suffixes: Vec<Term>,
}

impl Parse for DeclaredName {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let Some(name) = TokenIdentifier::parse(tokens, i)? else {
            return Ok(None);
        };
        let suffixes = parse_suffixes(tokens, i)?;
        Ok(Some(Self {
            name: name.lexeme,
            suffixes,
        }))
    }
}

fn parse_suffixes(tokens: &[Token], i: &mut usize) -> Result<Vec<Term>> {
    let mut suffixes = Vec::new();
    while TokenLeftBracket::parse(tokens, i)?.is_some() {
        let Some(suffix) = Term::parse(tokens, i)? else {
            return Err(expected("declared-name suffix term"));
        };
        let Some(_) = TokenRightBracket::parse(tokens, i)? else {
            return Err(expected("`]` after declared-name suffix"));
        };
        suffixes.push(suffix);
    }
    Ok(suffixes)
}
