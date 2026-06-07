use crate::lexer::{TokenColon, TokenKeyword, TokenKeywordKind};
use crate::{Parse, Result, Token};

use super::{DeclaredName, expected, parse_term_before_left_brace};
use crate::syntax::{Block, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TheoremDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: DeclaredName,
    pub statement: Term,
    pub body: Block,
}

impl Parse for TheoremDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Theorem))?
        else {
            return Ok(None);
        };
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("theorem name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after theorem name"));
        };
        let Some(statement) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("theorem statement"));
        };
        let Some(body) = Block::parse(tokens, &mut k)? else {
            return Err(expected("theorem body"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name,
            statement,
            body,
        }))
    }
}
