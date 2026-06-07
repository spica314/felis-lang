use crate::lexer::{TokenColon, TokenIdentifier, TokenKeyword, TokenKeywordKind};
use crate::{Parse, Result, Token};

use super::{DeclaredName, expected, parse_term_before_left_brace};
use crate::syntax::{Block, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: DeclaredName,
    pub ty: Term,
    pub effect: Option<TokenIdentifier>,
    pub body: Block,
}

impl Parse for FunctionDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Fn))?.is_none() {
            return Ok(None);
        }
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("function name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after function name"));
        };
        let Some(ty) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("function type"));
        };
        let effect =
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::With))?
                .is_some()
            {
                Some(
                    TokenIdentifier::parse(tokens, &mut k)?
                        .ok_or_else(|| expected("function effect after `with`"))?,
                )
            } else {
                None
            };
        let Some(body) = Block::parse(tokens, &mut k)? else {
            return Err(expected("function body"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name,
            ty,
            effect,
            body,
        }))
    }
}
