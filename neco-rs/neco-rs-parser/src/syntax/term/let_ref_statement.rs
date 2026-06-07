use super::Term;
use super::parse::{expect_semicolon, expected};
use crate::lexer::{TokenColon, TokenIdentifier, TokenKeyword, TokenKeywordKind};
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetRefStatement {
    pub reference: String,
    pub exclusive: bool,
    pub ty: Box<Term>,
    pub source: Box<Term>,
}

impl Parse for LetRefStatement {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let exclusive =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Excl))?
                .is_some();
        let Some(reference) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after let-ref name"));
        };
        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(expected("let-ref type"));
        };
        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Borrow))?
        else {
            return Err(expected("`borrow` in let-ref statement"));
        };
        let Some(source) = Term::parse(tokens, &mut k)? else {
            return Err(expected("let-ref source expression"));
        };
        expect_semicolon(tokens, &mut k)?;
        *i = k;
        Ok(Some(Self {
            reference: reference.lexeme,
            exclusive,
            ty: Box::new(ty),
            source: Box::new(source),
        }))
    }
}
