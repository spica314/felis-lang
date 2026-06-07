use super::parse::{expect_semicolon, expected};
use super::{BindingPattern, LetOperator, Term};
use crate::lexer::{TokenColon, TokenEquals, TokenLeftArrow};
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub binder: BindingPattern,
    pub ty: Box<Term>,
    pub operator: LetOperator,
    pub value: Box<Term>,
}

impl Parse for LetStatement {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let Some(binder) = BindingPattern::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after let binding pattern"));
        };
        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(expected("let binding type"));
        };
        let operator = if TokenEquals::parse(tokens, &mut k)?.is_some() {
            LetOperator::Equals
        } else {
            let Some(_) = TokenLeftArrow::parse(tokens, &mut k)? else {
                return Err(expected("`=` or `<-` in let statement"));
            };
            LetOperator::LeftArrow
        };
        let Some(value) = Term::parse(tokens, &mut k)? else {
            return Err(expected("let binding value"));
        };
        expect_semicolon(tokens, &mut k)?;
        *i = k;
        Ok(Some(Self {
            binder,
            ty: Box::new(ty),
            operator,
            value: Box::new(value),
        }))
    }
}
