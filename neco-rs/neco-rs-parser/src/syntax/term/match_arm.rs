use super::Term;
use super::{
    TermParseOption,
    parse::{expect_comma, expected},
};
use crate::lexer::{TokenComma, TokenFatArrow};
use crate::syntax::Pattern;
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub result: Box<Term>,
}

impl Parse for MatchArm {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let Some(pattern) = Pattern::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(_) = TokenFatArrow::parse(tokens, &mut k)? else {
            return Err(expected("`=>` after match pattern"));
        };
        let Some(result) = Term::parse_with_option(
            tokens,
            &mut k,
            Some(TermParseOption {
                stop_at_left_brace: false,
                stop_at_match_arm_boundary: true,
            }),
        )?
        else {
            return Err(expected("match arm result expression"));
        };
        if matches!(result, Term::Block(_)) {
            let _ = TokenComma::parse(tokens, &mut k)?;
        } else {
            expect_comma(tokens, &mut k)?;
        }
        *i = k;
        Ok(Some(Self {
            pattern,
            result: Box::new(result),
        }))
    }
}
