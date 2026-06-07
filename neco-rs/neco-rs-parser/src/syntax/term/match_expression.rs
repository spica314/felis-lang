use super::{MatchArm, Term};
use super::{TermParseOption, parse::expected};
use crate::lexer::{TokenLeftBrace, TokenRightBrace};
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchExpression {
    pub scrutinee: Box<Term>,
    pub arms: Vec<MatchArm>,
}

impl Parse for MatchExpression {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let Some(scrutinee) = Term::parse_with_option(
            tokens,
            &mut k,
            Some(TermParseOption {
                stop_at_left_brace: true,
                stop_at_match_arm_boundary: false,
            }),
        )?
        else {
            return Ok(None);
        };
        let Some(_) = TokenLeftBrace::parse(tokens, &mut k)? else {
            return Err(expected("match arm block"));
        };
        let mut arms = Vec::new();
        while TokenRightBrace::parse(tokens, &mut k)?.is_none() {
            let Some(arm) = MatchArm::parse(tokens, &mut k)? else {
                return Err(expected("match arm"));
            };
            arms.push(arm);
        }
        *i = k;
        Ok(Some(Self {
            scrutinee: Box::new(scrutinee),
            arms,
        }))
    }
}
