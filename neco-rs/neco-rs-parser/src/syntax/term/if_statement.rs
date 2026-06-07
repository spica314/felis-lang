use super::TermParseOption;
use super::parse::{expect_semicolon, expected};
use super::{Block, ElseBranch, Term};
use crate::lexer::{TokenKeyword, TokenKeywordKind};
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement {
    pub condition: Box<Term>,
    pub then_block: Block,
    pub else_branch: Option<ElseBranch>,
}

impl Parse for IfStatement {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let Some(condition) = Term::parse_with_option(
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
        let Some(then_block) = Block::parse(tokens, &mut k)? else {
            return Err(expected("then block after if condition"));
        };
        let else_branch =
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Else))?
                .is_some()
            {
                if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::If))?
                    .is_some()
                {
                    Some(ElseBranch::If(Box::new(
                        IfStatement::parse(tokens, &mut k)?
                            .ok_or_else(|| expected("if statement after `else if`"))?,
                    )))
                } else {
                    Some(ElseBranch::Block(
                        Block::parse(tokens, &mut k)?.ok_or_else(|| expected("else block"))?,
                    ))
                }
            } else {
                None
            };
        if !matches!(else_branch, Some(ElseBranch::If(_))) {
            expect_semicolon(tokens, &mut k)?;
        }
        *i = k;
        Ok(Some(Self {
            condition: Box::new(condition),
            then_block,
            else_branch,
        }))
    }
}
