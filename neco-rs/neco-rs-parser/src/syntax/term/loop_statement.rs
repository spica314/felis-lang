use super::Block;
use super::parse::expect_semicolon;
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopStatement {
    pub body: Block,
}

impl Parse for LoopStatement {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let Some(body) = Block::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        expect_semicolon(tokens, &mut k)?;
        *i = k;
        Ok(Some(Self { body }))
    }
}
