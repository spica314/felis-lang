use crate::{
    Parse, ParseError, Phase, PhaseParse, Statements,
    token::{Token, TokenBraceL, TokenBraceR},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemProcBlock<P: Phase> {
    pub brace_l: TokenBraceL,
    pub statements: Statements<P>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemProcBlockExt,
}

impl Parse for ItemProcBlock<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(statements) = Statements::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let item_proc_block = ItemProcBlock {
            brace_l,
            statements,
            brace_r,
            ext: (),
        };
        *i = k;
        Ok(Some(item_proc_block))
    }
}
