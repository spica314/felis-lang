use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{Token, TokenParenL, TokenParenR},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermUnit<P: Phase> {
    pub paren_l: TokenParenL,
    pub paren_r: TokenParenR,
    pub ext: P::ProcTermUnitExt,
}

impl Parse for ProcTermUnit<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(paren_l) = TokenParenL::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(paren_r) = TokenParenR::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let proc_term_unit = ProcTermUnit {
            paren_l,
            paren_r,
            ext: (),
        };

        *i = k;
        Ok(Some(proc_term_unit))
    }
}
