use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{Token, TokenParenL, TokenParenR},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermUnit<P: Phase> {
    pub paren_l: TokenParenL,
    pub paren_r: TokenParenR,
    pub ext: P::TermUnitExt,
}

impl Parse for TermUnit<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(paren_l) = TokenParenL::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(paren_r) = TokenParenR::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let term_paren = TermUnit {
            paren_l,
            paren_r,
            ext: (),
        };

        *i = k;
        Ok(Some(term_paren))
    }
}
