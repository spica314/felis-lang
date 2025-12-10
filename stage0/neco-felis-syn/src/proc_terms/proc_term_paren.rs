use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm,
    token::{Token, TokenParenL, TokenParenR},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermParen<P: Phase> {
    pub paren_l: TokenParenL,
    pub proc_term: Box<ProcTerm<P>>,
    pub paren_r: TokenParenR,
    pub ext: P::ProcTermParenExt,
}

impl Parse for ProcTermParen<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(paren_l) = TokenParenL::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(proc_term) = ProcTerm::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("proc_term_paren_1"));
        };

        let Some(paren_r) = TokenParenR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("proc_term_paren_2"));
        };

        let proc_term_paren = ProcTermParen {
            paren_l,
            proc_term: Box::new(proc_term),
            paren_r,
            ext: (),
        };

        *i = k;
        Ok(Some(proc_term_paren))
    }
}
