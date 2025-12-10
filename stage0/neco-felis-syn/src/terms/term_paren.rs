use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{Token, TokenParenL, TokenParenR},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermParen<P: Phase> {
    pub paren_l: TokenParenL,
    pub term: Box<Term<P>>,
    pub paren_r: TokenParenR,
    pub ext: P::TermParenExt,
}

impl<P: Phase> TermParen<P> {
    pub fn term(&self) -> &Term<P> {
        &self.term
    }
}

impl Parse for TermParen<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(paren_l) = TokenParenL::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(term) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("term_paren_1"));
        };

        let Some(paren_r) = TokenParenR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("term_paren_2"));
        };

        let term_paren = TermParen {
            paren_l,
            term: Box::new(term),
            paren_r,
            ext: (),
        };

        *i = k;
        Ok(Some(term_paren))
    }
}
