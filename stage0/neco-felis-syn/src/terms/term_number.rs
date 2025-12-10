use crate::{Parse, ParseError, Phase, PhaseParse, token::TokenNumber};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermNumber<P: Phase> {
    pub number: TokenNumber,
    pub ext: P::TermNumberExt,
}

impl Parse for TermNumber<PhaseParse> {
    fn parse(tokens: &[crate::token::Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(token_number) = TokenNumber::parse(tokens, i)? {
            let term_number = TermNumber {
                number: token_number,
                ext: (),
            };

            return Ok(Some(term_number));
        }

        Ok(None)
    }
}
