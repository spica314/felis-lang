use crate::{Parse, ParseError, Phase, PhaseParse, token::TokenNumber};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermNumber<P: Phase> {
    pub number: TokenNumber,
    pub ext: P::ProcTermNumberExt,
}

impl Parse for ProcTermNumber<PhaseParse> {
    fn parse(tokens: &[crate::token::Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(token_number) = TokenNumber::parse(tokens, i)? {
            let proc_term_number = ProcTermNumber {
                number: token_number,
                ext: (),
            };

            return Ok(Some(proc_term_number));
        }

        Ok(None)
    }
}
