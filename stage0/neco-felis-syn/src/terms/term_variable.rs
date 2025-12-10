use crate::{Parse, ParseError, Phase, PhaseParse, token::TokenVariable};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermVariable<P: Phase> {
    pub variable: TokenVariable,
    pub ext: P::TermVariableExt,
}

impl<P: Phase> TermVariable<P> {
    pub fn variable(&self) -> &TokenVariable {
        &self.variable
    }
}

impl Parse for TermVariable<PhaseParse> {
    fn parse(tokens: &[crate::token::Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(token_variable) = TokenVariable::parse(tokens, i)? {
            let term_variable = TermVariable {
                variable: token_variable,
                ext: (),
            };

            return Ok(Some(term_variable));
        }

        Ok(None)
    }
}
