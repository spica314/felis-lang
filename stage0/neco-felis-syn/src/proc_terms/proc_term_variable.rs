use crate::{Parse, ParseError, Phase, PhaseParse, token::TokenVariable};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermVariable<P: Phase> {
    pub variable: TokenVariable,
    pub ext: P::ProcTermVariableExt,
}

impl Parse for ProcTermVariable<PhaseParse> {
    fn parse(tokens: &[crate::token::Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(token_variable) = TokenVariable::parse(tokens, i)? {
            let proc_term_variable = ProcTermVariable {
                variable: token_variable,
                ext: (),
            };

            return Ok(Some(proc_term_variable));
        }

        Ok(None)
    }
}
