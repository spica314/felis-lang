use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm, ProcTermMethodChain, ProcTermNumber,
    ProcTermVariable,
    token::{Token, TokenOperator},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementMethodChainAssign<P: Phase> {
    pub method_chain: ProcTermMethodChain<P>,
    pub equals: TokenOperator,
    pub value: Box<ProcTerm<P>>,
    pub ext: P::StatementFieldAssignExt,
}

impl Parse for StatementMethodChainAssign<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse method chain access (e.g., "points .x 0")
        let Some(method_chain) = ProcTermMethodChain::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Parse "<-" operator
        let Some(equals) = TokenOperator::parse_operator(tokens, &mut k, "<-")? else {
            return Ok(None);
        };

        // Parse value expression (simple terms only to avoid infinite recursion)
        let value = if let Some(number) = ProcTermNumber::parse(tokens, &mut k)? {
            ProcTerm::Number(number)
        } else if let Some(variable) = ProcTermVariable::parse(tokens, &mut k)? {
            ProcTerm::Variable(variable)
        } else {
            return Err(ParseError::Unknown("expected value expression after '<-'"));
        };

        let statement_method_chain = StatementMethodChainAssign {
            method_chain,
            equals,
            value: Box::new(value),
            ext: (),
        };

        *i = k;
        Ok(Some(statement_method_chain))
    }
}
