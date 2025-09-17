use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm, ProcTermFieldAccess, ProcTermMethodChain,
    ProcTermNumber, ProcTermVariable,
    token::{Token, TokenOperator, TokenVariable},
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

        // Prefer legacy syntax: method chain + '<-'
        if let Some(method_chain) = ProcTermMethodChain::parse(tokens, &mut k)? {
            // Parse "<-" operator
            let Some(equals) = TokenOperator::parse_operator(tokens, &mut k, "<-")? else {
                return Ok(None);
            };

            // Parse value expression (simple terms only)
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
            return Ok(Some(statement_method_chain));
        }

        // New syntax sugar: FieldAccess "." "set" <index> <value>
        let mut k2 = *i;
        if let Some(field_access) = ProcTermFieldAccess::parse(tokens, &mut k2)?
            && let Some(dot) = TokenOperator::parse_operator_after_whitespace(tokens, &mut k2, ".")?
        {
            let _ = dot; // suppress unused
            if let Some(set_kw) = TokenVariable::parse(tokens, &mut k2)?
                && set_kw.s() == "set"
            {
                // Parse index term
                let index = if let Some(number) = ProcTermNumber::parse(tokens, &mut k2)? {
                    ProcTerm::Number(number)
                } else if let Some(variable) = ProcTermVariable::parse(tokens, &mut k2)? {
                    ProcTerm::Variable(variable)
                } else {
                    return Err(ParseError::Unknown("expected index after '.set'"));
                };

                // Parse value term
                let value = if let Some(number) = ProcTermNumber::parse(tokens, &mut k2)? {
                    ProcTerm::Number(number)
                } else if let Some(variable) = ProcTermVariable::parse(tokens, &mut k2)? {
                    ProcTerm::Variable(variable)
                } else {
                    return Err(ParseError::Unknown("expected value after index in '.set'"));
                };

                // Build a synthetic ProcTermMethodChain from field_access + index
                let method_chain = ProcTermMethodChain {
                    object: field_access.object.clone(),
                    dot: dot.clone(),
                    field: field_access.field.clone(),
                    index: Some(Box::new(index)),
                    ext: (),
                };

                let statement_method_chain = StatementMethodChainAssign {
                    method_chain,
                    equals: dot,
                    value: Box::new(value),
                    ext: (),
                };

                *i = k2;
                return Ok(Some(statement_method_chain));
            }
        }

        Ok(None)
    }
}
