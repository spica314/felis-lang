use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm, ProcTermNumber, ProcTermVariable,
    token::{Token, TokenOperator, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermMethodChain<P: Phase> {
    pub object: TokenVariable,
    pub dot: TokenOperator,
    pub field: TokenVariable,
    pub index: Option<Box<ProcTerm<P>>>,
    pub ext: P::ProcTermFieldAccessExt,
}

impl<P: Phase> ProcTermMethodChain<P> {
    pub fn object_name(&self) -> &str {
        self.object.s()
    }

    pub fn field_name(&self) -> &str {
        self.field.s()
    }
}

impl Parse for ProcTermMethodChain<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse object (variable)
        let Some(object) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Parse "." operator
        let Some(dot) = TokenOperator::parse_operator_after_whitespace(tokens, &mut k, ".")? else {
            return Ok(None);
        };

        // Parse field name (can be either a variable or keyword like #len)
        let field = if let Some(variable) = TokenVariable::parse(tokens, &mut k)? {
            variable
        } else if k < tokens.len() {
            // Check if it's a keyword token
            if let Token::Keyword(keyword) = &tokens[k] {
                // Create a pseudo-variable with the keyword content including #
                let field_token =
                    TokenVariable::new(keyword.pos().clone(), format!("#{}", keyword.s()));
                k += 1; // Advance past the keyword
                field_token
            } else {
                return Err(ParseError::Unknown("expected field name after '.'"));
            }
        } else {
            return Err(ParseError::Unknown("expected field name after '.'"));
        };

        // Try to parse an optional index
        // - Prefer numeric index
        // - Allow variable index unless it's clearly the start of the next argument
        let index = if let Some(number) = ProcTermNumber::parse(tokens, &mut k)? {
            Some(Box::new(ProcTerm::Number(number)))
        } else if k < tokens.len() {
            let can_take_variable_index = match &tokens[k] {
                Token::Variable(_) => {
                    // Peek if the following token is a '.' operator
                    let mut m = k + 1;
                    let next_is_dot = m < tokens.len()
                        && TokenOperator::parse_operator(tokens, &mut m, ".")?.is_some();
                    !next_is_dot
                }
                _ => false,
            };
            if can_take_variable_index {
                ProcTermVariable::parse(tokens, &mut k)?
                    .map(|variable| Box::new(ProcTerm::Variable(variable)))
            } else {
                None
            }
        } else {
            None
        };

        let proc_term_method_chain = ProcTermMethodChain {
            object,
            dot,
            field,
            index,
            ext: (),
        };

        *i = k;
        Ok(Some(proc_term_method_chain))
    }
}
