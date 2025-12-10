use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm, ProcTermNumber, ProcTermVariable, TokenColon2,
    token::{Token, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermConstructorCall<P: Phase> {
    pub type_name: TokenVariable,
    pub type_args: Vec<TokenVariable>,
    pub colon2: TokenColon2,
    // Accept either a keyword (e.g. #new_with_size) or a plain variable (new_with_size)
    // Normalize to TokenVariable internally
    pub method: TokenVariable,
    pub args: Vec<ProcTerm<P>>,
    pub ext: P::ProcTermConstructorCallExt,
}

impl Parse for ProcTermConstructorCall<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse type name
        let Some(type_name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Parse optional type arguments before "::"
        let mut type_args = Vec::new();
        loop {
            if let Some(colon2) = TokenColon2::parse(tokens, &mut k)? {
                // Found the separator; stop parsing type arguments
                // Parse method name: allow either keyword (e.g., #new_with_size) or plain variable (new_with_size)
                if k >= tokens.len() {
                    return Err(ParseError::Unknown("expected method name after '::'"));
                }

                let method: TokenVariable = if let Some(var) = TokenVariable::parse(tokens, &mut k)?
                {
                    var
                } else if let Token::Keyword(keyword) = &tokens[k] {
                    // Convert keyword into a variable token using the same position and string
                    let v = TokenVariable::new(keyword.pos().clone(), keyword.s().to_string());
                    k += 1;
                    v
                } else {
                    return Err(ParseError::Unknown("expected method name after '::'"));
                };

                // Parse arguments (simple terms only to avoid infinite recursion)
                let mut args = Vec::new();
                // For now, we'll only support simple arguments like numbers and variables
                while k < tokens.len() {
                    if let Some(arg) = ProcTermNumber::parse(tokens, &mut k)? {
                        args.push(ProcTerm::Number(arg));
                    } else if let Some(arg) = ProcTermVariable::parse(tokens, &mut k)? {
                        args.push(ProcTerm::Variable(arg));
                    } else {
                        break;
                    }
                }

                let proc_term_constructor_call = ProcTermConstructorCall {
                    type_name,
                    type_args,
                    colon2,
                    method,
                    args,
                    ext: (),
                };

                *i = k;
                return Ok(Some(proc_term_constructor_call));
            }

            // If we didn't see "::" yet, try to parse another type argument
            if let Some(arg) = TokenVariable::parse(tokens, &mut k)? {
                type_args.push(arg);
                continue;
            }

            // No "::" and no more variables -> not a constructor call
            return Ok(None);
        }

        // Unreachable: loop always returns
    }
}
