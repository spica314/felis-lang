use crate::{
    Parse, ParseError, Phase, PhaseParse, Statements,
    token::{Token, TokenBraceL, TokenBraceR, TokenKeyword},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermIf<P: Phase> {
    pub keyword_if: TokenKeyword,
    pub condition: Box<Statements<P>>,
    pub brace_l: TokenBraceL,
    pub then_body: Box<Statements<P>>,
    pub brace_r: TokenBraceR,
    pub else_clause: Option<ProcTermIfElse<P>>,
    pub ext: P::ProcTermIfExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermIfElse<P: Phase> {
    pub keyword_else: TokenKeyword,
    pub brace_l: TokenBraceL,
    pub else_body: Box<Statements<P>>,
    pub brace_r: TokenBraceR,
}

impl Parse for ProcTermIf<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse #if keyword
        let Some(keyword_if) = TokenKeyword::parse_keyword(tokens, &mut k, "if")? else {
            return Ok(None);
        };

        // Parse condition
        let Some(condition) = Statements::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown(
                "expected condition expression after #if",
            ));
        };

        // Parse opening brace
        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after if condition"));
        };

        // Parse then body
        let Some(then_body) = Statements::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected expression in if body"));
        };

        // Parse closing brace
        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } to close if body"));
        };

        // Parse optional else clause
        let else_clause =
            if let Some(keyword_else) = TokenKeyword::parse_keyword(tokens, &mut k, "else")? {
                // Parse else opening brace
                let Some(else_brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
                    return Err(ParseError::Unknown("expected { after #else"));
                };

                // Parse else body
                let Some(else_body) = Statements::parse(tokens, &mut k)? else {
                    return Err(ParseError::Unknown("expected expression in else body"));
                };

                // Parse else closing brace
                let Some(else_brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
                    return Err(ParseError::Unknown("expected } to close else body"));
                };

                Some(ProcTermIfElse {
                    keyword_else,
                    brace_l: else_brace_l,
                    else_body: Box::new(else_body),
                    brace_r: else_brace_r,
                })
            } else {
                None
            };

        let proc_term_if = ProcTermIf {
            keyword_if,
            condition: Box::new(condition),
            brace_l,
            then_body: Box::new(then_body),
            brace_r,
            else_clause,
            ext: (),
        };

        *i = k;
        Ok(Some(proc_term_if))
    }
}
