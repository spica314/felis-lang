use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{Token, TokenBraceL, TokenBraceR, TokenColon, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemTheorem<P: Phase> {
    pub keyword_theorem: TokenKeyword,
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub type_: Box<Term<P>>,
    pub brace_l: TokenBraceL,
    pub body: Box<Term<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemTheoremExt,
}

impl<P: Phase> ItemTheorem<P> {
    pub fn name(&self) -> &TokenVariable {
        &self.name
    }

    pub fn type_(&self) -> &Term<P> {
        &self.type_
    }

    pub fn body(&self) -> &Term<P> {
        &self.body
    }
}

impl Parse for ItemTheorem<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse #theorem keyword
        let Some(keyword_theorem) = TokenKeyword::parse_keyword(tokens, &mut k, "theorem")? else {
            return Ok(None);
        };

        // Parse name
        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected name after #theorem"));
        };

        // Parse colon
        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected : after theorem name"));
        };

        // Parse type
        let Some(type_) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected type after :"));
        };

        // Parse opening brace
        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after theorem type"));
        };

        // Parse body term
        let Some(body) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected theorem body"));
        };

        // Parse closing brace
        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } after theorem body"));
        };

        let theorem = ItemTheorem {
            keyword_theorem,
            name,
            colon,
            type_: Box::new(type_),
            brace_l,
            body: Box::new(body),
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(theorem))
    }
}
