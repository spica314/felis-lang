use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{Token, TokenBraceL, TokenBraceR, TokenColon, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemDefinition<P: Phase> {
    pub keyword_definition: TokenKeyword,
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub type_: Box<Term<P>>,
    pub brace_l: TokenBraceL,
    pub body: Box<Term<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemDefinitionExt,
}

impl<P: Phase> ItemDefinition<P> {
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

impl Parse for ItemDefinition<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse #definition keyword
        let Some(keyword_definition) = TokenKeyword::parse_keyword(tokens, &mut k, "definition")?
        else {
            return Ok(None);
        };

        // Parse name
        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected name after #definition"));
        };

        // Parse colon
        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected : after definition name"));
        };

        // Parse type
        let Some(type_) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected type after :"));
        };

        // Parse opening brace
        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after definition type"));
        };

        // Parse body term
        let Some(body) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected definition body"));
        };

        // Parse closing brace
        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } to close definition body"));
        };

        let definition = ItemDefinition {
            keyword_definition,
            name,
            colon,
            type_: Box::new(type_),
            brace_l,
            body: Box::new(body),
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(definition))
    }
}
