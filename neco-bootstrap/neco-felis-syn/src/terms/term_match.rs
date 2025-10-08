use crate::{
    Parse, ParseError, Phase, PhaseParse, TermMatchBranch,
    token::{Token, TokenBraceL, TokenBraceR, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermMatch<P: Phase> {
    pub keyword_match: TokenKeyword,
    pub scrutinee: TokenVariable,
    pub brace_l: TokenBraceL,
    pub branches: Vec<TermMatchBranch<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::TermMatchExt,
}

impl<P: Phase> TermMatch<P> {
    /// Get the scrutinee (the expression being matched)
    pub fn scrutinee(&self) -> &TokenVariable {
        &self.scrutinee
    }

    /// Get the match branches
    pub fn branches(&self) -> &[TermMatchBranch<P>] {
        &self.branches
    }
}

impl Parse for TermMatch<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse #match keyword
        let Some(keyword_match) = TokenKeyword::parse_keyword(tokens, &mut k, "match")? else {
            return Ok(None);
        };

        // Parse scrutinee (the expression being matched)
        let Some(scrutinee) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected variable after #match"));
        };

        // Parse opening brace
        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after match scrutinee"));
        };

        // Parse branches
        let mut branches = vec![];
        while let Some(branch) = TermMatchBranch::parse(tokens, &mut k)? {
            branches.push(branch);
        }

        // Parse closing brace
        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } to close match expression"));
        };

        let term_match = TermMatch {
            keyword_match,
            scrutinee,
            brace_l,
            branches,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(term_match))
    }
}
