use crate::{
    Parse, ParseError, Phase, PhaseParse, Statements,
    token::{
        Token, TokenBraceL, TokenBraceR, TokenColon, TokenColon2, TokenComma, TokenKeyword,
        TokenOperator, TokenVariable,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermMatch<P: Phase> {
    pub keyword_match: TokenKeyword,
    pub scrutinee: TokenVariable,
    pub brace_l: TokenBraceL,
    pub branches: Vec<ProcTermMatchBranch<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ProcTermMatchExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermMatchBranch<P: Phase> {
    pub pattern: ProcTermMatchPattern<P>,
    pub arrow: TokenOperator,
    pub body: Box<Statements<P>>,
    pub comma: Option<TokenComma>,
    pub ext: P::ProcTermMatchBranchExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermMatchPattern<P: Phase> {
    pub type_name: TokenVariable,
    pub colon2: TokenColon2,
    pub constructor: TokenVariable,
    pub brace_l: TokenBraceL,
    pub fields: Vec<ProcTermMatchField<P>>,
    pub brace_r: TokenBraceR,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermMatchField<P: Phase> {
    pub field_name: TokenVariable,
    pub colon: TokenColon,
    pub binder: TokenVariable,
    pub comma: Option<TokenComma>,
    pub ext: P::ProcTermMatchFieldExt,
}

impl<P: Phase> ProcTermMatch<P> {
    pub fn branches(&self) -> &[ProcTermMatchBranch<P>] {
        &self.branches
    }
}

impl Parse for ProcTermMatch<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(keyword_match) = TokenKeyword::parse_keyword(tokens, &mut k, "match")? else {
            return Ok(None);
        };

        let Some(scrutinee) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected variable after #match"));
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after #match variable"));
        };

        let mut branches = Vec::new();
        while let Some(branch) = ProcTermMatchBranch::parse(tokens, &mut k)? {
            branches.push(branch);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } to close #match"));
        };

        let proc_term_match = ProcTermMatch {
            keyword_match,
            scrutinee,
            brace_l,
            branches,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(proc_term_match))
    }
}

impl Parse for ProcTermMatchBranch<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(pattern) = ProcTermMatchPattern::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(arrow) = TokenOperator::parse_operator(tokens, &mut k, "=>")? else {
            return Err(ParseError::Unknown("expected => after match pattern"));
        };

        let Some(_brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after match =>"));
        };

        let Some(body) = Statements::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected statements in match branch"));
        };

        let Some(_brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } after match branch body"));
        };

        let comma = TokenComma::parse(tokens, &mut k)?;

        let branch = ProcTermMatchBranch {
            pattern,
            arrow,
            body: Box::new(body),
            comma,
            ext: (),
        };

        *i = k;
        Ok(Some(branch))
    }
}

impl ProcTermMatchPattern<PhaseParse> {
    pub fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(type_name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(colon2) = TokenColon2::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown(
                "expected :: after type name in match pattern",
            ));
        };

        let Some(constructor) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected constructor after ::"));
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after constructor name"));
        };

        let mut fields = Vec::new();
        while let Some(field) = ProcTermMatchField::parse(tokens, &mut k)? {
            fields.push(field);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } after pattern fields"));
        };

        *i = k;
        Ok(Some(Self {
            type_name,
            colon2,
            constructor,
            brace_l,
            fields,
            brace_r,
        }))
    }
}

impl ProcTermMatchField<PhaseParse> {
    pub fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(field_name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown(
                "expected : after field name in match pattern",
            ));
        };

        let Some(binder) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown(
                "expected binder variable in match pattern",
            ));
        };

        let comma = TokenComma::parse(tokens, &mut k)?;

        let field = ProcTermMatchField {
            field_name,
            colon,
            binder,
            comma,
            ext: (),
        };

        *i = k;
        Ok(Some(field))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FileIdGenerator, Token};

    #[test]
    fn parse_simple_proc_match() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#match v {
            Vec3::Vec3 { x: vx, } => { #let _ = 0; },
        }"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ProcTermMatch::parse(&tokens, &mut i).unwrap();
        assert!(result.is_some());
        let match_term = result.unwrap();
        assert_eq!(match_term.branches.len(), 1);
        let branch = &match_term.branches[0];
        assert_eq!(branch.pattern.constructor.s(), "Vec3");
        assert_eq!(branch.pattern.fields.len(), 1);
        assert_eq!(branch.pattern.fields[0].binder.s(), "vx");
    }
}
