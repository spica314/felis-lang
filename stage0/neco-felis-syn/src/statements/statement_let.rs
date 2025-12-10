use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm,
    token::{Token, TokenKeyword, TokenOperator, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLet<P: Phase> {
    pub let_keyword: TokenKeyword,
    pub variable: TokenVariable,
    pub equals: TokenOperator,
    pub value: Box<ProcTerm<P>>,
    pub ext: P::StatementLetExt,
}

impl Parse for StatementLet<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse "let" keyword
        let Some(let_keyword) = TokenKeyword::parse_keyword(tokens, &mut k, "let")? else {
            return Ok(None);
        };

        // Parse variable name
        let Some(variable) = TokenVariable::parse(tokens, &mut k)? else {
            // Not a plain let; allow other statement forms (e.g., let mut) to match
            return Ok(None);
        };

        // Parse "=" operator
        let Some(equals) = TokenOperator::parse_operator(tokens, &mut k, "=")? else {
            return Ok(None);
        };

        // Parse value expression
        let Some(value) = ProcTerm::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected: value expression after '='"));
        };

        let statement_let = StatementLet {
            let_keyword,
            variable,
            equals,
            value: Box::new(value),
            ext: (),
        };

        *i = k;
        Ok(Some(statement_let))
    }
}

impl<P: Phase> StatementLet<P> {
    pub fn variable_name(&self) -> &str {
        self.variable.s()
    }
}
