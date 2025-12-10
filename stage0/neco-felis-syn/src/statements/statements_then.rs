use crate::{
    Parse, ParseError, Phase, PhaseParse, Statement, Statements,
    token::{Token, TokenSemicolon},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementsThen<P: Phase> {
    pub head: Box<Statement<P>>,
    pub semicolon: TokenSemicolon,
    pub tail: Box<Statements<P>>,
    pub ext: P::StatementsThenExt,
}

impl Parse for StatementsThen<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(head) = Statement::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(semicolon) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(tail) = Statements::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected: Statements after ;"));
        };

        let statements_then = StatementsThen {
            head: Box::new(head),
            semicolon,
            tail: Box::new(tail),
            ext: (),
        };
        *i = k;
        Ok(Some(statements_then))
    }
}
