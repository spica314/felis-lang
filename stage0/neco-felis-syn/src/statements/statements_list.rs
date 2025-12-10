use crate::{
    Parse, ParseError, Phase, PhaseParse, Statement, StatementsThen,
    token::{Token, TokenSemicolon},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statements<P: Phase> {
    Statement(Box<Statement<P>>),
    Then(StatementsThen<P>),
    Nil,
}

impl Parse for Statements<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        // Parse a statement if present
        if let Some(head) = Statement::parse(tokens, i)? {
            // If a semicolon follows, parse the tail recursively as a Then-chain
            if let Some(semicolon) = TokenSemicolon::parse(tokens, i)? {
                let Some(tail) = Statements::parse(tokens, i)? else {
                    return Err(ParseError::Unknown("expected: Statements after ;"));
                };
                let statements_then = StatementsThen {
                    head: Box::new(head),
                    semicolon,
                    tail: Box::new(tail),
                    ext: (),
                };
                return Ok(Some(Statements::Then(statements_then)));
            }

            // No semicolon -> single statement
            return Ok(Some(Statements::Statement(Box::new(head))));
        }

        // No statement at all
        Ok(Some(Statements::Nil))
    }
}
