use crate::{
    Parse, ParseError, Phase, PhaseParse, Statements,
    token::{Token, TokenBraceL, TokenBraceR, TokenKeyword},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLoop<P: Phase> {
    pub keyword_loop: TokenKeyword,
    pub brace_l: TokenBraceL,
    pub body: Box<Statements<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::StatementLoopExt,
}

impl<P: Phase> StatementLoop<P> {
    /// Get the loop body
    pub fn body(&self) -> &Statements<P> {
        &self.body
    }
}

impl Parse for StatementLoop<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse #loop keyword
        let Some(keyword_loop) = TokenKeyword::parse_keyword(tokens, &mut k, "loop")? else {
            return Ok(None);
        };

        // Parse opening brace
        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected { after #loop"));
        };

        // Parse loop body
        let Some(body) = Statements::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected statements in loop body"));
        };

        // Parse closing brace
        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected } to close loop body"));
        };

        let statement_loop = StatementLoop {
            keyword_loop,
            brace_l,
            body: Box::new(body),
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(statement_loop))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FileIdGenerator, Token};

    #[test]
    fn test_parse_loop() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let source = "#loop { error_code_ref <- 42u64; }";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let result = StatementLoop::parse(&tokens, &mut i);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_some());
    }
}
