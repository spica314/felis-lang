use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm,
    token::{Token, TokenKeyword, TokenSemicolon},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementReturn<P: Phase> {
    pub keyword_return: TokenKeyword,
    pub value: Box<ProcTerm<P>>,
    pub semicolon: TokenSemicolon,
    pub ext: P::StatementReturnExt,
}

impl Parse for StatementReturn<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse #return keyword
        let Some(keyword_return) = TokenKeyword::parse_keyword(tokens, &mut k, "return")? else {
            return Ok(None);
        };

        // Parse return value expression
        let Some(value) = ProcTerm::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected expression after #return"));
        };

        // Parse semicolon
        let Some(semicolon) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected ; after return expression"));
        };

        let statement_return = StatementReturn {
            keyword_return,
            value: Box::new(value),
            semicolon,
            ext: (),
        };

        *i = k;
        Ok(Some(statement_return))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FileIdGenerator, Token};

    #[test]
    fn test_parse_return() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let source = "#return x;";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let result = StatementReturn::parse(&tokens, &mut i);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_some());
    }

    #[test]
    fn test_parse_return_complex() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let source = "#return add x y;";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let result = StatementReturn::parse(&tokens, &mut i);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_some());
    }
}
