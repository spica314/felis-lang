use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{Token, TokenKeyword, TokenSemicolon},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementBreak<P: Phase> {
    pub keyword_break: TokenKeyword,
    pub semicolon: TokenSemicolon,
    pub ext: P::StatementBreakExt,
}

impl Parse for StatementBreak<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse #break keyword
        let Some(keyword_break) = TokenKeyword::parse_keyword(tokens, &mut k, "break")? else {
            return Ok(None);
        };

        // Parse semicolon
        let Some(semicolon) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected ; after #break"));
        };

        let statement_break = StatementBreak {
            keyword_break,
            semicolon,
            ext: (),
        };

        *i = k;
        Ok(Some(statement_break))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FileIdGenerator, Token};

    #[test]
    fn test_parse_break() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let source = "#break;";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let result = StatementBreak::parse(&tokens, &mut i);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_some());
    }
}
