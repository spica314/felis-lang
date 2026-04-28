use crate::{
    Parse, Result, SourceFile, Token,
    lexer::{self, TokenEndOfFile},
};

pub fn parse_source(source: &str) -> Result<(Vec<Token>, Option<SourceFile>)> {
    let tokens = lexer::lex(source)?;

    let mut i = 0;
    let syntax = SourceFile::parse(&tokens, &mut i)?;
    let _ = TokenEndOfFile::parse(&tokens, &mut i)?;
    if i != tokens.len() {
        return Err(crate::Error::Message(
            "unexpected token after source file".to_string(),
        ));
    }

    Ok((tokens, syntax))
}
