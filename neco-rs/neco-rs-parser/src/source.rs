use crate::{Parse, Parser, Result, SourceFile, Token, lexer};

pub fn parse_source(source: &str) -> Result<(Vec<Token>, Option<SourceFile>)> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.lex()?;
    let mut parser = Parser::new(tokens.clone());
    let syntax = SourceFile::parse(&mut parser)?;
    Ok((tokens, syntax))
}
