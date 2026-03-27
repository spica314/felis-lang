mod lexer;
mod parser;
mod syntax;

use std::fmt;

pub use lexer::{Span, Token, TokenKind};
pub use parser::Parser;
pub use syntax::{JsonEntry, JsonValue};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub span: Option<Span>,
    pub message: String,
}

impl Error {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self {
            span: None,
            message: message.into(),
        }
    }

    pub(crate) fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.span {
            Some(span) => write!(f, "{}..{}: {}", span.start, span.end, self.message),
            None => write!(f, "{}", self.message),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

pub fn parse(source: &str) -> Result<(Vec<Token>, JsonValue)> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.lex()?;
    let mut parser = Parser::new(tokens.clone());
    let value = parser.parse_value()?;
    parser.expect_end()?;
    Ok((tokens, value))
}
