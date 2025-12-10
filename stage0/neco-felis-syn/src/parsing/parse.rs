use crate::token::Token;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParseError {
    Unknown(&'static str),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Unknown(msg) => write!(f, "Parse error: {msg}"),
        }
    }
}

impl std::error::Error for ParseError {}

pub trait Parse: Sized {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError>;
}
