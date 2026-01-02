mod ast;
mod error;
mod lexer;
mod parser;

pub use ast::JsonValue;
pub use error::ParseError;

pub fn parse_json(input: &str) -> Result<JsonValue, ParseError> {
    parser::parse(input)
}
