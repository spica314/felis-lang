use crate::ast::JsonValue;
use crate::error::ParseError;
use crate::lexer::{Lexer, Token, TokenKind};

pub fn parse(input: &str) -> Result<JsonValue, ParseError> {
    let tokens = Lexer::lex(input)?;
    let mut parser = Parser::new(tokens);
    let value = parser.parse_value()?;
    parser.expect_eof()?;
    Ok(value)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_value(&mut self) -> Result<JsonValue, ParseError> {
        let token = self.peek()?;
        match &token.kind {
            TokenKind::Null => {
                self.bump();
                Ok(JsonValue::Null)
            }
            TokenKind::True => {
                self.bump();
                Ok(JsonValue::Bool(true))
            }
            TokenKind::False => {
                self.bump();
                Ok(JsonValue::Bool(false))
            }
            TokenKind::Number(value) => {
                let value = value.clone();
                self.bump();
                Ok(JsonValue::Number(value))
            }
            TokenKind::String(value) => {
                let value = value.clone();
                self.bump();
                Ok(JsonValue::String(value))
            }
            TokenKind::LBracket => self.parse_array(),
            TokenKind::LBrace => self.parse_object(),
            TokenKind::Eof => Err(self.error_here("unexpected end of input")),
            _ => Err(self.error_here("unexpected token")),
        }
    }

    fn parse_array(&mut self) -> Result<JsonValue, ParseError> {
        self.expect(TokenKind::LBracket)?;
        let mut values = Vec::new();

        if self.check(TokenKind::RBracket) {
            self.bump();
            return Ok(JsonValue::Array(values));
        }

        loop {
            values.push(self.parse_value()?);
            if self.check(TokenKind::Comma) {
                self.bump();
                continue;
            }
            if self.check(TokenKind::RBracket) {
                self.bump();
                break;
            }
            return Err(self.error_here("expected ',' or ']' in array"));
        }

        Ok(JsonValue::Array(values))
    }

    fn parse_object(&mut self) -> Result<JsonValue, ParseError> {
        self.expect(TokenKind::LBrace)?;
        let mut members = Vec::new();

        if self.check(TokenKind::RBrace) {
            self.bump();
            return Ok(JsonValue::Object(members));
        }

        loop {
            let key = match &self.peek()?.kind {
                TokenKind::String(value) => value.clone(),
                _ => return Err(self.error_here("expected string key")),
            };
            self.bump();
            self.expect(TokenKind::Colon)?;
            let value = self.parse_value()?;
            members.push((key, value));

            if self.check(TokenKind::Comma) {
                self.bump();
                continue;
            }
            if self.check(TokenKind::RBrace) {
                self.bump();
                break;
            }
            return Err(self.error_here("expected ',' or '}' in object"));
        }

        Ok(JsonValue::Object(members))
    }

    fn expect_eof(&mut self) -> Result<(), ParseError> {
        if self.check(TokenKind::Eof) {
            Ok(())
        } else {
            Err(self.error_here("unexpected trailing tokens"))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if self.check(kind) {
            self.bump();
            Ok(())
        } else {
            Err(self.error_here("unexpected token"))
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.tokens
            .get(self.index)
            .map(|token| token.kind == kind)
            .unwrap_or(false)
    }

    fn peek(&self) -> Result<&Token, ParseError> {
        self.tokens
            .get(self.index)
            .ok_or_else(|| ParseError::new("unexpected end of input", 0, 0))
    }

    fn bump(&mut self) {
        if self.index < self.tokens.len() {
            self.index += 1;
        }
    }

    fn error_here(&self, message: &str) -> ParseError {
        if let Some(token) = self.tokens.get(self.index) {
            ParseError::new(message, token.line, token.column)
        } else {
            ParseError::new(message, 0, 0)
        }
    }
}
