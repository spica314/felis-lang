use crate::{Error, JsonEntry, JsonValue, Result, Span, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn parse_value(&mut self) -> Result<JsonValue> {
        match self.peek_kind() {
            TokenKind::LeftBrace => self.parse_object(),
            TokenKind::LeftBracket => self.parse_array(),
            TokenKind::String(value) => {
                self.cursor += 1;
                Ok(JsonValue::String(value))
            }
            TokenKind::Number(value) => {
                self.cursor += 1;
                Ok(JsonValue::Number(value))
            }
            TokenKind::True => {
                self.cursor += 1;
                Ok(JsonValue::Boolean(true))
            }
            TokenKind::False => {
                self.cursor += 1;
                Ok(JsonValue::Boolean(false))
            }
            TokenKind::Null => {
                self.cursor += 1;
                Ok(JsonValue::Null)
            }
            _ => Err(self.error_here("expected JSON value")),
        }
    }

    pub fn expect_end(&mut self) -> Result<()> {
        if matches!(self.peek_kind(), TokenKind::EndOfFile) {
            Ok(())
        } else {
            Err(self.error_here("unexpected trailing tokens"))
        }
    }

    fn parse_object(&mut self) -> Result<JsonValue> {
        self.expect_punctuation(TokenKind::LeftBrace)?;
        let mut entries = Vec::new();

        if self.consume_punctuation(TokenKind::RightBrace) {
            return Ok(JsonValue::Object(entries));
        }

        loop {
            let key = match self.peek_kind() {
                TokenKind::String(value) => {
                    self.cursor += 1;
                    value
                }
                _ => return Err(self.error_here("expected object key string")),
            };
            self.expect_punctuation(TokenKind::Colon)?;
            let value = self.parse_value()?;
            entries.push(JsonEntry { key, value });

            if self.consume_punctuation(TokenKind::Comma) {
                continue;
            }
            self.expect_punctuation(TokenKind::RightBrace)?;
            break;
        }

        Ok(JsonValue::Object(entries))
    }

    fn parse_array(&mut self) -> Result<JsonValue> {
        self.expect_punctuation(TokenKind::LeftBracket)?;
        let mut values = Vec::new();

        if self.consume_punctuation(TokenKind::RightBracket) {
            return Ok(JsonValue::Array(values));
        }

        loop {
            values.push(self.parse_value()?);
            if self.consume_punctuation(TokenKind::Comma) {
                continue;
            }
            self.expect_punctuation(TokenKind::RightBracket)?;
            break;
        }

        Ok(JsonValue::Array(values))
    }

    fn peek_kind(&self) -> TokenKind {
        self.tokens
            .get(self.cursor)
            .map(|token| token.kind.clone())
            .unwrap_or(TokenKind::EndOfFile)
    }

    fn consume_punctuation(&mut self, expected: TokenKind) -> bool {
        if self.peek_kind() == expected {
            self.cursor += 1;
            true
        } else {
            false
        }
    }

    fn expect_punctuation(&mut self, expected: TokenKind) -> Result<()> {
        if self.consume_punctuation(expected.clone()) {
            Ok(())
        } else {
            Err(self.error_here(format!("expected `{}`", punctuation_name(&expected))))
        }
    }

    fn error_here(&self, message: impl Into<String>) -> Error {
        let span = self
            .tokens
            .get(self.cursor)
            .map(|token| token.span)
            .unwrap_or(Span { start: 0, end: 0 });
        Error::new(message).with_span(span)
    }
}

fn punctuation_name(token: &TokenKind) -> &'static str {
    match token {
        TokenKind::LeftBrace => "{",
        TokenKind::RightBrace => "}",
        TokenKind::LeftBracket => "[",
        TokenKind::RightBracket => "]",
        TokenKind::Colon => ":",
        TokenKind::Comma => ",",
        _ => "?",
    }
}
