use crate::{Error, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Comma,
    String(String),
    Number(String),
    True,
    False,
    Null,
    EndOfFile,
}

pub(crate) struct Lexer {
    source: Vec<char>,
    offset: usize,
}

impl Lexer {
    pub(crate) fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            offset: 0,
        }
    }

    pub(crate) fn lex(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        while self.skip_trivia() {
            if self.is_eof() {
                break;
            }
            tokens.push(self.next_token()?);
        }
        tokens.push(Token {
            kind: TokenKind::EndOfFile,
            span: Span {
                start: self.offset,
                end: self.offset,
            },
            lexeme: String::new(),
        });
        Ok(tokens)
    }

    fn skip_trivia(&mut self) -> bool {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.bump_char();
            } else {
                break;
            }
        }
        true
    }

    fn next_token(&mut self) -> Result<Token> {
        let start = self.offset;
        let ch = self
            .bump_char()
            .ok_or_else(|| Error::new("unexpected end of input"))?;
        let token = match ch {
            '{' => simple_token(TokenKind::LeftBrace, start, self.offset, "{"),
            '}' => simple_token(TokenKind::RightBrace, start, self.offset, "}"),
            '[' => simple_token(TokenKind::LeftBracket, start, self.offset, "["),
            ']' => simple_token(TokenKind::RightBracket, start, self.offset, "]"),
            ':' => simple_token(TokenKind::Colon, start, self.offset, ":"),
            ',' => simple_token(TokenKind::Comma, start, self.offset, ","),
            '"' => return self.string_literal(start),
            't' => return self.keyword(start, 't', "rue", TokenKind::True),
            'f' => return self.keyword(start, 'f', "alse", TokenKind::False),
            'n' => return self.keyword(start, 'n', "ull", TokenKind::Null),
            '-' | '0'..='9' => return self.number_literal(start, ch),
            _ => {
                return Err(
                    Error::new(format!("unexpected character `{ch}`")).with_span(Span {
                        start,
                        end: self.offset,
                    }),
                );
            }
        };
        Ok(token)
    }

    fn string_literal(&mut self, start: usize) -> Result<Token> {
        let mut value = String::new();
        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => {
                    return Ok(Token {
                        kind: TokenKind::String(value),
                        span: Span {
                            start,
                            end: self.offset,
                        },
                        lexeme: self.slice(start, self.offset),
                    });
                }
                '\\' => {
                    let escaped = self.bump_char().ok_or_else(|| {
                        Error::new("unterminated string literal").with_span(Span {
                            start,
                            end: self.offset,
                        })
                    })?;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        '/' => value.push('/'),
                        'b' => value.push('\u{0008}'),
                        'f' => value.push('\u{000c}'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        'u' => {
                            let escaped = self.unicode_escape(start)?;
                            value.push(escaped);
                        }
                        other => {
                            return Err(Error::new(format!(
                                "unsupported escape sequence `\\{other}`"
                            ))
                            .with_span(Span {
                                start,
                                end: self.offset,
                            }));
                        }
                    }
                }
                other => value.push(other),
            }
        }
        Err(Error::new("unterminated string literal").with_span(Span {
            start,
            end: self.offset,
        }))
    }

    fn unicode_escape(&mut self, start: usize) -> Result<char> {
        let mut digits = String::new();
        for _ in 0..4 {
            let ch = self.bump_char().ok_or_else(|| {
                Error::new("unterminated unicode escape").with_span(Span {
                    start,
                    end: self.offset,
                })
            })?;
            if !ch.is_ascii_hexdigit() {
                return Err(
                    Error::new("unicode escape must contain four hex digits").with_span(Span {
                        start,
                        end: self.offset,
                    }),
                );
            }
            digits.push(ch);
        }
        // Four ASCII hex digits are validated above, so this conversion is infallible.
        let value = u32::from_str_radix(&digits, 16).expect("validated unicode escape");
        char::from_u32(value).ok_or_else(|| {
            Error::new("unicode escape is not a valid scalar value").with_span(Span {
                start,
                end: self.offset,
            })
        })
    }

    fn keyword(&mut self, start: usize, first: char, tail: &str, kind: TokenKind) -> Result<Token> {
        for expected in tail.chars() {
            let found = self.bump_char().ok_or_else(|| {
                Error::new(format!("unexpected end of input after `{first}`")).with_span(Span {
                    start,
                    end: self.offset,
                })
            })?;
            if found != expected {
                return Err(Error::new(format!(
                    "unexpected token `{}`",
                    self.slice(start, self.offset)
                ))
                .with_span(Span {
                    start,
                    end: self.offset,
                }));
            }
        }
        Ok(Token {
            kind,
            span: Span {
                start,
                end: self.offset,
            },
            lexeme: self.slice(start, self.offset),
        })
    }

    fn number_literal(&mut self, start: usize, first: char) -> Result<Token> {
        if first == '-' {
            let next = self.peek_char().ok_or_else(|| {
                Error::new("expected digit after `-`").with_span(Span {
                    start,
                    end: self.offset,
                })
            })?;
            if !next.is_ascii_digit() {
                return Err(Error::new("expected digit after `-`").with_span(Span {
                    start,
                    end: self.offset,
                }));
            }
        }

        self.consume_digits();

        if self.peek_char() == Some('.') {
            self.bump_char();
            if !self.peek_char().is_some_and(|ch| ch.is_ascii_digit()) {
                return Err(
                    Error::new("expected digit after decimal point").with_span(Span {
                        start,
                        end: self.offset,
                    }),
                );
            }
            self.consume_digits();
        }

        if matches!(self.peek_char(), Some('e' | 'E')) {
            self.bump_char();
            if matches!(self.peek_char(), Some('+' | '-')) {
                self.bump_char();
            }
            if !self.peek_char().is_some_and(|ch| ch.is_ascii_digit()) {
                return Err(Error::new("expected digit in exponent").with_span(Span {
                    start,
                    end: self.offset,
                }));
            }
            self.consume_digits();
        }

        Ok(Token {
            kind: TokenKind::Number(self.slice(start, self.offset)),
            span: Span {
                start,
                end: self.offset,
            },
            lexeme: self.slice(start, self.offset),
        })
    }

    fn consume_digits(&mut self) {
        while self.peek_char().is_some_and(|ch| ch.is_ascii_digit()) {
            self.bump_char();
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.source.get(self.offset).copied()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += 1;
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.source.len()
    }

    fn slice(&self, start: usize, end: usize) -> String {
        self.source[start..end].iter().collect()
    }
}

fn simple_token(kind: TokenKind, start: usize, end: usize, lexeme: &str) -> Token {
    Token {
        kind,
        span: Span { start, end },
        lexeme: lexeme.to_string(),
    }
}
