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
    Keyword(Keyword),
    Identifier(String),
    StringLiteral(String),
    CharLiteral(char),
    IntegerLiteral(String),
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Semicolon,
    Comma,
    Equals,
    Ampersand,
    AmpersandCaret,
    At,
    AtCaret,
    Underscore,
    DoubleColon,
    Arrow,
    FatArrow,
    LeftArrow,
    DotArrow,
    EndOfFile,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    As,
    BindBuiltin,
    Break,
    Continue,
    EntryPoint,
    Else,
    Fn,
    Proc,
    Pub,
    Type,
    Theorem,
    Prop,
    If,
    Match,
    Let,
    Loop,
    Use,
    Mod,
    With,
    Forall,
    Package,
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
        while self.skip_trivia()? {
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

    fn skip_trivia(&mut self) -> Result<bool> {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.bump_char();
                continue;
            }
            if self.starts_with("//") {
                while let Some(ch) = self.bump_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }
            if self.starts_with("/*") {
                let start = self.offset;
                self.offset += 2;
                while !self.starts_with("*/") {
                    if self.bump_char().is_none() {
                        break;
                    }
                }
                if self.starts_with("*/") {
                    self.offset += 2;
                    continue;
                }
                return Err(Error::new("unterminated block comment").with_span(Span {
                    start,
                    end: self.source.len(),
                }));
            }
            break;
        }
        Ok(true)
    }

    fn next_token(&mut self) -> Result<Token> {
        let start = self.offset;
        if self.starts_with("::") {
            self.offset += 2;
            return Ok(simple_token(
                TokenKind::DoubleColon,
                start,
                self.offset,
                "::",
            ));
        }
        if self.starts_with("->") {
            self.offset += 2;
            return Ok(simple_token(TokenKind::Arrow, start, self.offset, "->"));
        }
        if self.starts_with("=>") {
            self.offset += 2;
            return Ok(simple_token(TokenKind::FatArrow, start, self.offset, "=>"));
        }
        if self.starts_with("<-") {
            self.offset += 2;
            return Ok(simple_token(TokenKind::LeftArrow, start, self.offset, "<-"));
        }
        if self.starts_with(".>") {
            self.offset += 2;
            return Ok(simple_token(TokenKind::DotArrow, start, self.offset, ".>"));
        }
        if self.starts_with("@^") {
            self.offset += 2;
            return Ok(simple_token(TokenKind::AtCaret, start, self.offset, "@^"));
        }
        if self.starts_with("&^") {
            self.offset += 2;
            return Ok(simple_token(
                TokenKind::AmpersandCaret,
                start,
                self.offset,
                "&^",
            ));
        }

        let ch = self
            .bump_char()
            .ok_or_else(|| Error::new("unexpected end of input"))?;
        let token = match ch {
            '(' => simple_token(TokenKind::LeftParen, start, self.offset, "("),
            ')' => simple_token(TokenKind::RightParen, start, self.offset, ")"),
            '{' => simple_token(TokenKind::LeftBrace, start, self.offset, "{"),
            '}' => simple_token(TokenKind::RightBrace, start, self.offset, "}"),
            '[' => simple_token(TokenKind::LeftBracket, start, self.offset, "["),
            ']' => simple_token(TokenKind::RightBracket, start, self.offset, "]"),
            ':' => simple_token(TokenKind::Colon, start, self.offset, ":"),
            ';' => simple_token(TokenKind::Semicolon, start, self.offset, ";"),
            ',' => simple_token(TokenKind::Comma, start, self.offset, ","),
            '=' => simple_token(TokenKind::Equals, start, self.offset, "="),
            '&' => simple_token(TokenKind::Ampersand, start, self.offset, "&"),
            '@' => simple_token(TokenKind::At, start, self.offset, "@"),
            '_' => simple_token(TokenKind::Underscore, start, self.offset, "_"),
            '"' => return self.string_literal(start),
            '\'' => return self.char_literal(start),
            '#' => return self.keyword(start),
            ch if ch.is_ascii_digit() => return Ok(self.integer_literal(start, ch)),
            ch if is_identifier_start(ch) => return Ok(self.identifier(start, ch)),
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
                        kind: TokenKind::StringLiteral(value.clone()),
                        span: Span {
                            start,
                            end: self.offset,
                        },
                        lexeme: self.source[start..self.offset].iter().collect(),
                    });
                }
                '\\' => {
                    let escaped = self.bump_char().ok_or_else(|| {
                        Error::new("unterminated string literal").with_span(Span {
                            start,
                            end: self.offset,
                        })
                    })?;
                    value.push(match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        't' => '\t',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }
        Err(Error::new("unterminated string literal").with_span(Span {
            start,
            end: self.offset,
        }))
    }

    fn char_literal(&mut self, start: usize) -> Result<Token> {
        let value = match self.bump_char() {
            Some('\'') => {
                return Err(Error::new("empty char literal").with_span(Span {
                    start,
                    end: self.offset,
                }));
            }
            Some('\\') => {
                let escaped = self.bump_char().ok_or_else(|| {
                    Error::new("unterminated char literal").with_span(Span {
                        start,
                        end: self.offset,
                    })
                })?;
                match escaped {
                    '\'' => '\'',
                    '"' => '"',
                    '\\' => '\\',
                    '0' => '\0',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    other => {
                        return Err(Error::new(format!(
                            "unsupported char escape `\\{other}`"
                        ))
                        .with_span(Span {
                            start,
                            end: self.offset,
                        }));
                    }
                }
            }
            Some(ch) => ch,
            None => {
                return Err(Error::new("unterminated char literal").with_span(Span {
                    start,
                    end: self.offset,
                }));
            }
        };

        if !value.is_ascii() {
            return Err(Error::new("char literal must be ASCII").with_span(Span {
                start,
                end: self.offset,
            }));
        }

        if !matches!(self.bump_char(), Some('\'')) {
            return Err(Error::new("unterminated char literal").with_span(Span {
                start,
                end: self.offset,
            }));
        }

        Ok(Token {
            kind: TokenKind::CharLiteral(value),
            span: Span {
                start,
                end: self.offset,
            },
            lexeme: self.source[start..self.offset].iter().collect(),
        })
    }

    fn keyword(&mut self, start: usize) -> Result<Token> {
        let mut text = String::from("#");
        while let Some(ch) = self.peek_char() {
            if is_identifier_continue(ch) {
                text.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }
        let keyword = match text.as_str() {
            "#as" => Keyword::As,
            "#bind_builtin" => Keyword::BindBuiltin,
            "#break" => Keyword::Break,
            "#continue" => Keyword::Continue,
            "#entrypoint" => Keyword::EntryPoint,
            "#else" => Keyword::Else,
            "#fn" => Keyword::Fn,
            "#proc" => Keyword::Proc,
            "#pub" => Keyword::Pub,
            "#type" => Keyword::Type,
            "#theorem" => Keyword::Theorem,
            "#prop" => Keyword::Prop,
            "#if" => Keyword::If,
            "#match" => Keyword::Match,
            "#let" => Keyword::Let,
            "#loop" => Keyword::Loop,
            "#use" => Keyword::Use,
            "#mod" => Keyword::Mod,
            "#with" => Keyword::With,
            "#forall" => Keyword::Forall,
            "#package" => Keyword::Package,
            _ => {
                return Err(
                    Error::new(format!("unknown keyword `{text}`")).with_span(Span {
                        start,
                        end: self.offset,
                    }),
                );
            }
        };
        Ok(Token {
            kind: TokenKind::Keyword(keyword),
            span: Span {
                start,
                end: self.offset,
            },
            lexeme: text,
        })
    }

    fn integer_literal(&mut self, start: usize, first: char) -> Token {
        let mut text = String::new();
        text.push(first);

        if first == '0' && matches!(self.peek_char(), Some('x' | 'X')) {
            text.push(self.bump_char().expect("peeked hexadecimal prefix"));
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_hexdigit() {
                    text.push(ch);
                    self.bump_char();
                } else {
                    break;
                }
            }
        } else {
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() {
                    text.push(ch);
                    self.bump_char();
                } else {
                    break;
                }
            }
        }

        Token {
            kind: TokenKind::IntegerLiteral(text.clone()),
            span: Span {
                start,
                end: self.offset,
            },
            lexeme: text,
        }
    }

    fn identifier(&mut self, start: usize, first: char) -> Token {
        let mut text = String::new();
        text.push(first);
        while let Some(ch) = self.peek_char() {
            if is_identifier_continue(ch) {
                text.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }
        Token {
            kind: TokenKind::Identifier(text.clone()),
            span: Span {
                start,
                end: self.offset,
            },
            lexeme: text,
        }
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.source.len()
    }

    fn starts_with(&self, text: &str) -> bool {
        let mut index = self.offset;
        for ch in text.chars() {
            if self.source.get(index).copied() != Some(ch) {
                return false;
            }
            index += 1;
        }
        true
    }

    fn peek_char(&self) -> Option<char> {
        self.source.get(self.offset).copied()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += 1;
        Some(ch)
    }
}

fn simple_token(kind: TokenKind, start: usize, end: usize, lexeme: &str) -> Token {
    Token {
        kind,
        span: Span { start, end },
        lexeme: lexeme.to_string(),
    }
}

fn is_identifier_start(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

fn is_identifier_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '-'
}
