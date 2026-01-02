use crate::error::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Comma,
    String(String),
    Number(String),
    True,
    False,
    Null,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

impl Token {
    fn new(kind: TokenKind, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}

pub struct Lexer {
    chars: Vec<char>,
    index: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn lex(input: &str) -> Result<Vec<Token>, ParseError> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        loop {
            lexer.skip_whitespace();
            let (line, column) = (lexer.line, lexer.column);
            let Some(ch) = lexer.peek() else {
                tokens.push(Token::new(TokenKind::Eof, line, column));
                break;
            };

            let token = match ch {
                '{' => {
                    lexer.bump();
                    Token::new(TokenKind::LBrace, line, column)
                }
                '}' => {
                    lexer.bump();
                    Token::new(TokenKind::RBrace, line, column)
                }
                '[' => {
                    lexer.bump();
                    Token::new(TokenKind::LBracket, line, column)
                }
                ']' => {
                    lexer.bump();
                    Token::new(TokenKind::RBracket, line, column)
                }
                ':' => {
                    lexer.bump();
                    Token::new(TokenKind::Colon, line, column)
                }
                ',' => {
                    lexer.bump();
                    Token::new(TokenKind::Comma, line, column)
                }
                '"' => {
                    lexer.bump();
                    let value = lexer.lex_string()?;
                    Token::new(TokenKind::String(value), line, column)
                }
                '-' | '0'..='9' => {
                    let value = lexer.lex_number()?;
                    Token::new(TokenKind::Number(value), line, column)
                }
                't' => {
                    lexer.consume_keyword("true")?;
                    Token::new(TokenKind::True, line, column)
                }
                'f' => {
                    lexer.consume_keyword("false")?;
                    Token::new(TokenKind::False, line, column)
                }
                'n' => {
                    lexer.consume_keyword("null")?;
                    Token::new(TokenKind::Null, line, column)
                }
                _ => {
                    return Err(ParseError::new(
                        format!("unexpected character '{}'", ch),
                        line,
                        column,
                    ));
                }
            };

            tokens.push(token);
        }

        Ok(tokens)
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.chars.get(self.index).copied();
        if let Some(ch) = ch {
            self.index += 1;
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn lex_string(&mut self) -> Result<String, ParseError> {
        let mut out = String::new();

        loop {
            let (line, column) = (self.line, self.column);
            let Some(ch) = self.peek() else {
                return Err(ParseError::new("unterminated string", line, column));
            };

            if ch == '"' {
                self.bump();
                break;
            }

            if ch <= '\u{001F}' {
                return Err(ParseError::new("control character in string", line, column));
            }

            if ch == '\\' {
                self.bump();
                let (line, column) = (self.line, self.column);
                let Some(esc) = self.bump() else {
                    return Err(ParseError::new("unterminated escape", line, column));
                };
                match esc {
                    '"' => out.push('"'),
                    '\\' => out.push('\\'),
                    '/' => out.push('/'),
                    'b' => out.push('\u{0008}'),
                    'f' => out.push('\u{000C}'),
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    'u' => {
                        let code_unit = self.read_hex4()?;
                        if (0xD800..=0xDBFF).contains(&code_unit) {
                            let (line, column) = (self.line, self.column);
                            let Some('\\') = self.peek() else {
                                return Err(ParseError::new(
                                    "expected low surrogate",
                                    line,
                                    column,
                                ));
                            };
                            self.bump();
                            let Some('u') = self.peek() else {
                                return Err(ParseError::new(
                                    "expected low surrogate",
                                    line,
                                    column,
                                ));
                            };
                            self.bump();
                            let low = self.read_hex4()?;
                            if !(0xDC00..=0xDFFF).contains(&low) {
                                return Err(ParseError::new(
                                    "invalid low surrogate",
                                    line,
                                    column,
                                ));
                            }
                            let high = code_unit as u32 - 0xD800;
                            let low = low as u32 - 0xDC00;
                            let scalar = 0x10000 + ((high << 10) | low);
                            if let Some(ch) = char::from_u32(scalar) {
                                out.push(ch);
                            } else {
                                return Err(ParseError::new(
                                    "invalid unicode scalar",
                                    line,
                                    column,
                                ));
                            }
                        } else if (0xDC00..=0xDFFF).contains(&code_unit) {
                            return Err(ParseError::new("unexpected low surrogate", line, column));
                        } else if let Some(ch) = char::from_u32(code_unit as u32) {
                            out.push(ch);
                        } else {
                            return Err(ParseError::new("invalid unicode scalar", line, column));
                        }
                    }
                    _ => {
                        return Err(ParseError::new("invalid escape", line, column));
                    }
                }
            } else {
                self.bump();
                out.push(ch);
            }
        }

        Ok(out)
    }

    fn read_hex4(&mut self) -> Result<u16, ParseError> {
        let mut value: u16 = 0;
        for _ in 0..4 {
            let (line, column) = (self.line, self.column);
            let Some(ch) = self.bump() else {
                return Err(ParseError::new("unterminated unicode escape", line, column));
            };
            let digit = ch.to_digit(16).ok_or_else(|| {
                ParseError::new("invalid unicode escape", line, column)
            })?;
            value = (value << 4) | digit as u16;
        }
        Ok(value)
    }

    fn consume_keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        let (line, column) = (self.line, self.column);
        let mut i = self.index;
        for expected in keyword.chars() {
            let Some(actual) = self.chars.get(i).copied() else {
                return Err(ParseError::new("invalid literal", line, column));
            };
            if actual != expected {
                return Err(ParseError::new("invalid literal", line, column));
            }
            i += 1;
        }

        for _ in 0..keyword.chars().count() {
            self.bump();
        }

        Ok(())
    }

    fn lex_number(&mut self) -> Result<String, ParseError> {
        let mut buf = String::new();
        let (line, column) = (self.line, self.column);

        if self.peek() == Some('-') {
            buf.push('-');
            self.bump();
        }

        let Some(ch) = self.peek() else {
            return Err(ParseError::new("invalid number", line, column));
        };

        if ch == '0' {
            buf.push('0');
            self.bump();
            if matches!(self.peek(), Some('0'..='9')) {
                return Err(ParseError::new("leading zero in number", line, column));
            }
        } else if ch.is_ascii_digit() {
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    buf.push(ch);
                    self.bump();
                } else {
                    break;
                }
            }
        } else {
            return Err(ParseError::new("invalid number", line, column));
        }

        if self.peek() == Some('.') {
            buf.push('.');
            self.bump();
            if !matches!(self.peek(), Some('0'..='9')) {
                return Err(ParseError::new("invalid fraction", line, column));
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    buf.push(ch);
                    self.bump();
                } else {
                    break;
                }
            }
        }

        if matches!(self.peek(), Some('e' | 'E')) {
            let exp_char = self.bump().unwrap();
            buf.push(exp_char);
            if matches!(self.peek(), Some('+' | '-')) {
                buf.push(self.bump().unwrap());
            }
            if !matches!(self.peek(), Some('0'..='9')) {
                return Err(ParseError::new("invalid exponent", line, column));
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    buf.push(ch);
                    self.bump();
                } else {
                    break;
                }
            }
        }

        Ok(buf)
    }
}
