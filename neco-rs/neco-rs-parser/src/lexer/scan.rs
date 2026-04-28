use crate::{Error, Result};

use super::*;

pub fn skip_trivia(chars: &[char], i: &mut usize, r: &mut usize, c: &mut usize) {
    while *i < chars.len() {
        if chars[*i].is_whitespace() {
            if chars[*i] == '\n' {
                *r += 1;
                *c = 0;
            } else {
                *c += 1;
            }
            *i += 1;
            continue;
        }

        if chars[*i..].starts_with(&['/', '/']) {
            while *i < chars.len() {
                let ch = chars[*i];
                *i += 1;
                if ch == '\n' {
                    *r += 1;
                    *c = 0;
                    break;
                }
                *c += 1;
            }
            continue;
        }

        if chars[*i..].starts_with(&['/', '*']) {
            *i += 2;
            *c += 2;
            while *i < chars.len() && !chars[*i..].starts_with(&['*', '/']) {
                if chars[*i] == '\n' {
                    *r += 1;
                    *c = 0;
                } else {
                    *c += 1;
                }
                *i += 1;
            }
            if *i < chars.len() {
                *i += 2;
                *c += 2;
            }
            continue;
        }

        break;
    }
}

pub(crate) fn lex(source: &str) -> Result<Vec<Token>> {
    let chars: Vec<_> = source.chars().collect();

    let mut tokens = vec![];
    let mut i = 0;
    let mut r = 0;
    let mut c = 0;

    while i < chars.len() {
        skip_trivia(&chars, &mut i, &mut r, &mut c);
        if i == chars.len() {
            break;
        }

        if chars[i] == '#' {
            // keyword
            let start = FilePos { r, c };
            let start_i = i;
            i += 1;
            c += 1;
            while i < chars.len()
                && (chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-')
            {
                i += 1;
                c += 1;
            }
            let lexeme: String = chars[start_i..i].iter().collect();
            let kind = match lexeme.as_str() {
                "#as" => TokenKeywordKind::As,
                "#bind_builtin" => TokenKeywordKind::BindBuiltin,
                "#break" => TokenKeywordKind::Break,
                "#continue" => TokenKeywordKind::Continue,
                "#entrypoint" => TokenKeywordKind::EntryPoint,
                "#else" => TokenKeywordKind::Else,
                "#fn" => TokenKeywordKind::Fn,
                "#proc" => TokenKeywordKind::Proc,
                "#struct" => TokenKeywordKind::Struct,
                "#pub" => TokenKeywordKind::Pub,
                "#type" => TokenKeywordKind::Type,
                "#theorem" => TokenKeywordKind::Theorem,
                "#prop" => TokenKeywordKind::Prop,
                "#if" => TokenKeywordKind::If,
                "#match" => TokenKeywordKind::Match,
                "#let" => TokenKeywordKind::Let,
                "#letref" => TokenKeywordKind::LetRef,
                "#loop" => TokenKeywordKind::Loop,
                "#use" => TokenKeywordKind::Use,
                "#mod" => TokenKeywordKind::Mod,
                "#with" => TokenKeywordKind::With,
                "#excl" => TokenKeywordKind::Excl,
                "#borrow" => TokenKeywordKind::Borrow,
                "#forall" => TokenKeywordKind::Forall,
                "#package" => TokenKeywordKind::Package,
                _ => {
                    return Err(Error::MessageWithSpan(
                        format!("unknown keyword `{lexeme}`"),
                        Span {
                            start,
                            end: FilePos { r, c },
                        },
                    ));
                }
            };
            tokens.push(Token::Keyword(TokenKeyword {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
                lexeme,
                kind,
            }));
            continue;
        }

        /* symbols */
        if chars[i..].starts_with(&[':', ':']) {
            let start = FilePos { r, c };
            i += 2;
            c += 2;
            tokens.push(Token::DoubleColon(TokenDoubleColon {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
            }));
            continue;
        }
        if chars[i..].starts_with(&['-', '>']) {
            let start = FilePos { r, c };
            i += 2;
            c += 2;
            tokens.push(Token::Arrow(TokenArrow {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
            }));
            continue;
        }
        if chars[i..].starts_with(&['=', '>']) {
            let start = FilePos { r, c };
            i += 2;
            c += 2;
            tokens.push(Token::FatArrow(TokenFatArrow {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
            }));
            continue;
        }
        if chars[i..].starts_with(&['<', '-']) {
            let start = FilePos { r, c };
            i += 2;
            c += 2;
            tokens.push(Token::LeftArrow(TokenLeftArrow {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
            }));
            continue;
        }
        if chars[i..].starts_with(&['.', '>']) {
            let start = FilePos { r, c };
            i += 2;
            c += 2;
            tokens.push(Token::DotArrow(TokenDotArrow {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
            }));
            continue;
        }
        if chars[i..].starts_with(&['&', '^']) {
            let start = FilePos { r, c };
            i += 2;
            c += 2;
            tokens.push(Token::AmpersandCaret(TokenAmpersandCaret {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
            }));
            continue;
        }
        let start = FilePos { r, c };
        let token = match chars[i] {
            '(' => Some(Token::LeftParen(TokenLeftParen {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            ')' => Some(Token::RightParen(TokenRightParen {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            '{' => Some(Token::LeftBrace(TokenLeftBrace {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            '}' => Some(Token::RightBrace(TokenRightBrace {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            '[' => Some(Token::LeftBracket(TokenLeftBracket {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            ']' => Some(Token::RightBracket(TokenRightBracket {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            ':' => Some(Token::Colon(TokenColon {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            ';' => Some(Token::Semicolon(TokenSemicolon {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            ',' => Some(Token::Comma(TokenComma {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            '=' => Some(Token::Equals(TokenEquals {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            '&' => Some(Token::Ampersand(TokenAmpersand {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            '_' => Some(Token::Underscore(TokenUnderscore {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            '.' => Some(Token::Dot(TokenDot {
                span: Span {
                    start,
                    end: FilePos { r, c: c + 1 },
                },
            })),
            _ => None,
        };
        if let Some(token) = token {
            tokens.push(token);
            i += 1;
            c += 1;
            continue;
        }

        /* literals */
        let start = FilePos { r, c };
        let start_i = i;
        if chars[i] == '"' {
            let mut terminated = false;
            i += 1;
            c += 1;
            while i < chars.len() {
                if chars[i] == '"' {
                    i += 1;
                    c += 1;
                    terminated = true;
                    let lexeme: String = chars[start_i..i].iter().collect();
                    tokens.push(Token::StringLiteral(TokenStringLiteral {
                        span: Span {
                            start,
                            end: FilePos { r, c },
                        },
                        lexeme,
                    }));
                    break;
                }
                if chars[i] == '\\' && i + 1 < chars.len() {
                    i += 1;
                    c += 1;
                }
                if chars[i] == '\n' {
                    r += 1;
                    c = 0;
                } else {
                    c += 1;
                }
                i += 1;
            }
            if !terminated {
                return Err(Error::MessageWithSpan(
                    "unterminated string literal".to_string(),
                    Span {
                        start,
                        end: FilePos { r, c },
                    },
                ));
            }
            continue;
        }
        if chars[i] == '\'' {
            let mut terminated = false;
            i += 1;
            c += 1;
            while i < chars.len() {
                if chars[i] == '\'' {
                    i += 1;
                    c += 1;
                    terminated = true;
                    let lexeme: String = chars[start_i..i].iter().collect();
                    tokens.push(Token::CharLiteral(TokenCharLiteral {
                        span: Span {
                            start,
                            end: FilePos { r, c },
                        },
                        lexeme,
                    }));
                    break;
                }
                if chars[i] == '\\' && i + 1 < chars.len() {
                    i += 1;
                    c += 1;
                }
                if chars[i] == '\n' {
                    r += 1;
                    c = 0;
                } else {
                    c += 1;
                }
                i += 1;
            }
            if !terminated {
                return Err(Error::MessageWithSpan(
                    "unterminated char literal".to_string(),
                    Span {
                        start,
                        end: FilePos { r, c },
                    },
                ));
            }
            continue;
        }
        if chars[i].is_ascii_digit() {
            i += 1;
            c += 1;
            if chars[start_i] == '0' && i < chars.len() && matches!(chars[i], 'x' | 'X') {
                i += 1;
                c += 1;
                while i < chars.len() && chars[i].is_ascii_hexdigit() {
                    i += 1;
                    c += 1;
                }
            } else {
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                    c += 1;
                }
            }
            let lexeme: String = chars[start_i..i].iter().collect();
            tokens.push(Token::IntegerLiteral(TokenIntegerLiteral {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
                lexeme,
            }));
            continue;
        }
        if chars[i].is_ascii_alphabetic() {
            i += 1;
            c += 1;
            while i < chars.len()
                && (chars[i].is_ascii_alphanumeric() || chars[i] == '_' || chars[i] == '-')
            {
                i += 1;
                c += 1;
            }
            let lexeme: String = chars[start_i..i].iter().collect();
            tokens.push(Token::Identifier(TokenIdentifier {
                span: Span {
                    start,
                    end: FilePos { r, c },
                },
                lexeme,
            }));
            continue;
        }

        return Err(Error::MessageWithSpan(
            format!("unexpected character `{}`", chars[i]),
            Span {
                start,
                end: FilePos { r, c: c + 1 },
            },
        ));
    }

    // eof
    tokens.push(Token::EndOfFile(TokenEndOfFile {
        span: Span {
            start: FilePos { r, c },
            end: FilePos { r, c },
        },
    }));

    Ok(tokens)
}
