use crate::{Error, Parse, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FilePos {
    pub r: usize,
    pub c: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: FilePos,
    pub end: FilePos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(TokenKeyword),
    Identifier(TokenIdentifier),
    StringLiteral(TokenStringLiteral),
    CharLiteral(TokenCharLiteral),
    IntegerLiteral(TokenIntegerLiteral),
    LeftParen(TokenLeftParen),
    RightParen(TokenRightParen),
    LeftBrace(TokenLeftBrace),
    RightBrace(TokenRightBrace),
    LeftBracket(TokenLeftBracket),
    RightBracket(TokenRightBracket),
    Colon(TokenColon),
    Semicolon(TokenSemicolon),
    Comma(TokenComma),
    Equals(TokenEquals),
    Ampersand(TokenAmpersand),
    AmpersandCaret(TokenAmpersandCaret),
    Underscore(TokenUnderscore),
    DoubleColon(TokenDoubleColon),
    Arrow(TokenArrow),
    FatArrow(TokenFatArrow),
    LeftArrow(TokenLeftArrow),
    Dot(TokenDot),
    DotArrow(TokenDotArrow),
    EndOfFile(TokenEndOfFile),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenKeyword {
    pub span: Span,
    pub lexeme: String,
    pub kind: TokenKeywordKind,
}

impl Parse for TokenKeyword {
    type ParseOption = TokenKeywordKind;

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        option: Option<Self::ParseOption>,
    ) -> Result<Option<TokenKeyword>> {
        match &tokens[*i] {
            Token::Keyword(keyword) => {
                if let Some(kind) = option {
                    if kind == keyword.kind {
                        *i += 1;
                        Ok(Some(keyword.clone()))
                    } else {
                        Ok(None)
                    }
                } else {
                    *i += 1;
                    Ok(Some(keyword.clone()))
                }
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenIdentifier {
    pub span: Span,
    pub lexeme: String,
}

impl Parse for TokenIdentifier {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Identifier(identifier) => {
                *i += 1;
                Ok(Some(identifier.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenStringLiteral {
    pub span: Span,
    pub lexeme: String,
}

impl Parse for TokenStringLiteral {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::StringLiteral(string_literal) => {
                *i += 1;
                Ok(Some(string_literal.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenCharLiteral {
    pub span: Span,
    pub lexeme: String,
}

impl Parse for TokenCharLiteral {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::CharLiteral(char_literal) => {
                *i += 1;
                Ok(Some(char_literal.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenIntegerLiteral {
    pub span: Span,
    pub lexeme: String,
}

impl Parse for TokenIntegerLiteral {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::IntegerLiteral(integer_literal) => {
                *i += 1;
                Ok(Some(integer_literal.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenLeftParen {
    pub span: Span,
}

impl Parse for TokenLeftParen {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::LeftParen(left_paren) => {
                *i += 1;
                Ok(Some(left_paren.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenRightParen {
    pub span: Span,
}

impl Parse for TokenRightParen {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::RightParen(right_paren) => {
                *i += 1;
                Ok(Some(right_paren.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenLeftBrace {
    pub span: Span,
}

impl Parse for TokenLeftBrace {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::LeftBrace(left_brace) => {
                *i += 1;
                Ok(Some(left_brace.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenRightBrace {
    pub span: Span,
}

impl Parse for TokenRightBrace {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::RightBrace(right_brace) => {
                *i += 1;
                Ok(Some(right_brace.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenLeftBracket {
    pub span: Span,
}

impl Parse for TokenLeftBracket {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::LeftBracket(left_bracket) => {
                *i += 1;
                Ok(Some(left_bracket.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenRightBracket {
    pub span: Span,
}

impl Parse for TokenRightBracket {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::RightBracket(right_bracket) => {
                *i += 1;
                Ok(Some(right_bracket.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenColon {
    pub span: Span,
}

impl Parse for TokenColon {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Colon(colon) => {
                *i += 1;
                Ok(Some(colon.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenSemicolon {
    pub span: Span,
}

impl Parse for TokenSemicolon {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Semicolon(semicolon) => {
                *i += 1;
                Ok(Some(semicolon.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenComma {
    pub span: Span,
}

impl Parse for TokenComma {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Comma(comma) => {
                *i += 1;
                Ok(Some(comma.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenEquals {
    pub span: Span,
}

impl Parse for TokenEquals {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Equals(equals) => {
                *i += 1;
                Ok(Some(equals.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenAmpersand {
    pub span: Span,
}

impl Parse for TokenAmpersand {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Ampersand(ampersand) => {
                *i += 1;
                Ok(Some(ampersand.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenAmpersandCaret {
    pub span: Span,
}

impl Parse for TokenAmpersandCaret {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::AmpersandCaret(ampersand_caret) => {
                *i += 1;
                Ok(Some(ampersand_caret.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenUnderscore {
    pub span: Span,
}

impl Parse for TokenUnderscore {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Underscore(underscore) => {
                *i += 1;
                Ok(Some(underscore.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenDoubleColon {
    pub span: Span,
}

impl Parse for TokenDoubleColon {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::DoubleColon(double_colon) => {
                *i += 1;
                Ok(Some(double_colon.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenArrow {
    pub span: Span,
}

impl Parse for TokenArrow {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Arrow(arrow) => {
                *i += 1;
                Ok(Some(arrow.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenFatArrow {
    pub span: Span,
}

impl Parse for TokenFatArrow {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::FatArrow(fat_arrow) => {
                *i += 1;
                Ok(Some(fat_arrow.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenLeftArrow {
    pub span: Span,
}

impl Parse for TokenLeftArrow {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::LeftArrow(left_arrow) => {
                *i += 1;
                Ok(Some(left_arrow.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenDot {
    pub span: Span,
}

impl Parse for TokenDot {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::Dot(dot) => {
                *i += 1;
                Ok(Some(dot.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenDotArrow {
    pub span: Span,
}

impl Parse for TokenDotArrow {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::DotArrow(dot_arrow) => {
                *i += 1;
                Ok(Some(dot_arrow.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenEndOfFile {
    pub span: Span,
}

impl Parse for TokenEndOfFile {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        match &tokens[*i] {
            Token::EndOfFile(end_of_file) => {
                *i += 1;
                Ok(Some(end_of_file.clone()))
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKeywordKind {
    As,
    BindBuiltin,
    Break,
    Continue,
    EntryPoint,
    Else,
    Fn,
    Proc,
    Struct,
    Pub,
    Type,
    Theorem,
    Prop,
    If,
    Match,
    Let,
    LetRef,
    Loop,
    Use,
    Mod,
    With,
    Excl,
    Borrow,
    Forall,
    Package,
}

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
