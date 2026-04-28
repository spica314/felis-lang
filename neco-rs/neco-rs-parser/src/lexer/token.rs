use crate::{Parse, Result};

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
