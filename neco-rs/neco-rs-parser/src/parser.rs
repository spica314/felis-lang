use crate::{Error, Keyword, Result, Span, Token, TokenKind};

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Option<Self>>;
}

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    stop_at_match_arm_boundary: bool,
    stop_at_left_brace: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            stop_at_match_arm_boundary: false,
            stop_at_left_brace: false,
        }
    }

    pub(crate) fn with_match_arm_boundary<T>(
        &mut self,
        stop_at_match_arm_boundary: bool,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let previous = self.stop_at_match_arm_boundary;
        self.stop_at_match_arm_boundary = stop_at_match_arm_boundary;
        let result = f(self);
        self.stop_at_match_arm_boundary = previous;
        result
    }

    pub(crate) fn stop_at_match_arm_boundary(&self) -> bool {
        self.stop_at_match_arm_boundary
    }

    pub(crate) fn with_left_brace_boundary<T>(
        &mut self,
        stop_at_left_brace: bool,
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let previous = self.stop_at_left_brace;
        self.stop_at_left_brace = stop_at_left_brace;
        let result = f(self);
        self.stop_at_left_brace = previous;
        result
    }

    pub(crate) fn stop_at_left_brace(&self) -> bool {
        self.stop_at_left_brace
    }

    pub(crate) fn looks_like_typed_binder(&self) -> bool {
        matches!(
            (
                self.tokens.get(self.cursor),
                self.tokens.get(self.cursor + 1),
                self.tokens.get(self.cursor + 2)
            ),
            (
                Some(Token {
                    kind: TokenKind::Identifier(_),
                    ..
                }),
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }),
                Some(_)
            )
        )
    }

    pub(crate) fn looks_like_match_arm_boundary(&self) -> bool {
        if !self.is_pattern_start() {
            return false;
        }

        let mut index = self.cursor;
        let mut bracket_depth = 0usize;

        while let Some(token) = self.tokens.get(index) {
            match &token.kind {
                TokenKind::FatArrow if bracket_depth == 0 => return true,
                TokenKind::RightBrace if bracket_depth == 0 => return false,
                TokenKind::LeftBracket => bracket_depth += 1,
                TokenKind::RightBracket => {
                    if bracket_depth == 0 {
                        return false;
                    }
                    bracket_depth -= 1;
                }
                TokenKind::Identifier(_)
                | TokenKind::Keyword(Keyword::Package)
                | TokenKind::DoubleColon
                | TokenKind::Underscore => {}
                _ => return false,
            }
            index += 1;
        }

        false
    }

    pub(crate) fn is_item_start(&self) -> bool {
        self.check_keyword(Keyword::Pub)
            || matches!(
                self.peek_kind(),
                TokenKind::Keyword(
                    Keyword::EntryPoint
                        | Keyword::Use
                        | Keyword::Mod
                        | Keyword::BindBuiltin
                        | Keyword::Fn
                        | Keyword::Proc
                        | Keyword::Struct
                        | Keyword::Type
                        | Keyword::Prop
                        | Keyword::Theorem
                )
            )
    }

    pub(crate) fn is_term_start(&self) -> bool {
        self.check_keyword(Keyword::Match)
            || self.check_punctuation(TokenKind::LeftParen)
            || matches!(
                self.peek_kind(),
                TokenKind::StringLiteral(_)
                    | TokenKind::CharLiteral(_)
                    | TokenKind::IntegerLiteral(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::Keyword(Keyword::Package)
                    | TokenKind::Ampersand
                    | TokenKind::AmpersandCaret
            )
    }

    pub(crate) fn is_pattern_start(&self) -> bool {
        self.check_punctuation(TokenKind::Underscore)
            || matches!(
                self.peek_kind(),
                TokenKind::Identifier(_) | TokenKind::Keyword(Keyword::Package)
            )
    }

    pub(crate) fn is_path_start(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::Identifier(_) | TokenKind::Keyword(Keyword::Package)
        )
    }

    pub(crate) fn at_end(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::EndOfFile)
    }

    pub(crate) fn peek_kind(&self) -> TokenKind {
        self.tokens
            .get(self.cursor)
            .map(|token| token.kind.clone())
            .unwrap_or(TokenKind::EndOfFile)
    }

    pub(crate) fn check_keyword(&self, keyword: Keyword) -> bool {
        matches!(self.peek_kind(), TokenKind::Keyword(found) if found == keyword)
    }

    pub(crate) fn consume_keyword(&mut self, keyword: Keyword) -> bool {
        if self.check_keyword(keyword) {
            self.cursor += 1;
            true
        } else {
            false
        }
    }

    pub(crate) fn expect_keyword(&mut self, keyword: Keyword) -> Result<()> {
        if self.consume_keyword(keyword) {
            Ok(())
        } else {
            Err(self.error_here(format!("expected keyword `#{}`", keyword_name(keyword))))
        }
    }

    pub(crate) fn check_punctuation(&self, kind: TokenKind) -> bool {
        self.peek_kind() == kind
    }

    pub(crate) fn consume_punctuation(&mut self, kind: TokenKind) -> bool {
        if self.check_punctuation(kind.clone()) {
            self.cursor += 1;
            true
        } else {
            false
        }
    }

    pub(crate) fn expect_punctuation(&mut self, kind: TokenKind) -> Result<()> {
        if self.consume_punctuation(kind.clone()) {
            Ok(())
        } else {
            Err(self.error_here(format!("expected `{}`", punctuation_name(&kind))))
        }
    }

    pub(crate) fn consume_identifier(&mut self) -> Option<String> {
        match self.peek_kind() {
            TokenKind::Identifier(name) => {
                self.cursor += 1;
                Some(name)
            }
            _ => None,
        }
    }

    pub(crate) fn expect_identifier(&mut self) -> Result<String> {
        self.consume_identifier()
            .ok_or_else(|| self.error_here("expected identifier"))
    }

    pub(crate) fn consume_string_literal(&mut self) -> Option<String> {
        match self.peek_kind() {
            TokenKind::StringLiteral(value) => {
                self.cursor += 1;
                Some(value)
            }
            _ => None,
        }
    }

    pub(crate) fn expect_string_literal(&mut self) -> Result<String> {
        self.consume_string_literal()
            .ok_or_else(|| self.error_here("expected string literal"))
    }

    pub(crate) fn consume_integer_literal(&mut self) -> Option<String> {
        match self.peek_kind() {
            TokenKind::IntegerLiteral(value) => {
                self.cursor += 1;
                Some(value)
            }
            _ => None,
        }
    }

    pub(crate) fn consume_char_literal(&mut self) -> Option<char> {
        match self.peek_kind() {
            TokenKind::CharLiteral(value) => {
                self.cursor += 1;
                Some(value)
            }
            _ => None,
        }
    }

    pub(crate) fn error_here(&self, message: impl Into<String>) -> Error {
        let span = self
            .tokens
            .get(self.cursor)
            .map(|token| token.span)
            .unwrap_or(Span { start: 0, end: 0 });
        Error::new(message).with_span(span)
    }
}

fn keyword_name(keyword: Keyword) -> &'static str {
    match keyword {
        Keyword::As => "as",
        Keyword::BindBuiltin => "bind_builtin",
        Keyword::Break => "break",
        Keyword::Continue => "continue",
        Keyword::EntryPoint => "entrypoint",
        Keyword::Else => "else",
        Keyword::Fn => "fn",
        Keyword::Proc => "proc",
        Keyword::Struct => "struct",
        Keyword::Pub => "pub",
        Keyword::Type => "type",
        Keyword::Theorem => "theorem",
        Keyword::Prop => "prop",
        Keyword::If => "if",
        Keyword::Match => "match",
        Keyword::Let => "let",
        Keyword::LetRef => "letref",
        Keyword::Loop => "loop",
        Keyword::Use => "use",
        Keyword::Mod => "mod",
        Keyword::With => "with",
        Keyword::Excl => "excl",
        Keyword::Borrow => "borrow",
        Keyword::Forall => "forall",
        Keyword::Package => "package",
    }
}

fn punctuation_name(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::LeftParen => "(",
        TokenKind::RightParen => ")",
        TokenKind::LeftBrace => "{",
        TokenKind::RightBrace => "}",
        TokenKind::LeftBracket => "[",
        TokenKind::RightBracket => "]",
        TokenKind::Colon => ":",
        TokenKind::Semicolon => ";",
        TokenKind::Comma => ",",
        TokenKind::Equals => "=",
        TokenKind::Ampersand => "&",
        TokenKind::AmpersandCaret => "&^",
        TokenKind::Underscore => "_",
        TokenKind::DoubleColon => "::",
        TokenKind::Arrow => "->",
        TokenKind::FatArrow => "=>",
        TokenKind::LeftArrow => "<-",
        TokenKind::Dot => ".",
        TokenKind::DotArrow => ".>",
        TokenKind::EndOfFile => "end of file",
        TokenKind::Keyword(_)
        | TokenKind::Identifier(_)
        | TokenKind::StringLiteral(_)
        | TokenKind::CharLiteral(_)
        | TokenKind::IntegerLiteral(_) => "token",
    }
}
