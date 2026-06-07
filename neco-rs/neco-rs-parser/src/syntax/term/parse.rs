use crate::lexer::{
    TokenAmpersand, TokenAmpersandCaret, TokenArrow, TokenCharLiteral, TokenComma, TokenDot,
    TokenDotArrow, TokenDoubleColon, TokenEquals, TokenIdentifier, TokenIntegerLiteral,
    TokenKeyword, TokenKeywordKind, TokenLeftBrace, TokenLeftBracket, TokenLeftParen,
    TokenRightBrace, TokenRightBracket, TokenRightParen, TokenSemicolon, TokenStringLiteral,
};
use crate::{Error, Parse, Result, Token};

use crate::syntax::PathExpression;
use crate::syntax::pattern::is_pattern_start;

use super::*;

pub(super) fn parse_arrow_term(
    tokens: &[Token],
    i: &mut usize,
    option: TermParseOption,
) -> Result<Option<Term>> {
    let Some(left) = parse_forall_term(tokens, i, option)? else {
        return Ok(None);
    };
    if TokenArrow::parse(tokens, i)?.is_some() {
        let result =
            parse_arrow_term(tokens, i, option)?.ok_or_else(|| expected("arrow result type"))?;
        let parameter = match left {
            Term::TypedBinder(binder) => ArrowParameter::Binder(binder),
            other => ArrowParameter::Domain(Box::new(other)),
        };
        return Ok(Some(Term::Arrow(ArrowTerm {
            parameter,
            result: Box::new(result),
        })));
    }
    Ok(Some(left))
}

fn parse_forall_term(
    tokens: &[Token],
    i: &mut usize,
    option: TermParseOption,
) -> Result<Option<Term>> {
    if TokenKeyword::parse_with_option(tokens, i, Some(TokenKeywordKind::Forall))?.is_some() {
        let binder = TypedBinder::parse(tokens, i)?.ok_or_else(|| expected("forall binder"))?;
        expect_comma(tokens, i)?;
        let body = Term::parse_with_option(tokens, i, Some(option))?
            .ok_or_else(|| expected("forall body"))?;
        return Ok(Some(Term::Forall(ForallTerm {
            binder,
            body: Box::new(body),
        })));
    }
    parse_application_term(tokens, i, option).map(Some)
}

fn parse_application_term(
    tokens: &[Token],
    i: &mut usize,
    option: TermParseOption,
) -> Result<Term> {
    let mut term = parse_postfix_term(tokens, i, option)?;
    loop {
        if option.stop_at_left_brace && matches!(tokens.get(*i), Some(Token::LeftBrace(_))) {
            break;
        }
        if option.stop_at_match_arm_boundary
            && looks_like_match_arm_boundary(tokens, *i)
            && !looks_like_numeric_literal_suffix_continuation(&term, tokens, *i)
        {
            break;
        }
        if is_term_start(tokens, *i) {
            let argument = parse_postfix_term(tokens, i, option)?;
            term = match term {
                Term::Application {
                    callee,
                    mut arguments,
                } => {
                    arguments.push(argument);
                    Term::Application { callee, arguments }
                }
                other => Term::Application {
                    callee: Box::new(other),
                    arguments: vec![argument],
                },
            };
            continue;
        }
        break;
    }
    Ok(term)
}

fn parse_postfix_term(tokens: &[Token], i: &mut usize, option: TermParseOption) -> Result<Term> {
    let mut term = parse_primary_term(tokens, i)?;
    loop {
        if TokenLeftBracket::parse(tokens, i)?.is_some() {
            let argument =
                Term::parse(tokens, i)?.ok_or_else(|| expected("bracket suffix argument"))?;
            let Some(_) = TokenRightBracket::parse(tokens, i)? else {
                return Err(expected("`]` after bracket suffix"));
            };
            term = match term {
                Term::Application {
                    callee,
                    mut arguments,
                } => {
                    arguments.push(argument);
                    Term::Application { callee, arguments }
                }
                other => Term::Application {
                    callee: Box::new(other),
                    arguments: vec![argument],
                },
            };
            continue;
        }
        if TokenDoubleColon::parse(tokens, i)?.is_some() {
            let Some(segment) = TokenIdentifier::parse(tokens, i)? else {
                return Err(expected("path segment after `::`"));
            };
            term = append_path_segment(term, segment)?;
            continue;
        }
        if TokenDotArrow::parse(tokens, i)?.is_some() {
            let method = expect_identifier(tokens, i)?;
            term = Term::MethodCall {
                receiver: Box::new(term),
                method,
            };
            continue;
        }
        if TokenDot::parse(tokens, i)?.is_some() {
            let field = expect_identifier(tokens, i)?;
            term = Term::FieldAccess {
                receiver: Box::new(term),
                field,
            };
            continue;
        }
        if !option.stop_at_left_brace && matches!(tokens.get(*i), Some(Token::LeftBrace(_))) {
            match term {
                Term::Path(path) => {
                    term = Term::StructLiteral {
                        path,
                        fields: parse_struct_literal_fields(tokens, i)?,
                    };
                    continue;
                }
                other => {
                    term = other;
                }
            }
        }
        break;
    }
    Ok(term)
}

fn append_path_segment(term: Term, segment: TokenIdentifier) -> Result<Term> {
    match term {
        Term::Path(mut path) => {
            path.segments.push(segment);
            Ok(Term::Path(path))
        }
        Term::Application { callee, arguments } => {
            let Term::Path(mut path) = *callee else {
                return Err(Error::Message(
                    "`::` can only extend a path expression".to_string(),
                ));
            };
            path.segments.push(segment);
            Ok(Term::Application {
                callee: Box::new(Term::Path(path)),
                arguments,
            })
        }
        _ => Err(Error::Message(
            "`::` can only extend a path expression".to_string(),
        )),
    }
}

fn parse_primary_term(tokens: &[Token], i: &mut usize) -> Result<Term> {
    if TokenKeyword::parse_with_option(tokens, i, Some(TokenKeywordKind::Match))?.is_some() {
        return Ok(Term::Match(
            MatchExpression::parse(tokens, i)?.ok_or_else(|| expected("match expression"))?,
        ));
    }

    if matches!(tokens.get(*i), Some(Token::LeftBrace(_))) {
        return Ok(Term::Block(
            Block::parse(tokens, i)?.ok_or_else(|| expected("block expression"))?,
        ));
    }

    if TokenLeftParen::parse(tokens, i)?.is_some() {
        if TokenRightParen::parse(tokens, i)?.is_some() {
            return Ok(Term::Unit);
        }
        if looks_like_typed_binder(tokens, *i) {
            let binder = TypedBinder::parse(tokens, i)?.ok_or_else(|| expected("typed binder"))?;
            expect_right_paren(tokens, i)?;
            return Ok(Term::TypedBinder(binder));
        }
        let inner = Term::parse(tokens, i)?.ok_or_else(|| expected("grouped expression"))?;
        expect_right_paren(tokens, i)?;
        return Ok(Term::Group(Box::new(inner)));
    }

    if let Some(text) = TokenStringLiteral::parse(tokens, i)? {
        return Ok(Term::StringLiteral(parse_string_literal(&text.lexeme)?));
    }

    if let Some(ch) = TokenCharLiteral::parse(tokens, i)? {
        return Ok(Term::CharLiteral(parse_char_literal(&ch.lexeme)?));
    }

    if let Some(number) = TokenIntegerLiteral::parse(tokens, i)? {
        return Ok(Term::IntegerLiteral(number.lexeme));
    }

    if TokenAmpersand::parse(tokens, i)?.is_some() {
        return Ok(Term::Reference {
            referent: Box::new(parse_application_term(
                tokens,
                i,
                TermParseOption::default(),
            )?),
            exclusive: false,
        });
    }

    if TokenAmpersandCaret::parse(tokens, i)?.is_some() {
        return Ok(Term::Reference {
            referent: Box::new(parse_application_term(
                tokens,
                i,
                TermParseOption::default(),
            )?),
            exclusive: true,
        });
    }

    if is_path_start(tokens, *i) {
        return Ok(Term::Path(
            PathExpression::parse(tokens, i)?.ok_or_else(|| expected("path expression"))?,
        ));
    }

    Err(expected("term"))
}

fn parse_struct_literal_fields(tokens: &[Token], i: &mut usize) -> Result<Vec<StructLiteralField>> {
    let Some(_) = TokenLeftBrace::parse(tokens, i)? else {
        return Err(expected("struct literal field block"));
    };
    let mut fields = Vec::new();
    while TokenRightBrace::parse(tokens, i)?.is_none() {
        let name = expect_identifier(tokens, i)?;
        let Some(_) = TokenEquals::parse(tokens, i)? else {
            return Err(expected("`=` after struct literal field name"));
        };
        let value =
            Term::parse(tokens, i)?.ok_or_else(|| expected("struct literal field value"))?;
        fields.push(StructLiteralField { name, value });
        if TokenComma::parse(tokens, i)?.is_some() {
            continue;
        }
        expect_right_brace(tokens, i)?;
        break;
    }
    Ok(fields)
}

fn is_term_start(tokens: &[Token], i: usize) -> bool {
    matches!(
        tokens.get(i),
        Some(Token::StringLiteral(_))
            | Some(Token::CharLiteral(_))
            | Some(Token::IntegerLiteral(_))
            | Some(Token::Identifier(_))
            | Some(Token::Ampersand(_))
            | Some(Token::AmpersandCaret(_))
            | Some(Token::LeftParen(_))
            | Some(Token::LeftBrace(_))
    ) || matches!(
        tokens.get(i),
        Some(Token::Keyword(keyword))
            if matches!(
                keyword.kind,
                TokenKeywordKind::Package
                    | TokenKeywordKind::Match
                    | TokenKeywordKind::Forall
            )
    )
}

fn is_path_start(tokens: &[Token], i: usize) -> bool {
    matches!(tokens.get(i), Some(Token::Identifier(_)))
        || matches!(
            tokens.get(i),
            Some(Token::Keyword(keyword)) if keyword.kind == TokenKeywordKind::Package
        )
}

fn looks_like_typed_binder(tokens: &[Token], i: usize) -> bool {
    matches!(tokens.get(i), Some(Token::Identifier(_)))
        && matches!(tokens.get(i + 1), Some(Token::Colon(_)))
}

fn looks_like_match_arm_boundary(tokens: &[Token], i: usize) -> bool {
    if !is_pattern_start(tokens, i) {
        return false;
    }

    let mut index = i;
    let mut bracket_depth = 0usize;
    while let Some(token) = tokens.get(index) {
        match token {
            Token::FatArrow(_) if bracket_depth == 0 => return true,
            Token::RightBrace(_) if bracket_depth == 0 => return false,
            Token::LeftBracket(_) => bracket_depth += 1,
            Token::RightBracket(_) => {
                if bracket_depth == 0 {
                    return false;
                }
                bracket_depth -= 1;
            }
            Token::Identifier(_) | Token::DoubleColon(_) | Token::Underscore(_) => {}
            Token::Keyword(keyword) if keyword.kind == TokenKeywordKind::Package => {}
            _ => return false,
        }
        index += 1;
    }
    false
}

fn looks_like_numeric_literal_suffix_continuation(term: &Term, tokens: &[Token], i: usize) -> bool {
    matches!(term, Term::IntegerLiteral(_))
        && matches!(
            tokens.get(i),
            Some(Token::Identifier(identifier)) if identifier.lexeme == "i32" || identifier.lexeme == "u8"
        )
}

fn expect_identifier(tokens: &[Token], i: &mut usize) -> Result<String> {
    let Some(identifier) = TokenIdentifier::parse(tokens, i)? else {
        return Err(expected("identifier"));
    };
    Ok(identifier.lexeme)
}

pub(super) fn expect_semicolon(tokens: &[Token], i: &mut usize) -> Result<()> {
    let Some(_) = TokenSemicolon::parse(tokens, i)? else {
        return Err(expected("`;`"));
    };
    Ok(())
}

pub(super) fn expect_comma(tokens: &[Token], i: &mut usize) -> Result<()> {
    let Some(_) = TokenComma::parse(tokens, i)? else {
        return Err(expected("`,`"));
    };
    Ok(())
}

fn expect_right_paren(tokens: &[Token], i: &mut usize) -> Result<()> {
    let Some(_) = TokenRightParen::parse(tokens, i)? else {
        return Err(expected("`)`"));
    };
    Ok(())
}

pub(super) fn expect_right_brace(tokens: &[Token], i: &mut usize) -> Result<()> {
    let Some(_) = TokenRightBrace::parse(tokens, i)? else {
        return Err(Error::Message(format!(
            "expected `}}`, found {:?}",
            tokens.get(*i)
        )));
    };
    Ok(())
}

fn parse_char_literal(lexeme: &str) -> Result<char> {
    let inner = lexeme
        .strip_prefix('\'')
        .and_then(|text| text.strip_suffix('\''))
        .ok_or_else(|| invalid("char literal"))?;
    if let Some(escaped) = inner.strip_prefix('\\') {
        return match escaped {
            "'" => Ok('\''),
            "\"" => Ok('"'),
            "\\" => Ok('\\'),
            "0" => Ok('\0'),
            "n" => Ok('\n'),
            "r" => Ok('\r'),
            "t" => Ok('\t'),
            _ => Err(invalid("char literal escape sequence")),
        };
    }
    let mut chars = inner.chars();
    let Some(ch) = chars.next() else {
        return Err(invalid("empty char literal"));
    };
    if chars.next().is_some() {
        return Err(invalid("char literal with more than one character"));
    }
    if !ch.is_ascii() {
        return Err(Error::Message("char literal must be ASCII".to_string()));
    }
    Ok(ch)
}

fn parse_string_literal(lexeme: &str) -> Result<String> {
    let inner = lexeme
        .strip_prefix('"')
        .and_then(|text| text.strip_suffix('"'))
        .ok_or_else(|| invalid("string literal"))?;
    let mut result = String::new();
    let mut chars = inner.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            result.push(ch);
            continue;
        }
        let Some(escaped) = chars.next() else {
            return Err(invalid("string literal escape sequence"));
        };
        let value = match escaped {
            '\'' => '\'',
            '"' => '"',
            '\\' => '\\',
            '0' => '\0',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            _ => return Err(invalid("string literal escape sequence")),
        };
        result.push(value);
    }
    Ok(result)
}

pub(super) fn expected(what: &str) -> Error {
    Error::Message(format!("expected {what}"))
}

fn invalid(what: &str) -> Error {
    Error::Message(format!("invalid {what}"))
}
