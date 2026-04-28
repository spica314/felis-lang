use crate::lexer::{
    TokenDoubleColon, TokenIdentifier, TokenKeyword, TokenKeywordKind, TokenLeftBracket,
    TokenRightBracket, TokenUnderscore,
};
use crate::{Error, Parse, Result, Token};

use super::{PathExpression, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Wildcard,
    Bind(String),
    Constructor {
        path: PathExpression,
        subpatterns: Vec<Pattern>,
    },
}

impl Parse for Pattern {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        if TokenUnderscore::parse(tokens, &mut k)?.is_some() {
            *i = k;
            return Ok(Some(Self::Wildcard));
        }

        let Some(path) = parse_constructor_pattern_path(tokens, &mut k)? else {
            return Ok(None);
        };

        if path.token_keyword_package.is_none() && path.segments.len() == 1 {
            *i = k;
            return Ok(Some(Self::Bind(path.segments[0].lexeme.clone())));
        }

        let mut subpatterns = Vec::new();
        while is_pattern_start(tokens, k) {
            let Some(subpattern) = Self::parse(tokens, &mut k)? else {
                return Err(Error::Message(
                    "expected constructor subpattern".to_string(),
                ));
            };
            subpatterns.push(subpattern);
        }

        *i = k;
        Ok(Some(Self::Constructor { path, subpatterns }))
    }
}

fn parse_constructor_pattern_path(
    tokens: &[Token],
    i: &mut usize,
) -> Result<Option<PathExpression>> {
    let mut k = *i;
    let mut segments = Vec::new();

    let token_keyword_package =
        TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Package))?;
    if token_keyword_package.is_some() {
        let Some(_) = TokenDoubleColon::parse(tokens, &mut k)? else {
            return Err(Error::Message("expected `::` after `package`".to_string()));
        };
    }

    let Some(segment) = TokenIdentifier::parse(tokens, &mut k)? else {
        return Ok(None);
    };
    segments.push(segment);
    parse_ignored_suffixes(tokens, &mut k)?;

    while TokenDoubleColon::parse(tokens, &mut k)?.is_some() {
        let Some(segment) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(Error::Message(
                "expected path segment after `::`".to_string(),
            ));
        };
        segments.push(segment);
        parse_ignored_suffixes(tokens, &mut k)?;
    }

    *i = k;
    Ok(Some(PathExpression {
        token_keyword_package,
        segments,
    }))
}

fn parse_ignored_suffixes(tokens: &[Token], i: &mut usize) -> Result<()> {
    while TokenLeftBracket::parse(tokens, i)?.is_some() {
        let Some(_) = Term::parse(tokens, i)? else {
            return Err(Error::Message(
                "expected pattern path suffix term".to_string(),
            ));
        };
        let Some(_) = TokenRightBracket::parse(tokens, i)? else {
            return Err(Error::Message(
                "expected `]` after pattern path suffix".to_string(),
            ));
        };
    }
    Ok(())
}

pub(crate) fn is_pattern_start(tokens: &[Token], i: usize) -> bool {
    matches!(
        tokens.get(i),
        Some(Token::Underscore(_)) | Some(Token::Identifier(_))
    ) || matches!(
        tokens.get(i),
        Some(Token::Keyword(keyword)) if keyword.kind == TokenKeywordKind::Package
    )
}
