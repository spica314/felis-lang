use crate::lexer::{TokenColon, TokenKeyword, TokenKeywordKind};
use crate::{Parse, Result, Token};

use super::{
    ConstructorDeclaration, DeclaredName, expected, parse_constructor_block,
    parse_term_before_left_brace,
};
use crate::syntax::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: DeclaredName,
    pub ty: Term,
    pub constructors: Vec<ConstructorDeclaration>,
}

impl Parse for PropDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Prop))?
        else {
            return Ok(None);
        };
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("prop name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after prop name"));
        };
        let Some(ty) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("prop type"));
        };
        let Some(constructors) = parse_constructor_block(tokens, &mut k)? else {
            return Err(expected("prop constructor block"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name,
            ty,
            constructors,
        }))
    }
}
