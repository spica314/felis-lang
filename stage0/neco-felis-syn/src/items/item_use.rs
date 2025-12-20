use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{Token, TokenColon2, TokenKeyword, TokenSemicolon, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemUseScopePrefix<P: Phase> {
    pub name: TokenVariable,
    pub colon2: TokenColon2,
    pub ext: P::ItemUseScopePrefixExt,
}

impl Parse for ItemUseScopePrefix<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(colon2) = TokenColon2::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let prefix = ItemUseScopePrefix {
            name,
            colon2,
            ext: (),
        };

        *i = k;
        Ok(Some(prefix))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemUse<P: Phase> {
    pub keyword_use: TokenKeyword,
    pub scope_prefixes: Vec<ItemUseScopePrefix<P>>,
    pub name: TokenVariable,
    pub semicolon: TokenSemicolon,
    pub ext: P::ItemUseExt,
}

impl Parse for ItemUse<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(keyword_use) = TokenKeyword::parse_keyword(tokens, &mut k, "use")? else {
            return Ok(None);
        };

        let mut scope_prefixes = Vec::new();
        while let Some(prefix) = ItemUseScopePrefix::parse(tokens, &mut k)? {
            scope_prefixes.push(prefix);
        }

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_use_name"));
        };

        let Some(semicolon) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_use_semicolon"));
        };

        let item_use = ItemUse {
            keyword_use,
            scope_prefixes,
            name,
            semicolon,
            ext: (),
        };

        *i = k;
        Ok(Some(item_use))
    }
}
