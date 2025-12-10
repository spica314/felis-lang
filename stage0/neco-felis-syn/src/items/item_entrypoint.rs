use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{TokenKeyword, TokenSemicolon, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemEntrypoint<P: Phase> {
    pub keyword_entrypoint: TokenKeyword,
    pub name: TokenVariable,
    pub semicolon: TokenSemicolon,
    pub ext: P::ItemEntrypointExt,
}

impl Parse for ItemEntrypoint<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(keyword_entrypoint) = TokenKeyword::parse_keyword(tokens, &mut k, "entrypoint")?
        else {
            return Ok(None);
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected TokenVariable"));
        };

        let Some(semicolon) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected TokenSemicolon"));
        };

        *i = k;
        Ok(Some(ItemEntrypoint {
            keyword_entrypoint,
            name,
            semicolon,
            ext: (),
        }))
    }
}
