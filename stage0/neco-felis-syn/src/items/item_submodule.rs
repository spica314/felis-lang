use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{Token, TokenKeyword, TokenSemicolon, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemSubmodule<P: Phase> {
    pub keyword_submodule: TokenKeyword,
    pub name: TokenVariable,
    pub semicolon: TokenSemicolon,
    pub ext: P::ItemSubmoduleExt,
}

impl Parse for ItemSubmodule<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(keyword_submodule) = TokenKeyword::parse_keyword(tokens, &mut k, "submodule")?
        else {
            return Ok(None);
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_submodule_name"));
        };

        let Some(semicolon) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_submodule_semicolon"));
        };

        let item_submodule = ItemSubmodule {
            keyword_submodule,
            name,
            semicolon,
            ext: (),
        };

        *i = k;
        Ok(Some(item_submodule))
    }
}
