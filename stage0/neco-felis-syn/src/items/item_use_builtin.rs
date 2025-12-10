use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{Token, TokenKeyword, TokenSemicolon, TokenString, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemUseBuiltin<P: Phase> {
    pub keyword_use_builtin: TokenKeyword,
    pub builtin_name: TokenString,
    pub keyword_as: TokenKeyword,
    pub name: TokenVariable,
    pub semicolon: TokenSemicolon,
    pub ext: P::ItemBuiltinExt,
}

impl Parse for ItemUseBuiltin<PhaseParse> {
    fn parse(
        tokens: &[Token],
        i: &mut usize,
    ) -> Result<Option<ItemUseBuiltin<PhaseParse>>, ParseError> {
        let mut k = *i;

        let Some(keyword_use_builtin) = TokenKeyword::parse_keyword(tokens, &mut k, "use_builtin")?
        else {
            return Ok(None);
        };

        let Some(builtin_name) = TokenString::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(keyword_as) = TokenKeyword::parse_keyword(tokens, &mut k, "as")? else {
            return Ok(None);
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(semicolon) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let item_builtin = ItemUseBuiltin {
            keyword_use_builtin,
            builtin_name,
            keyword_as,
            name,
            semicolon,
            ext: (),
        };

        *i = k;
        Ok(Some(item_builtin))
    }
}
