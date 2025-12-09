use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{TokenBraceL, TokenBraceR, TokenColon, TokenComma, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemStructField<P: Phase> {
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub ty: Box<Term<P>>,
    pub comma: TokenComma,
    pub ext: P::ItemStructFieldExt,
}

impl Parse for ItemStructField<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_struct_field_name"));
        };

        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_struct_field_type"));
        };

        let Some(comma) = TokenComma::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected , after Term"));
        };

        let field = ItemStructField {
            name,
            colon,
            ty: Box::new(ty),
            comma,
            ext: (),
        };

        *i = k;
        Ok(Some(field))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemType<P: Phase> {
    pub keyword_type: TokenKeyword,
    pub name: TokenVariable,
    pub brace_l: TokenBraceL,
    pub constructors: Vec<ItemTypeConstructor<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemTypeExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemTypeConstructor<P: Phase> {
    pub name: TokenVariable,
    pub brace_l: TokenBraceL,
    pub fields: Vec<ItemStructField<P>>,
    pub brace_r: TokenBraceR,
    pub comma: Option<TokenComma>,
    pub ext: P::ItemTypeConstructorExt,
}

impl<P: Phase> ItemType<P> {
    pub fn name(&self) -> &TokenVariable {
        &self.name
    }

    pub fn constructors(&self) -> &[ItemTypeConstructor<P>] {
        &self.constructors
    }
}

impl<P: Phase> ItemTypeConstructor<P> {
    pub fn fields(&self) -> &[ItemStructField<P>] {
        &self.fields
    }
}

impl Parse for ItemType<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(keyword_type) = TokenKeyword::parse_keyword(tokens, &mut k, "type")? else {
            return Ok(None);
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_type_name"));
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_type_brace_l"));
        };

        let mut constructors = Vec::new();
        while let Some(constructor) = ItemTypeConstructor::parse(tokens, &mut k)? {
            constructors.push(constructor);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_type_brace_r"));
        };

        let item_type = ItemType {
            keyword_type,
            name,
            brace_l,
            constructors,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(item_type))
    }
}

impl Parse for ItemTypeConstructor<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_type_constructor_brace_l"));
        };

        let mut fields = Vec::new();
        while let Some(field) = ItemStructField::parse(tokens, &mut k)? {
            fields.push(field);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_type_constructor_brace_r"));
        };

        let comma = TokenComma::parse(tokens, &mut k)?;

        let constructor = ItemTypeConstructor {
            name,
            brace_l,
            fields,
            brace_r,
            comma,
            ext: (),
        };

        *i = k;
        Ok(Some(constructor))
    }
}
