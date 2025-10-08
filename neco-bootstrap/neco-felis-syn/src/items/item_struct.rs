use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{TokenBraceL, TokenBraceR, TokenColon, TokenComma, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemStruct<P: Phase> {
    pub keyword_struct: TokenKeyword,
    pub name: TokenVariable,
    pub brace_l: TokenBraceL,
    pub fields: Vec<ItemStructField<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemStructExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemStructField<P: Phase> {
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub ty: Box<Term<P>>,
    pub comma: TokenComma,
    pub ext: P::ItemStructFieldExt,
}

impl<P: Phase> ItemStruct<P> {
    pub fn name(&self) -> &TokenVariable {
        &self.name
    }

    pub fn fields(&self) -> &[ItemStructField<P>] {
        &self.fields
    }
}

impl Parse for ItemStruct<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(keyword_struct) = TokenKeyword::parse_keyword(tokens, &mut k, "struct")? else {
            return Ok(None);
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_struct_1"));
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_struct_2"));
        };

        let mut fields = vec![];
        while let Some(field) = ItemStructField::parse(tokens, &mut k)? {
            fields.push(field);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_struct_3"));
        };

        let item_struct = ItemStruct {
            keyword_struct,
            name,
            brace_l,
            fields,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(item_struct))
    }
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
            return Err(ParseError::Unknown("item_struct_field_1"));
        };

        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_struct_field_2"));
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FileIdGenerator, Token};

    #[test]
    fn test_parse_simple_struct() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#struct Vec3 {
    x: f32,
    y: f32,
    z: f32,
}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ItemStruct::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_item = result.unwrap();
        assert_eq!(struct_item.name.s(), "Vec3");
        assert_eq!(struct_item.fields.len(), 3);
        assert_eq!(struct_item.fields[0].name.s(), "x");
        assert_eq!(struct_item.fields[1].name.s(), "y");
        assert_eq!(struct_item.fields[2].name.s(), "z");
    }

    #[test]
    fn test_parse_struct_with_trailing_comma() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#struct Point2D {
    x: f32,
    y: f32,
}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ItemStruct::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_item = result.unwrap();
        assert_eq!(struct_item.name.s(), "Point2D");
        assert_eq!(struct_item.fields.len(), 2);
    }

    #[test]
    fn test_parse_empty_struct() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#struct Empty {
}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ItemStruct::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_item = result.unwrap();
        assert_eq!(struct_item.name.s(), "Empty");
        assert_eq!(struct_item.fields.len(), 0);
    }

    #[test]
    fn test_parse_struct_single_field() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#struct Counter {
    value: u64,
}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ItemStruct::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_item = result.unwrap();
        assert_eq!(struct_item.name.s(), "Counter");
        assert_eq!(struct_item.fields.len(), 1);
        assert_eq!(struct_item.fields[0].name.s(), "value");
    }
}
