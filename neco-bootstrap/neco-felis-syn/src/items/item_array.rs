use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{TokenBraceL, TokenBraceR, TokenColon, TokenComma, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemArray<P: Phase> {
    pub keyword_array: TokenKeyword,
    pub name: TokenVariable,
    pub brace_l: TokenBraceL,
    pub fields: Vec<ItemArrayField<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemArrayExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemArrayField<P: Phase> {
    pub keyword: TokenKeyword,
    pub colon: Option<TokenColon>,
    pub value: Box<Term<P>>,
    pub comma: Option<TokenComma>,
}

impl<P: Phase> ItemArray<P> {
    pub fn name(&self) -> &TokenVariable {
        &self.name
    }

    pub fn fields(&self) -> &[ItemArrayField<P>] {
        &self.fields
    }
}

impl Parse for ItemArray<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(keyword_array) = TokenKeyword::parse_keyword(tokens, &mut k, "array")? else {
            return Ok(None);
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_array_1"));
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_array_2"));
        };

        let mut fields = vec![];
        while let Some(field) = ItemArrayField::parse(tokens, &mut k)? {
            fields.push(field);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_array_3"));
        };

        let item_array = ItemArray {
            keyword_array,
            name,
            brace_l,
            fields,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(item_array))
    }
}

impl Parse for ItemArrayField<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        // Try to parse any of the known array field keywords
        // Supported:
        // - #item: <type>
        // - #dimension: <number>
        // - #members { x: T, y: U, }
        if let Some(keyword) = TokenKeyword::parse_keyword(tokens, &mut k, "item")? {
            let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("item_array_field_1"));
            };

            let Some(value) = Term::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("item_array_field_2"));
            };

            let comma = TokenComma::parse(tokens, &mut k)?;

            let field = ItemArrayField {
                keyword,
                colon: Some(colon),
                value: Box::new(value),
                comma,
            };

            *i = k;
            return Ok(Some(field));
        }

        if let Some(keyword) = TokenKeyword::parse_keyword(tokens, &mut k, "dimension")? {
            let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("item_array_field_1"));
            };

            let Some(value) = Term::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("item_array_field_2"));
            };

            let comma = TokenComma::parse(tokens, &mut k)?;

            let field = ItemArrayField {
                keyword,
                colon: Some(colon),
                value: Box::new(value),
                comma,
            };

            *i = k;
            return Ok(Some(field));
        }

        // New syntax: #members { ... }
        if let Some(keyword) = TokenKeyword::parse_keyword(tokens, &mut k, "members")? {
            let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("item_array_members_1"));
            };

            // Reuse TermStructField parsing for members inside braces
            let mut fields = vec![];
            while let Some(field) =
                crate::terms::term_struct::TermStructField::parse(tokens, &mut k)?
            {
                fields.push(field);
            }

            let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("item_array_members_2"));
            };

            // Synthesize a Term::Struct to keep downstream unchanged
            let term_struct = crate::terms::term_struct::TermStruct {
                keyword_struct: TokenKeyword::new(keyword.pos().clone(), "struct"),
                brace_l,
                fields,
                brace_r,
                ext: (),
            };

            let value = Term::Struct(term_struct);
            let comma = TokenComma::parse(tokens, &mut k)?;

            let field = ItemArrayField {
                keyword,
                colon: None,
                value: Box::new(value),
                comma,
            };

            *i = k;
            return Ok(Some(field));
        }

        // Unknown field
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FileIdGenerator, Token};

    #[test]
    fn test_parse_simple_array() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#array Points {
    #item: f32,
}"#;
        let tokens = Token::lex(s, file_id);
        println!("Tokens: {tokens:?}");

        let mut i = 0;
        let result = ItemArray::parse(&tokens, &mut i);

        match result {
            Ok(Some(array)) => {
                assert_eq!(array.name.s(), "Points");
                assert_eq!(array.fields.len(), 1);
            }
            Ok(None) => {
                panic!("Parser returned None, expected array");
            }
            Err(e) => {
                println!("Parse error at token {i}: {e:?}");
                println!(
                    "Tokens around position: {:?}",
                    &tokens[i.saturating_sub(3)..tokens.len().min(i + 3)]
                );
                panic!("Parse error: {e:?}");
            }
        }
    }

    #[test]
    fn test_parse_array_with_struct_item() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#array Points {
    #item: #struct {
        x: f32,
        y: f32,
    },
    #dimension: 1,
}"#;
        let tokens = Token::lex(s, file_id);
        println!("Tokens: {tokens:?}");

        let mut i = 0;
        let result = ItemArray::parse(&tokens, &mut i);

        match result {
            Ok(Some(array)) => {
                assert_eq!(array.name.s(), "Points");
                assert_eq!(array.fields.len(), 2);
            }
            Ok(None) => {
                panic!("Parser returned None, expected array");
            }
            Err(e) => {
                println!("Parse error at token {i}: {e:?}");
                println!(
                    "Tokens around position: {:?}",
                    &tokens[i.saturating_sub(3)..tokens.len().min(i + 3)]
                );
                panic!("Parse error: {e:?}");
            }
        }
    }

    #[test]
    fn test_parse_array_fails_without_name() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#array {
    #item: f32,
}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ItemArray::parse(&tokens, &mut i);

        assert!(result.is_err());
    }
}
