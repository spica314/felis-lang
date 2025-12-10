use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{TokenBraceL, TokenBraceR, TokenColon, TokenComma, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermStruct<P: Phase> {
    pub keyword_struct: TokenKeyword,
    pub brace_l: TokenBraceL,
    pub fields: Vec<TermStructField<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::TermStructExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermStructField<P: Phase> {
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub ty: Box<Term<P>>,
    pub comma: Option<TokenComma>,
}

impl<P: Phase> TermStruct<P> {
    pub fn fields(&self) -> &[TermStructField<P>] {
        &self.fields
    }
}

impl Parse for TermStruct<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(keyword_struct) = TokenKeyword::parse_keyword(tokens, &mut k, "struct")? else {
            return Ok(None);
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("term_struct_1"));
        };

        let mut fields = vec![];
        while let Some(field) = TermStructField::parse(tokens, &mut k)? {
            fields.push(field);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("term_struct_2"));
        };

        let term_struct = TermStruct {
            keyword_struct,
            brace_l,
            fields,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(term_struct))
    }
}

impl Parse for TermStructField<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("term_struct_field_1"));
        };

        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("term_struct_field_2"));
        };

        let comma = TokenComma::parse(tokens, &mut k)?;

        let field = TermStructField {
            name,
            colon,
            ty: Box::new(ty),
            comma,
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
    fn test_parse_anonymous_struct() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#struct {
    x: f32,
    y: f32,
}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = TermStruct::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_term = result.unwrap();
        assert_eq!(struct_term.fields.len(), 2);
        assert_eq!(struct_term.fields[0].name.s(), "x");
        assert_eq!(struct_term.fields[1].name.s(), "y");
    }

    #[test]
    fn test_parse_empty_struct() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"#struct {}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = TermStruct::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_term = result.unwrap();
        assert_eq!(struct_term.fields.len(), 0);
    }
}
