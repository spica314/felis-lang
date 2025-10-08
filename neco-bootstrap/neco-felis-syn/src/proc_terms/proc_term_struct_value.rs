use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm,
    token::{Token, TokenBraceL, TokenBraceR, TokenColon, TokenComma, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermStructValue<P: Phase> {
    pub struct_name: TokenVariable,
    pub brace_l: TokenBraceL,
    pub fields: Vec<ProcTermStructField<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ProcTermStructValueExt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermStructField<P: Phase> {
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub value: Box<ProcTerm<P>>,
    pub comma: TokenComma,
}

impl Parse for ProcTermStructValue<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse struct name (e.g., Vec3)
        let Some(struct_name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Check if the next token is a brace (to distinguish from other uses of variables)
        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Parse fields
        let mut fields = vec![];
        loop {
            // Try to parse a field
            let field_k = k;

            // Parse field name
            let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
                // No more fields
                break;
            };

            // Parse colon
            let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
                // Not a field, reset and break
                k = field_k;
                break;
            };

            // Parse field value
            let Some(value) = ProcTerm::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("proc_term_struct_value_field_value"));
            };

            // Optional comma
            let Some(comma) = TokenComma::parse(tokens, &mut k)? else {
                return Err(ParseError::Unknown("expected , after ProcTerm"));
            };

            fields.push(ProcTermStructField {
                name,
                colon,
                value: Box::new(value),
                comma: comma.clone(),
            });
        }

        // Parse closing brace
        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("proc_term_struct_value_brace_r"));
        };

        let proc_term_struct_value = ProcTermStructValue {
            struct_name,
            brace_l,
            fields,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(proc_term_struct_value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::FileIdGenerator;

    #[test]
    fn test_parse_struct_value() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"Vec3 {
            x: 18,
            y: 14,
            z: 10,
        }"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ProcTermStructValue::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_value = result.unwrap();
        assert_eq!(struct_value.struct_name.s(), "Vec3");
        assert_eq!(struct_value.fields.len(), 3);
        assert_eq!(struct_value.fields[0].name.s(), "x");
        assert_eq!(struct_value.fields[1].name.s(), "y");
        assert_eq!(struct_value.fields[2].name.s(), "z");
    }

    #[test]
    fn test_parse_empty_struct_value() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"Empty {}"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ProcTermStructValue::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_value = result.unwrap();
        assert_eq!(struct_value.struct_name.s(), "Empty");
        assert_eq!(struct_value.fields.len(), 0);
    }

    #[test]
    fn test_parse_struct_value_from_struct_2_fe() {
        // Test the exact syntax from struct_2.fe
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = r#"Vec3 {
        x: 18,
        y: 14,
        z: 10,
    }"#;
        let tokens = Token::lex(s, file_id);

        let mut i = 0;
        let result = ProcTermStructValue::parse(&tokens, &mut i).unwrap();

        assert!(result.is_some());
        let struct_value = result.unwrap();

        // Verify struct name
        assert_eq!(struct_value.struct_name.s(), "Vec3");

        // Verify fields
        assert_eq!(struct_value.fields.len(), 3);

        // Verify field names
        assert_eq!(struct_value.fields[0].name.s(), "x");
        assert_eq!(struct_value.fields[1].name.s(), "y");
        assert_eq!(struct_value.fields[2].name.s(), "z");

        // Verify field values are numbers
        match &*struct_value.fields[0].value {
            ProcTerm::Number(num) => assert_eq!(num.number.s(), "18"),
            _ => panic!("Expected number for x field"),
        }
        match &*struct_value.fields[1].value {
            ProcTerm::Number(num) => assert_eq!(num.number.s(), "14"),
            _ => panic!("Expected number for y field"),
        }
        match &*struct_value.fields[2].value {
            ProcTerm::Number(num) => assert_eq!(num.number.s(), "10"),
            _ => panic!("Expected number for z field"),
        }
    }
}
