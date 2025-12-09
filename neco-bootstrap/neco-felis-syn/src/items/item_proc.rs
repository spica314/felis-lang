use crate::{
    ItemProcBlock, Parse, ParseError, Phase, PhaseParse, Term,
    token::{Token, TokenColon, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemProc<P: Phase> {
    pub keyword_proc: TokenKeyword,
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub ty: Box<Term<P>>,
    pub proc_block: ItemProcBlock<P>,
    pub ext: P::ItemProcExt,
}

impl Parse for ItemProc<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(keyword_proc) = TokenKeyword::parse_keyword(tokens, &mut k, "proc")? else {
            return Ok(None);
        };

        // From this point, we have seen `#proc`, so failures should be errors
        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_proc_name"));
        };

        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_proc_colon"));
        };

        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_proc_type"));
        };

        let Some(proc_block) = ItemProcBlock::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_proc_block"));
        };

        let item_proc = ItemProc {
            keyword_proc,
            name,
            colon,
            ty: Box::new(ty),
            proc_block,
            ext: (),
        };
        *i = k;
        Ok(Some(item_proc))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{FileIdGenerator, Token};

    #[test]
    fn debug_parse_item_proc_in_struct_3() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/type_3.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        // find first 'proc'
        let mut start = 0;
        for (idx, tok) in tokens.iter().enumerate() {
            if let crate::token::Token::Keyword(k) = tok
                && k.s() == "proc"
            {
                start = idx;
                break;
            }
        }

        // Find the opening brace after the proc signature
        let mut brace_idx = start;
        while brace_idx < tokens.len() {
            if matches!(tokens[brace_idx], crate::token::Token::BraceL(_)) {
                break;
            }
            brace_idx += 1;
        }
        assert!(
            brace_idx < tokens.len(),
            "Could not find opening brace for proc block"
        );

        // Try parsing the first statement inside the block
        let mut i = brace_idx + 1;
        if let Some(_stmt) = crate::Statement::parse(&tokens, &mut i).unwrap() {
            println!("Parsed first statement in proc block, advanced to token index {i}");
            // Expect a semicolon next
            let semi = crate::token::TokenSemicolon::parse(&tokens, &mut i).unwrap();
            assert!(semi.is_some(), "Expected semicolon after first statement");
        } else {
            panic!("Failed to parse first statement at index {}", brace_idx + 1);
        }

        // Now parse full statements list and report where it ends
        let mut j = brace_idx + 1;
        let _stmts = crate::Statements::parse(&tokens, &mut j).unwrap().unwrap();
        println!("Parsed full statements list; index now {j}");
        // Next token ideally should be a BraceR, but print it either way for debugging
        println!("Token at {j}: {:?}", &tokens[j]);

        // Additionally, try to parse the second let statement directly
        let mut k2 = i; // i was after first semicolon
        if let Some(_stmt2) = crate::Statement::parse(&tokens, &mut k2).unwrap() {
            println!("Parsed second statement; advanced to token index {k2}");
            let semi2 = crate::token::TokenSemicolon::parse(&tokens, &mut k2).unwrap();
            assert!(semi2.is_some(), "Expected semicolon after second statement");
        } else {
            panic!("Failed to parse second statement at index {i}");
        }
    }
}
