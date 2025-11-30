use crate::{Item, Parse, ParseError, Phase, PhaseParse, token::Token};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File<P: Phase> {
    pub items: Vec<Item<P>>,
    pub ext: P::FileExt,
}

impl<P: Phase> File<P> {
    pub fn items(&self) -> &[Item<P>] {
        &self.items
    }
}

impl Parse for File<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let mut items = vec![];
        while let Some(item) = Item::parse(tokens, &mut k)? {
            items.push(item);
        }

        let file = File { items, ext: () };

        *i = k;
        Ok(Some(file))
    }
}

#[cfg(test)]
mod test {
    use crate::{FileIdGenerator, token::Token};

    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn test_parse_inductive_eq() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s =
            std::fs::read_to_string("../../testcases/felis/single/wip/inductive_eq.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        let mut i = 0;
        let file = File::parse(&tokens, &mut i).unwrap().unwrap();

        assert_debug_snapshot!(file);
        assert_eq!(i, tokens.len());
    }

    #[test]
    fn test_parse_inductive_nat() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s =
            std::fs::read_to_string("../../testcases/felis/single/wip/inductive_nat.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        let mut i = 0;
        let file = File::parse(&tokens, &mut i).unwrap().unwrap();

        assert_debug_snapshot!(file);
        assert_eq!(i, tokens.len());
    }

    #[test]
    fn test_parse_eq_and_nat() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/wip/eq_and_nat.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        let mut i = 0;
        let file = File::parse(&tokens, &mut i).unwrap().unwrap();

        assert_debug_snapshot!(file);
        assert_eq!(i, tokens.len());
    }

    #[test]
    fn test_parse_syscall_group_exit_42() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/syscall_group_exit_42.fe")
            .unwrap();
        let tokens = Token::lex(&s, file_id);

        let mut i = 0;
        let file = File::parse(&tokens, &mut i).unwrap().unwrap();

        assert_debug_snapshot!(file);
        assert_eq!(i, tokens.len());
    }

    #[test]
    fn test_parse_let_mut() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/let_mut.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        let mut i = 0;
        let result = File::parse(&tokens, &mut i);

        match result {
            Ok(Some(file)) => {
                println!("Parsed successfully up to token {i} of {}", tokens.len());
                if i < tokens.len() {
                    println!("Remaining tokens: {:?}", &tokens[i..]);
                }
                assert_debug_snapshot!(file);
                assert_eq!(i, tokens.len());
            }
            Ok(None) => {
                println!("Parsing returned None at token {i} of {}", tokens.len());
                println!(
                    "Tokens around position: {:?}",
                    &tokens[i.saturating_sub(3)..tokens.len().min(i + 3)]
                );
                panic!("Parsing returned None");
            }
            Err(e) => {
                println!("Parse error at token {i} of {}: {:?}", tokens.len(), e);
                println!(
                    "Tokens around position: {:?}",
                    &tokens[i.saturating_sub(3)..tokens.len().min(i + 3)]
                );
                panic!("Parse error: {e:?}");
            }
        }
    }

    #[test]
    fn test_parse_struct_1() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/struct_1.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        let mut i = 0;
        let file = File::parse(&tokens, &mut i).unwrap().unwrap();

        assert_debug_snapshot!(file);
        assert_eq!(i, tokens.len());

        // Verify the struct was parsed correctly
        // Look for the struct item in the parsed items
        let mut found_struct = false;
        for item in &file.items {
            if let crate::Item::Struct(struct_item) = item {
                assert_eq!(struct_item.name().s(), "Vec3");
                assert_eq!(struct_item.fields().len(), 3);
                assert_eq!(struct_item.fields()[0].name.s(), "x");
                assert_eq!(struct_item.fields()[1].name.s(), "y");
                assert_eq!(struct_item.fields()[2].name.s(), "z");
                found_struct = true;
                break;
            }
        }
        assert!(found_struct, "Vec3 struct not found in parsed items");
    }

    #[test]
    fn test_parse_struct_2() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/struct_2.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        let mut i = 0;
        let file = File::parse(&tokens, &mut i).unwrap().unwrap();

        assert_debug_snapshot!(file);
        assert_eq!(i, tokens.len());

        // Verify the struct definition was parsed
        let mut found_struct = false;
        let mut found_proc = false;

        for item in &file.items {
            match item {
                crate::Item::Struct(struct_item) => {
                    assert_eq!(struct_item.name().s(), "Vec3");
                    assert_eq!(struct_item.fields().len(), 3);
                    assert_eq!(struct_item.fields()[0].name.s(), "x");
                    assert_eq!(struct_item.fields()[1].name.s(), "y");
                    assert_eq!(struct_item.fields()[2].name.s(), "z");
                    found_struct = true;
                }
                crate::Item::Proc(proc_item) => {
                    if proc_item.name.s() == "main" {
                        // Verify the proc contains a struct value creation
                        // The actual struct value creation is in the proc body
                        found_proc = true;
                    }
                }
                _ => {}
            }
        }

        assert!(found_struct, "Vec3 struct definition not found");
        assert!(found_proc, "main proc not found");
    }

    #[test]
    fn test_parse_array_len() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/array_len.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        println!("Total tokens: {}", tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            println!("{i}: {token:?}");
        }

        let mut i = 0;
        let result = File::parse(&tokens, &mut i);

        // Debug: Print status
        if i >= 44 {
            println!("Stopped at token {i}, trying to parse remaining tokens as ItemProc");
        }

        match result {
            Ok(Some(file)) => {
                println!("Parsed successfully up to token {i} of {}", tokens.len());
                if i < tokens.len() {
                    println!("Remaining tokens: {:?}", &tokens[i..]);
                }
                assert_debug_snapshot!(file);

                // Complete file parsing
                assert_eq!(i, tokens.len()); // Should have parsed all tokens
            }
            Ok(None) => {
                println!("Parsing returned None at token {i} of {}", tokens.len());
                println!(
                    "Tokens around position: {:?}",
                    &tokens[i.saturating_sub(3)..tokens.len().min(i + 3)]
                );
                panic!("Parsing returned None");
            }
            Err(e) => {
                println!("Parse error at token {i} of {}: {:?}", tokens.len(), e);
                println!(
                    "Tokens around position: {:?}",
                    &tokens[i.saturating_sub(3)..tokens.len().min(i + 3)]
                );
                panic!("Parse error: {e:?}");
            }
        }
    }

    #[test]
    fn test_parse_struct_3() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();
        let s = std::fs::read_to_string("../../testcases/felis/single/struct_3.fe").unwrap();
        let tokens = Token::lex(&s, file_id);

        println!("Total tokens for struct_3.fe: {}", tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            println!("{i}: {token:?}");
            if i >= 40 {
                println!("... (showing first 40 tokens)");
                break;
            }
        }

        let mut i = 0;
        let result = File::parse(&tokens, &mut i);

        match result {
            Ok(Some(file)) => {
                println!("Parsed successfully up to token {i} of {}", tokens.len());
                if i < tokens.len() {
                    println!("Remaining unparsed tokens: {:?}", &tokens[i..]);
                }
                assert_eq!(i, tokens.len(), "Failed to parse all tokens");

                // Verify the struct definition
                let mut found_struct = false;
                let mut found_proc = false;

                for item in &file.items {
                    match item {
                        crate::Item::Struct(struct_item) => {
                            assert_eq!(struct_item.name().s(), "Vec3");
                            assert_eq!(struct_item.fields().len(), 3);
                            found_struct = true;
                        }
                        crate::Item::Proc(proc_item) => {
                            if proc_item.name.s() == "main" {
                                found_proc = true;
                            }
                        }
                        _ => {}
                    }
                }

                assert!(found_struct, "Vec3 struct not found");
                assert!(found_proc, "main proc not found");
            }
            Ok(None) => {
                panic!("Parsing returned None at token {i}");
            }
            Err(e) => {
                println!("Parse error at token {i}: {e:?}");
                if i < tokens.len() {
                    println!("Token at error position: {:?}", tokens[i]);
                }
                panic!("Parse error: {e:?}");
            }
        }
    }
}
