use crate::{Parse, Parser, Result};

use super::Item;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub items: Vec<Item>,
}

impl Parse for SourceFile {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let mut items = Vec::new();
        while !parser.at_end() {
            items.push(Item::parse(parser)?.unwrap());
        }
        Ok(Some(Self { items }))
    }
}
