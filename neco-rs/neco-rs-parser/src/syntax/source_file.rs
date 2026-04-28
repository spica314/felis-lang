use crate::{Parse, Result, Token};

use super::Item;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub items: Vec<Item>,
}

impl Parse for SourceFile {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let mut items = Vec::new();
        while let Some(item) = Item::parse(tokens, &mut k)? {
            items.push(item);
        }

        *i = k;
        Ok(Some(Self { items }))
    }
}
