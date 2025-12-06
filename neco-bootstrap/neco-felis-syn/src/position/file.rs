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
