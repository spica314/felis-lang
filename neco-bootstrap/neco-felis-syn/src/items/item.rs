use crate::{
    ItemDefinition, ItemEntrypoint, ItemInductive, ItemProc, ItemTheorem, ItemType, ItemUseBuiltin,
    Parse, ParseError, Phase, PhaseParse, token::Token,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item<P: Phase> {
    Inductive(ItemInductive<P>),
    Definition(ItemDefinition<P>),
    Theorem(ItemTheorem<P>),
    Entrypoint(ItemEntrypoint<P>),
    UseBuiltin(ItemUseBuiltin<P>),
    Proc(Box<ItemProc<P>>),
    Type(ItemType<P>),
}

impl Parse for Item<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(entrypoint) = ItemEntrypoint::parse(tokens, i)? {
            Ok(Some(Item::Entrypoint(entrypoint)))
        } else if let Some(use_builtin) = ItemUseBuiltin::parse(tokens, i)? {
            Ok(Some(Item::UseBuiltin(use_builtin)))
        } else if let Some(inductive) = ItemInductive::parse(tokens, i)? {
            Ok(Some(Item::Inductive(inductive)))
        } else if let Some(definition) = ItemDefinition::parse(tokens, i)? {
            Ok(Some(Item::Definition(definition)))
        } else if let Some(theorem) = ItemTheorem::parse(tokens, i)? {
            Ok(Some(Item::Theorem(theorem)))
        } else if let Some(proc) = ItemProc::parse(tokens, i)? {
            Ok(Some(Item::Proc(Box::new(proc))))
        } else if let Some(ty) = ItemType::parse(tokens, i)? {
            Ok(Some(Item::Type(ty)))
        } else {
            Ok(None)
        }
    }
}
