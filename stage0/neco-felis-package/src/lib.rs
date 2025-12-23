use neco_felis_syn::{ModuleTree, Phase};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Package<P: Phase> {
    pub name: String,
    pub module_tree: ModuleTree<P>,
}

impl<P: Phase> Package<P> {
    pub fn new(name: impl Into<String>, module_tree: ModuleTree<P>) -> Self {
        Self {
            name: name.into(),
            module_tree,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Packages<P: Phase> {
    pub items: BTreeMap<String, Package<P>>,
}

impl<P: Phase> Packages<P> {
    pub fn new(items: BTreeMap<String, Package<P>>) -> Self {
        Self { items }
    }
}
