use neco_felis_syn::{ModuleTree, Phase};

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
