use super::Block;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopStatement {
    pub body: Block,
}
