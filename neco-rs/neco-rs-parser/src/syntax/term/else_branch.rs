use super::{Block, IfStatement};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElseBranch {
    Block(Block),
    If(Box<IfStatement>),
}
