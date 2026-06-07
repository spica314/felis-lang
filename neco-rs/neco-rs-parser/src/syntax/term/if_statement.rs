use super::{Block, ElseBranch, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement {
    pub condition: Box<Term>,
    pub then_block: Block,
    pub else_branch: Option<ElseBranch>,
}
