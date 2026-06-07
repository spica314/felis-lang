use super::{IfStatement, LetRefStatement, LetStatement, LoopStatement, Term};
use crate::syntax::Item;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(LetStatement),
    LetRef(LetRefStatement),
    If(IfStatement),
    Loop(LoopStatement),
    Break,
    Continue,
    Item(Box<Item>),
    Expression(Box<Term>),
}
