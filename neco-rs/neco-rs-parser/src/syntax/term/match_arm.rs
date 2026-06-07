use super::Term;
use crate::syntax::Pattern;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub result: Box<Term>,
}
