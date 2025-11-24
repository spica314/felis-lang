use crate::{Type, TypeHole};
use neco_felis_elaboration::{NameId, TermId};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnificationError {
    TypeMismatch { expected: Type, actual: Type },
    Occurs { hole: TypeHole, ty: Type },
}

impl fmt::Display for UnificationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnificationError::TypeMismatch { expected, actual } => {
                write!(f, "type mismatch: expected {expected}, got {actual}")
            }
            UnificationError::Occurs { hole, ty } => {
                write!(f, "occurs check failed: hole {hole:?} appears in {ty}")
            }
        }
    }
}

impl std::error::Error for UnificationError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypingError {
    MissingBuiltinType { builtin: String },
    DuplicateBinding { name_id: NameId },
    UnboundName { name_id: NameId },
    UnificationFailed(UnificationError),
    Unsupported(String),
    UnknownTermId(TermId),
}

impl fmt::Display for TypingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypingError::MissingBuiltinType { builtin } => {
                write!(f, "type for builtin '{builtin}' is not provided")
            }
            TypingError::DuplicateBinding { name_id } => {
                write!(f, "name already bound: {name_id:?}")
            }
            TypingError::UnboundName { name_id } => write!(f, "unbound name: {name_id:?}"),
            TypingError::UnificationFailed(err) => err.fmt(f),
            TypingError::Unsupported(msg) => write!(f, "unsupported typing feature: {msg}"),
            TypingError::UnknownTermId(term_id) => write!(f, "unknown term id {term_id:?}"),
        }
    }
}

impl std::error::Error for TypingError {}
