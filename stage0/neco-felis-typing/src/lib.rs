//! Typing phase that runs after resolve.
//!
//! This crate focuses on two responsibilities:
//! - describing Felis types with wildcard holes that can be solved gradually
//! - walking a resolved file to associate each `TermId` with a (possibly
//!   partial) type, while letting callers plug in builtin types.

mod builtin;
mod context;
mod error;
mod types;
mod unify;

pub use builtin::BuiltinTypes;
pub use context::{TypeChecker, TypingResult};
pub use error::{TypingError, UnificationError};
pub use types::{IntegerType, StructFieldType, Term, TypeHole};
pub use unify::{TypeSolutions, UnificationCtx};
