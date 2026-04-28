mod error;
mod lexer;
mod manifest;
mod parser;
mod root;
mod source;
mod syntax;

pub use error::{Error, Result};
pub use lexer::{Keyword, Span, Token, TokenKind};
pub use manifest::{Dependency, DependencySource, PackageManifest, WorkspaceManifest};
pub use parser::{Parse, Parser};
pub use root::{
    ParsedPackage, ParsedRoot, ParsedSourceFile, ParsedWorkspace, SourceFileRole, parse_root,
};
pub use source::parse_source;
pub use syntax::*;
