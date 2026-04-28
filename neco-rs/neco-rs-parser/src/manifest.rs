mod model;
pub(crate) mod parse;

#[cfg(test)]
mod tests;

pub(crate) use model::Manifest;
pub use model::{Dependency, DependencySource, PackageManifest, WorkspaceManifest};
pub(crate) use parse::parse_manifest;
