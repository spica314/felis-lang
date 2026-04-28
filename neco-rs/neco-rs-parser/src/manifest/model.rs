use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageManifest {
    pub name: String,
    pub dependencies: Vec<Dependency>,
    pub felis_lib_entrypoint: Option<PathBuf>,
    pub felis_bin_entrypoints: Vec<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dependency {
    pub name: String,
    pub source: DependencySource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencySource {
    Workspace,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceManifest {
    pub members: Vec<PathBuf>,
}

pub(crate) enum Manifest {
    Package(PackageManifest),
    Workspace(WorkspaceManifest),
}
