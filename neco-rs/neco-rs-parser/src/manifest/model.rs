use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageManifest {
    pub name: String,
    pub dependencies: Vec<Dependency>,
    pub felis_lib_entrypoint: Option<PathBuf>,
    pub felis_bin_entrypoints: Vec<PathBuf>,
    pub felis_test_entrypoints: Vec<PathBuf>,
    pub native_link_mode: NativeLinkMode,
    pub native_libraries: Vec<String>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeLinkMode {
    KernelStart,
    LibcStart,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceManifest {
    pub members: Vec<PathBuf>,
}

pub(crate) enum Manifest {
    Package(PackageManifest),
    Workspace(WorkspaceManifest),
}
