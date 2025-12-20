#[derive(Debug, Clone)]
pub enum CompileError {
    UnsupportedConstruct(String),
    EntrypointNotFound,
    InvalidSyscall,
    NameResolution(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::UnsupportedConstruct(msg) => write!(f, "Unsupported construct: {msg}"),
            CompileError::EntrypointNotFound => write!(f, "Entrypoint not found"),
            CompileError::InvalidSyscall => write!(f, "Invalid syscall"),
            CompileError::NameResolution(msg) => write!(f, "Name resolution failed: {msg}"),
        }
    }
}

impl std::error::Error for CompileError {}
