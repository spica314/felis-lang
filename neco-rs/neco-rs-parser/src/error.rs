use std::fmt;
use std::path::PathBuf;

use crate::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub path: Option<PathBuf>,
    pub span: Option<Span>,
    pub message: String,
}

impl Error {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self {
            path: None,
            span: None,
            message: message.into(),
        }
    }

    pub(crate) fn with_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub(crate) fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.path, self.span) {
            (Some(path), Some(span)) => write!(
                f,
                "{}:{}..{}: {}",
                path.display(),
                span.start,
                span.end,
                self.message
            ),
            (Some(path), None) => write!(f, "{}: {}", path.display(), self.message),
            (None, Some(span)) => write!(f, "{}..{}: {}", span.start, span.end, self.message),
            (None, None) => write!(f, "{}", self.message),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
