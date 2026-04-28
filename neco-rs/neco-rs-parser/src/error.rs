use std::fmt;
use std::path::PathBuf;

use crate::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error1 {
    pub path: Option<PathBuf>,
    pub span: Option<Span>,
    pub message: String,
}

impl Error1 {
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

impl fmt::Display for Error1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.path, self.span) {
            (Some(path), Some(span)) => write!(
                f,
                "{}:{}:{}..{}:{}: {}",
                path.display(),
                span.start.r,
                span.start.c,
                span.end.r,
                span.end.c,
                self.message
            ),
            (Some(path), None) => write!(f, "{}: {}", path.display(), self.message),
            (None, Some(span)) => write!(
                f,
                "{}:{}..{}:{}: {}",
                span.start.r, span.start.c, span.end.r, span.end.c, self.message
            ),
            (None, None) => write!(f, "{}", self.message),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    Error1(Error1),
    Message(String),
    MessageWithSpan(String, Span),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Error1(err) => err.fmt(f),
            Error::Message(err) => err.fmt(f),
            Error::MessageWithSpan(err, span) => {
                write!(
                    f,
                    "{}, at {}:{} - {}:{}",
                    err, span.start.r, span.start.c, span.end.r, span.end.c
                )
            }
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self::Error1(Error1::new(message))
    }

    pub(crate) fn with_path(self, path: impl Into<PathBuf>) -> Self {
        match self {
            Self::Error1(error) => Self::Error1(error.with_path(path)),
            Self::Message(message) => Self::Error1(Error1::new(message).with_path(path)),
            Self::MessageWithSpan(message, span) => Self::Error1(Error1 {
                path: Some(path.into()),
                span: Some(span),
                message,
            }),
        }
    }

    pub(crate) fn with_span(self, span: Span) -> Self {
        match self {
            Self::Error1(error) => Self::Error1(error.with_span(span)),
            Self::Message(message) => Self::Error1(Error1::new(message).with_span(span)),
            Self::MessageWithSpan(message, _) => Self::MessageWithSpan(message, span),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
