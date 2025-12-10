//! Minimal temporary directory helper tailored for the Felis toolchain.

use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

static UNIQUE_COUNTER: AtomicU64 = AtomicU64::new(0);

/// A tiny replacement for `tempfile::TempDir` that stays under `/tmp/.felis-lang`.
pub struct TempDir {
    path: PathBuf,
}

impl TempDir {
    /// Create a new temporary directory with a deterministic, time-based name.
    pub fn new() -> std::io::Result<Self> {
        let base = PathBuf::from("/tmp/.felis-lang");
        fs::create_dir_all(&base)?;

        let dir_name = Self::generate_name()?;
        let dir_path = base.join(dir_name);
        fs::create_dir(&dir_path)?;

        Ok(Self { path: dir_path })
    }

    fn generate_name() -> std::io::Result<String> {
        let since_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(std::io::Error::other)?;
        let count = UNIQUE_COUNTER.fetch_add(1, Ordering::SeqCst);
        Ok(format!(".tmp{}-{count}", since_epoch.as_nanos()))
    }

    /// Access the directory path (mirrors `tempfile::TempDir::path`).
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl AsRef<Path> for TempDir {
    fn as_ref(&self) -> &Path {
        self.path()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}
