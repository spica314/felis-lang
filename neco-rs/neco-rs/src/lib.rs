use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

mod cli;
mod codegen;
mod effect;
mod ir;
mod lowering;
#[cfg(test)]
mod tests;

use lowering::lower_package_to_program;
use neco_rs_parser::parse_root;

#[derive(Debug)]
pub enum Error {
    Usage(String),
    Io {
        path: Option<PathBuf>,
        source: std::io::Error,
    },
    Elf(neco_rs_elf::Error),
    Parse(neco_rs_parser::Error),
    Unsupported(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Usage(message) | Self::Unsupported(message) => f.write_str(message),
            Self::Io {
                path: Some(path),
                source,
            } => write!(f, "{}: {}", path.display(), source),
            Self::Io { path: None, source } => source.fmt(f),
            Self::Elf(source) => source.fmt(f),
            Self::Parse(source) => source.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl From<neco_rs_parser::Error> for Error {
    fn from(value: neco_rs_parser::Error) -> Self {
        Self::Parse(value)
    }
}

impl From<neco_rs_elf::Error> for Error {
    fn from(value: neco_rs_elf::Error) -> Self {
        Self::Elf(value)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn run_cli<I>(args: I) -> Result<()>
where
    I: IntoIterator<Item = String>,
{
    let options = cli::CliOptions::parse(args)?;
    compile_path_to_elf(&options.input, &options.output)
}

pub fn compile_path_to_elf(input: &Path, output: &Path) -> Result<()> {
    let parsed = parse_root(input)?;
    let package = cli::select_package_for_build(parsed, output)?;

    let program = lower_package_to_program(&package)?;
    let elf = codegen::build_linux_x86_64_program_executable(&program).to_bytes()?;
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent).map_err(|source| Error::Io {
            path: Some(parent.to_path_buf()),
            source,
        })?;
    }
    fs::write(output, elf).map_err(|source| Error::Io {
        path: Some(output.to_path_buf()),
        source,
    })?;
    cli::set_output_executable(output)?;
    Ok(())
}
