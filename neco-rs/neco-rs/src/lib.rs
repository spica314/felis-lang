use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

mod cli;
mod codegen;
mod effect;
mod ir;
mod linker;
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
    Link {
        status: Option<i32>,
        stderr: String,
    },
    RunTerminatedBySignal,
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
            Self::Link { status, stderr } => {
                write!(f, "native linker failed")?;
                if let Some(status) = status {
                    write!(f, " with status {status}")?;
                }
                let stderr = stderr.trim();
                if !stderr.is_empty() {
                    write!(f, ": {stderr}")?;
                }
                Ok(())
            }
            Self::RunTerminatedBySignal => f.write_str("run target terminated without exit status"),
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

pub fn run_cli<I>(args: I) -> Result<i32>
where
    I: IntoIterator<Item = String>,
{
    let options = cli::CliOptions::parse(args)?;
    let parsed = parse_root(&options.input)?;
    match options.command {
        cli::CliCommand::Build => {
            let (package, output) = match options.output {
                Some(output) => (cli::select_package_for_build(parsed, &output)?, output),
                None => {
                    let package = cli::select_package_for_default_build(parsed)?;
                    let output = cli::default_output_path(&package);
                    (package, output)
                }
            };
            compile_package_to_elf(&package, &output)?;
            Ok(0)
        }
        cli::CliCommand::Run => {
            let package = cli::select_package_for_run(parsed, options.binary_name.as_deref())?;
            let output = cli::default_output_path(&package);
            compile_package_to_elf(&package, &output)?;
            run_output(&output)
        }
        cli::CliCommand::Test => {
            for package in cli::select_packages_for_test(parsed) {
                let output = cli::test_output_path(&package);
                compile_package_to_elf(&package, &output)?;
                let status = run_output(&output)?;
                if status != 0 {
                    return Ok(status);
                }
            }
            Ok(0)
        }
    }
}

pub fn compile_path_to_elf(input: &Path, output: &Path) -> Result<()> {
    let parsed = parse_root(input)?;
    let package = cli::select_package_for_build(parsed, output)?;
    compile_package_to_elf(&package, output)
}

fn compile_package_to_elf(package: &neco_rs_parser::ParsedPackage, output: &Path) -> Result<()> {
    let program = lower_package_to_program(package)?;
    write_compiled_ptx_artifacts(package, &program)?;
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent).map_err(|source| Error::Io {
            path: Some(parent.to_path_buf()),
            source,
        })?;
    }
    match package.manifest.native_link_mode {
        neco_rs_parser::NativeLinkMode::KernelStart => {
            let elf = codegen::build_linux_x86_64_program_executable(&program).to_bytes()?;
            fs::write(output, elf).map_err(|source| Error::Io {
                path: Some(output.to_path_buf()),
                source,
            })?;
        }
        neco_rs_parser::NativeLinkMode::LibcStart => {
            let image =
                codegen::build_linux_x86_64_program_image(&program, codegen::EntryAbi::LibcMain);
            linker::link_linux_x86_64_libc_start_executable(
                &image,
                &package.manifest.native_libraries,
                output,
            )?;
        }
    }
    cli::set_output_executable(output)?;
    Ok(())
}

fn run_output(output: &Path) -> Result<i32> {
    const TEXT_FILE_BUSY: i32 = 26;
    const MAX_ATTEMPTS: usize = 20;

    let status = 'retry: {
        for attempt in 1..=MAX_ATTEMPTS {
            match Command::new(output).status() {
                Ok(status) => break 'retry status,
                Err(error)
                    if error.raw_os_error() == Some(TEXT_FILE_BUSY) && attempt < MAX_ATTEMPTS =>
                {
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(source) => {
                    return Err(Error::Io {
                        path: Some(output.to_path_buf()),
                        source,
                    });
                }
            }
        }
        unreachable!("run retry loop always returns or breaks")
    };

    status.code().ok_or(Error::RunTerminatedBySignal)
}

fn write_compiled_ptx_artifacts(
    package: &neco_rs_parser::ParsedPackage,
    program: &ir::LoweredProgram,
) -> Result<()> {
    if program.compiled_ptx.is_empty() {
        return Ok(());
    }

    let output_dir = package.root_dir.join(".neco");
    fs::create_dir_all(&output_dir).map_err(|source| Error::Io {
        path: Some(output_dir.clone()),
        source,
    })?;

    for artifact in &program.compiled_ptx {
        let mut bytes = program.data[artifact.data_index].as_slice();
        if bytes.ends_with(&[0]) {
            bytes = &bytes[..bytes.len() - 1];
        }
        let path = output_dir.join(format!("{}.ptx", artifact.function_name));
        fs::write(&path, bytes).map_err(|source| Error::Io {
            path: Some(path),
            source,
        })?;
    }

    Ok(())
}
