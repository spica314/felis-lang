use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use neco_rs_elf::{Elf64Executable, ElfMachine, LoadSegment, SegmentFlags};
use neco_rs_parser::{Item, LetOperator, ParsedPackage, ParsedRoot, Statement, Term, parse_root};

#[derive(Debug)]
pub enum Error {
    Usage(String),
    Io {
        path: Option<PathBuf>,
        source: std::io::Error,
    },
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

pub type Result<T> = std::result::Result<T, Error>;

pub fn run_cli<I>(args: I) -> Result<()>
where
    I: IntoIterator<Item = String>,
{
    let options = CliOptions::parse(args)?;
    compile_path_to_elf(&options.input, &options.output)
}

pub fn compile_path_to_elf(input: &Path, output: &Path) -> Result<()> {
    let parsed = parse_root(input)?;
    let package = match parsed {
        ParsedRoot::Package(package) => package,
        ParsedRoot::Workspace(_) => {
            return Err(Error::Unsupported(
                "workspace root is not supported yet; pass a package root".to_string(),
            ));
        }
    };

    let exit_code = extract_exit_code(&package)?;
    let elf = build_linux_x86_64_exit_executable(exit_code).to_bytes();
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
    Ok(())
}

struct CliOptions {
    input: PathBuf,
    output: PathBuf,
}

impl CliOptions {
    fn parse<I>(args: I) -> Result<Self>
    where
        I: IntoIterator<Item = String>,
    {
        let mut args = args.into_iter();
        let _program = args.next();

        let mut input = None;
        let mut output = None;

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "build" => {}
                "-o" | "--output" => {
                    let Some(path) = args.next() else {
                        return Err(Error::Usage("missing value for --output".to_string()));
                    };
                    output = Some(PathBuf::from(path));
                }
                "--help" | "-h" => {
                    return Err(Error::Usage(
                        "usage: neco-rs build <package-root> [-o <output-elf>]".to_string(),
                    ));
                }
                _ if arg.starts_with('-') => {
                    return Err(Error::Usage(format!("unknown option `{arg}`")));
                }
                _ => {
                    if input.is_some() {
                        return Err(Error::Usage(
                            "expected a single package root path".to_string(),
                        ));
                    }
                    input = Some(PathBuf::from(arg));
                }
            }
        }

        let input = input.ok_or_else(|| Error::Usage("missing package root path".to_string()))?;
        let output = output.unwrap_or_else(|| default_output_path(&input));

        Ok(Self { input, output })
    }
}

fn default_output_path(input: &Path) -> PathBuf {
    let package_root = if input
        .file_name()
        .is_some_and(|name| name == std::ffi::OsStr::new("neco-package.json"))
    {
        input.parent().unwrap_or(input)
    } else {
        input
    };

    let name = package_root
        .file_name()
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| std::ffi::OsStr::new("a.out"));

    package_root.join(".neco").join(name)
}

fn extract_exit_code(package: &ParsedPackage) -> Result<i32> {
    let entrypoint_name = package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
        .find_map(|item| match item {
            Item::EntryPoint(entrypoint) => Some(entrypoint.name.as_str()),
            _ => None,
        })
        .ok_or_else(|| Error::Unsupported("missing #entrypoint declaration".to_string()))?;

    let main_fn = package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
        .find_map(|item| match item {
            Item::Function(function) if function.name.name == entrypoint_name => Some(function),
            _ => None,
        })
        .ok_or_else(|| {
            Error::Unsupported(format!(
                "missing function body for entrypoint `{entrypoint_name}`"
            ))
        })?;

    if main_fn.body.statements.len() != 1 || main_fn.body.tail.is_none() {
        return Err(Error::Unsupported(
            "only a single `#let _ <- IO::exit <code>;` statement followed by `()` is supported"
                .to_string(),
        ));
    }

    let Statement::Let(let_stmt) = &main_fn.body.statements[0] else {
        return Err(Error::Unsupported(
            "entrypoint body must start with a let statement".to_string(),
        ));
    };

    if let_stmt.operator != LetOperator::LeftArrow {
        return Err(Error::Unsupported(
            "entrypoint let statement must use `<-`".to_string(),
        ));
    }

    match main_fn.body.tail.as_deref() {
        Some(Term::Unit) => {}
        _ => {
            return Err(Error::Unsupported(
                "entrypoint body must end with `()`".to_string(),
            ));
        }
    }

    let Term::Application { callee, arguments } = let_stmt.value.as_ref() else {
        return Err(Error::Unsupported(
            "entrypoint let statement must call `IO::exit`".to_string(),
        ));
    };

    let Term::Path(path) = callee.as_ref() else {
        return Err(Error::Unsupported(
            "entrypoint let statement must call a path expression".to_string(),
        ));
    };

    let callee_segments: Vec<_> = path
        .segments
        .iter()
        .map(|segment| segment.name.as_str())
        .collect();
    if callee_segments != ["IO", "exit"] {
        return Err(Error::Unsupported(format!(
            "unsupported entrypoint call `{}; expected IO::exit`",
            callee_segments.join("::")
        )));
    }

    parse_exit_code_arguments(arguments)
}

fn parse_exit_code_arguments(arguments: &[Term]) -> Result<i32> {
    match arguments {
        [value] => parse_i32_literal(value),
        [value, suffix] if is_i32_suffix(suffix) => parse_bare_integer_literal(value),
        _ => Err(Error::Unsupported(
            "`IO::exit` must receive a single `i32` literal argument".to_string(),
        )),
    }
}

fn parse_i32_literal(term: &Term) -> Result<i32> {
    let Term::IntegerLiteral(literal) = term else {
        return Err(Error::Unsupported(
            "exit code must be an integer literal".to_string(),
        ));
    };

    let digits = literal
        .strip_suffix("i32")
        .ok_or_else(|| Error::Unsupported("exit code must use the `i32` suffix".to_string()))?;
    digits.parse::<i32>().map_err(|_| {
        Error::Unsupported(format!(
            "exit code literal `{literal}` could not be parsed as i32"
        ))
    })
}

fn parse_bare_integer_literal(term: &Term) -> Result<i32> {
    let Term::IntegerLiteral(literal) = term else {
        return Err(Error::Unsupported(
            "exit code must be an integer literal".to_string(),
        ));
    };

    literal.parse::<i32>().map_err(|_| {
        Error::Unsupported(format!(
            "exit code literal `{literal}` could not be parsed as i32"
        ))
    })
}

fn is_i32_suffix(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "i32",
        _ => false,
    }
}

fn build_linux_x86_64_exit_executable(exit_code: i32) -> Elf64Executable {
    let entry = 0x401000;
    let mut elf = Elf64Executable::new(ElfMachine::X86_64, entry);
    elf.add_load_segment(LoadSegment::new(
        entry,
        0x1000,
        SegmentFlags::READ_EXECUTE,
        exit_syscall_code(exit_code),
    ));
    elf
}

fn exit_syscall_code(exit_code: i32) -> Vec<u8> {
    let mut code = Vec::with_capacity(12);
    code.extend_from_slice(&[0xb8, 0x3c, 0x00, 0x00, 0x00]);
    code.push(0xbf);
    code.extend_from_slice(&exit_code.to_le_bytes());
    code.extend_from_slice(&[0x0f, 0x05]);
    code
}

#[cfg(test)]
mod tests {
    use super::*;

    fn repo_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root")
    }

    #[test]
    fn extracts_exit_code_from_fixture() {
        let root = repo_root().join("tests/testcases/exit-42");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let exit_code = extract_exit_code(&package).expect("extract exit code");
        assert_eq!(exit_code, 42);
    }

    #[test]
    fn builds_elf_image_with_exit_syscall() {
        let elf = build_linux_x86_64_exit_executable(42).to_bytes();
        assert_eq!(&elf[0..4], b"\x7FELF");
        assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1005..0x100a], &[0xbf, 42, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x100a..0x100c], &[0x0f, 0x05]);
    }

    #[test]
    fn defaults_output_into_package_neco_directory() {
        let root = repo_root().join("tests/testcases/exit-42");
        let output = default_output_path(&root);
        assert_eq!(output, root.join(".neco").join("exit-42"));
    }
}
