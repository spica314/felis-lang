use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

mod effect;

use effect::{Value, bind_pattern, lower_effect, resolve_value};
use neco_rs_elf::{Elf64Executable, LoadSegment, SegmentFlags};
use neco_rs_parser::{Item, ParsedPackage, ParsedRoot, Statement, Term, parse_root};

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

    let program = lower_package_to_program(&package)?;
    let elf = build_linux_x86_64_program_executable(&program).to_bytes();
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct LoweredProgram {
    operations: Vec<Operation>,
    data: Vec<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Operation {
    Write { fd: u32, data_index: usize },
    Exit(i32),
}

fn lower_package_to_program(package: &ParsedPackage) -> Result<LoweredProgram> {
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

    if !matches!(main_fn.body.tail.as_deref(), Some(Term::Unit)) {
        return Err(Error::Unsupported(
            "entrypoint body must end with `()`".to_string(),
        ));
    }

    let mut program = LoweredProgram {
        operations: Vec::new(),
        data: Vec::new(),
    };
    let mut environment = HashMap::new();
    let mut terminated = false;

    for statement in &main_fn.body.statements {
        if terminated {
            return Err(Error::Unsupported(
                "statements after `IO::exit` are not supported".to_string(),
            ));
        }

        terminated = lower_statement(statement, &mut environment, &mut program)?;
    }

    if !terminated {
        program.operations.push(Operation::Exit(0));
    }

    Ok(program)
}

fn lower_statement(
    statement: &Statement,
    environment: &mut HashMap<String, Value>,
    program: &mut LoweredProgram,
) -> Result<bool> {
    let Statement::Let(let_stmt) = statement else {
        return Err(Error::Unsupported(
            "only let statements are supported in entrypoint bodies".to_string(),
        ));
    };

    match let_stmt.operator {
        neco_rs_parser::LetOperator::Equals => {
            let value = lower_pure_value(let_stmt.value.as_ref(), environment, program)?;
            bind_pattern(&let_stmt.binder, value, environment);
            Ok(false)
        }
        neco_rs_parser::LetOperator::LeftArrow => lower_effect(
            &let_stmt.binder,
            let_stmt.value.as_ref(),
            environment,
            program,
        ),
    }
}

fn lower_pure_value(
    term: &Term,
    environment: &HashMap<String, Value>,
    program: &mut LoweredProgram,
) -> Result<Value> {
    match term {
        Term::StringLiteral(literal) => {
            let data_index = intern_data(program, literal.as_bytes().to_vec());
            Ok(Value::ByteString(data_index))
        }
        Term::MethodCall { receiver, method } if method == "as_bytes" => {
            match resolve_value(receiver.as_ref(), environment)? {
                Value::ByteString(data_index) => Ok(Value::ByteString(data_index)),
                other => Err(Error::Unsupported(format!(
                    "`as_bytes` expects a string reference, got {other:?}"
                ))),
            }
        }
        Term::Path(_) => resolve_value(term, environment),
        _ => Err(Error::Unsupported(format!(
            "unsupported pure expression in entrypoint body: {term:?}"
        ))),
    }
}

fn intern_data(program: &mut LoweredProgram, bytes: Vec<u8>) -> usize {
    program.data.push(bytes);
    program.data.len() - 1
}

fn build_linux_x86_64_program_executable(program: &LoweredProgram) -> Elf64Executable {
    let code_virtual_address = 0x401000;
    let data_virtual_address = 0x402000;
    let mut elf = Elf64Executable::new(code_virtual_address);
    elf.add_load_segment(LoadSegment::new(
        code_virtual_address,
        0x1000,
        SegmentFlags::READ_EXECUTE,
        program_syscall_code(program, data_virtual_address),
    ));
    if !program.data.is_empty() {
        elf.add_load_segment(LoadSegment::new(
            data_virtual_address,
            0x1000,
            SegmentFlags::READ_ONLY,
            flatten_data(program),
        ));
    }
    elf
}

fn flatten_data(program: &LoweredProgram) -> Vec<u8> {
    let total_len = program.data.iter().map(Vec::len).sum();
    let mut data = Vec::with_capacity(total_len);
    for bytes in &program.data {
        data.extend_from_slice(bytes);
    }
    data
}

fn data_addresses(program: &LoweredProgram, data_virtual_address: u64) -> Vec<u64> {
    let mut next_address = data_virtual_address;
    let mut addresses = Vec::with_capacity(program.data.len());
    for bytes in &program.data {
        addresses.push(next_address);
        next_address += bytes.len() as u64;
    }
    addresses
}

fn program_syscall_code(program: &LoweredProgram, data_virtual_address: u64) -> Vec<u8> {
    let addresses = data_addresses(program, data_virtual_address);
    let mut code = Vec::new();

    for operation in &program.operations {
        match *operation {
            Operation::Write { fd, data_index } => {
                let bytes = &program.data[data_index];
                code.extend_from_slice(&[0xb8, 0x01, 0x00, 0x00, 0x00]);
                code.push(0xbf);
                code.extend_from_slice(&fd.to_le_bytes());
                code.extend_from_slice(&[0x48, 0xbe]);
                code.extend_from_slice(&addresses[data_index].to_le_bytes());
                code.push(0xba);
                code.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::Exit(exit_code) => {
                code.extend_from_slice(&[0xb8, 0x3c, 0x00, 0x00, 0x00]);
                code.push(0xbf);
                code.extend_from_slice(&exit_code.to_le_bytes());
                code.extend_from_slice(&[0x0f, 0x05]);
            }
        }
    }

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
    fn lowers_exit_fixture_to_program() {
        let root = repo_root().join("tests/testcases/exit-42");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(program.operations, vec![Operation::Exit(42)]);
        assert!(program.data.is_empty());
    }

    #[test]
    fn lowers_hello_world_fixture_to_program() {
        let root = repo_root().join("tests/testcases/hello-world");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![
                Operation::Write {
                    fd: 1,
                    data_index: 0
                },
                Operation::Exit(0)
            ]
        );
        assert_eq!(program.data, vec![b"Hello, world!".to_vec()]);
    }

    #[test]
    fn builds_elf_image_with_exit_syscall() {
        let program = LoweredProgram {
            operations: vec![Operation::Exit(42)],
            data: Vec::new(),
        };
        let elf = build_linux_x86_64_program_executable(&program).to_bytes();
        assert_eq!(&elf[0..4], b"\x7FELF");
        assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1005..0x100a], &[0xbf, 42, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x100a..0x100c], &[0x0f, 0x05]);
    }

    #[test]
    fn builds_elf_image_with_write_and_implicit_exit() {
        let program = LoweredProgram {
            operations: vec![
                Operation::Write {
                    fd: 1,
                    data_index: 0,
                },
                Operation::Exit(0),
            ],
            data: vec![b"Hello, world!".to_vec()],
        };
        let elf = build_linux_x86_64_program_executable(&program).to_bytes();

        assert_eq!(&elf[0..4], b"\x7FELF");
        assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1005..0x100a], &[0xbf, 0x01, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x100a..0x100c], &[0x48, 0xbe]);
        assert_eq!(&elf[0x100c..0x1014], &0x402000_u64.to_le_bytes());
        assert_eq!(&elf[0x1014..0x1019], &[0xba, 13, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1019..0x101b], &[0x0f, 0x05]);
        assert_eq!(&elf[0x101b..0x1020], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1020..0x1025], &[0xbf, 0x00, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1025..0x1027], &[0x0f, 0x05]);
        assert_eq!(&elf[0x2000..0x200d], b"Hello, world!");
    }

    #[test]
    fn defaults_output_into_package_neco_directory() {
        let root = repo_root().join("tests/testcases/exit-42");
        let output = default_output_path(&root);
        assert_eq!(output, root.join(".neco").join("exit-42"));
    }
}
