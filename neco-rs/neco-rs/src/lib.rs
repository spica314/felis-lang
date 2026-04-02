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
pub(crate) struct LoweredProgram {
    operations: Vec<Operation>,
    data: Vec<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum I32Expr {
    Literal(i32),
    Add(Box<I32Expr>, Box<I32Expr>),
    Sub(Box<I32Expr>, Box<I32Expr>),
    Mul(Box<I32Expr>, Box<I32Expr>),
    Div(Box<I32Expr>, Box<I32Expr>),
    Mod(Box<I32Expr>, Box<I32Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Operation {
    Write { fd: u32, data_index: usize },
    Exit(I32Expr),
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
        program
            .operations
            .push(Operation::Exit(I32Expr::Literal(0)));
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
        Term::Group(inner) => lower_pure_value(inner, environment, program),
        Term::StringLiteral(literal) => {
            let data_index = intern_data(program, literal.as_bytes().to_vec());
            Ok(Value::ByteString(data_index))
        }
        Term::IntegerLiteral(_) | Term::Application { .. } => {
            if let Ok(expr) = lower_i32_expr(term, environment) {
                return Ok(Value::I32(expr));
            }
            Err(Error::Unsupported(format!(
                "unsupported pure expression in entrypoint body: {term:?}"
            )))
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

pub(crate) fn lower_i32_expr(term: &Term, environment: &HashMap<String, Value>) -> Result<I32Expr> {
    match term {
        Term::Group(inner) => lower_i32_expr(inner, environment),
        Term::IntegerLiteral(literal) => parse_suffixed_i32_literal(literal),
        Term::Path(_) => match resolve_value(term, environment)? {
            Value::I32(expr) => Ok(expr),
            other => Err(Error::Unsupported(format!(
                "expected an `i32` value, got {other:?}"
            ))),
        },
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_i32_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            lower_i32_primitive_call(callee, arguments, environment)
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `i32` expression in entrypoint body: {term:?}"
        ))),
    }
}

fn lower_i32_literal_application(callee: &Term, arguments: &[Term]) -> Result<Option<I32Expr>> {
    let [suffix] = arguments else {
        return Ok(None);
    };
    let Term::IntegerLiteral(literal) = callee else {
        return Ok(None);
    };
    if !is_i32_suffix_term(suffix) {
        return Ok(None);
    }
    Ok(Some(parse_bare_i32_literal(literal)?))
}

fn lower_i32_primitive_call(
    callee: &Term,
    arguments: &[Term],
    environment: &HashMap<String, Value>,
) -> Result<I32Expr> {
    let Term::Path(path) = callee else {
        return Err(Error::Unsupported(
            "unsupported `i32` callee in entrypoint body".to_string(),
        ));
    };

    let segments: Vec<_> = path
        .segments
        .iter()
        .map(|segment| segment.name.as_str())
        .collect();
    let [lhs, rhs] = arguments else {
        return Err(Error::Unsupported(format!(
            "`{}` must receive exactly two arguments",
            segments.join("::")
        )));
    };
    let lhs = Box::new(lower_i32_expr(lhs, environment)?);
    let rhs = Box::new(lower_i32_expr(rhs, environment)?);

    let primitive = segments.last().copied().unwrap_or_default();

    match primitive {
        "i32_add" => Ok(I32Expr::Add(lhs, rhs)),
        "i32_sub" => Ok(I32Expr::Sub(lhs, rhs)),
        "i32_mul" => Ok(I32Expr::Mul(lhs, rhs)),
        "i32_div" => Ok(I32Expr::Div(lhs, rhs)),
        "i32_mod" => Ok(I32Expr::Mod(lhs, rhs)),
        _ => Err(Error::Unsupported(format!(
            "unsupported `i32` primitive call `{}`",
            segments.join("::")
        ))),
    }
}

fn parse_suffixed_i32_literal(literal: &str) -> Result<I32Expr> {
    let digits = literal.strip_suffix("i32").ok_or_else(|| {
        Error::Unsupported("integer literal must use the `i32` suffix".to_string())
    })?;
    Ok(I32Expr::Literal(parse_i32_digits(digits, literal)?))
}

fn parse_bare_i32_literal(literal: &str) -> Result<I32Expr> {
    Ok(I32Expr::Literal(parse_i32_digits(literal, literal)?))
}

fn parse_i32_digits(digits: &str, original: &str) -> Result<i32> {
    digits.parse::<i32>().map_err(|_| {
        Error::Unsupported(format!(
            "integer literal `{original}` could not be parsed as i32"
        ))
    })
}

fn is_i32_suffix_term(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "i32",
        _ => false,
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
        match operation {
            Operation::Write { fd, data_index } => {
                let bytes = &program.data[*data_index];
                // mov eax, (imm32): 0xb8, (imm32)
                // write syscall: 1
                code.extend_from_slice(&[0xb8, 0x01, 0x00, 0x00, 0x00]);
                // mov edi, (imm32): 0xbf, (imm32)
                code.push(0xbf);
                code.extend_from_slice(&fd.to_le_bytes());
                // mov rsi, (imm64): 0x48, 0xbe, (imm64)
                code.extend_from_slice(&[0x48, 0xbe]);
                code.extend_from_slice(&addresses[*data_index].to_le_bytes());
                // mov edx, (imm32): 0xba
                code.push(0xba);
                code.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
                // syscall
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::Exit(exit_code) => {
                emit_i32_expr_to_eax(exit_code, &mut code);
                // mov edi, eax
                code.extend_from_slice(&[0x89, 0xc7]);
                // mov eax, (imm32): 0xb8, (imm32)
                // exit syscall: 60
                code.extend_from_slice(&[0xb8, 0x3c, 0x00, 0x00, 0x00]);
                // syscall
                code.extend_from_slice(&[0x0f, 0x05]);
            }
        }
    }

    code
}

fn emit_i32_expr_to_eax(expr: &I32Expr, code: &mut Vec<u8>) {
    match expr {
        I32Expr::Literal(value) => {
            // mov eax, imm32
            code.push(0xb8);
            code.extend_from_slice(&value.to_le_bytes());
        }
        // add eax, ecx
        I32Expr::Add(lhs, rhs) => emit_i32_binary_expr(lhs, rhs, code, &[0x01, 0xc8]),
        // sub eax, ecx
        I32Expr::Sub(lhs, rhs) => emit_i32_binary_expr(lhs, rhs, code, &[0x29, 0xc8]),
        // imul eax, ecx
        I32Expr::Mul(lhs, rhs) => emit_i32_binary_expr(lhs, rhs, code, &[0x0f, 0xaf, 0xc1]),
        I32Expr::Div(lhs, rhs) => emit_i32_div_mod_expr(lhs, rhs, code, false),
        I32Expr::Mod(lhs, rhs) => emit_i32_div_mod_expr(lhs, rhs, code, true),
    }
}

fn emit_i32_binary_expr(lhs: &I32Expr, rhs: &I32Expr, code: &mut Vec<u8>, opcode: &[u8]) {
    emit_i32_expr_to_eax(lhs, code);
    // Save the left operand while evaluating the right operand into eax.
    // push rax
    code.push(0x50);
    emit_i32_expr_to_eax(rhs, code);
    // Move the right operand into ecx.
    // mov ecx, eax
    code.extend_from_slice(&[0x89, 0xc1]);
    // Restore the left operand into eax, then apply the binary opcode.
    // pop rax
    code.push(0x58);
    code.extend_from_slice(opcode);
}

fn emit_i32_div_mod_expr(lhs: &I32Expr, rhs: &I32Expr, code: &mut Vec<u8>, modulo: bool) {
    emit_i32_expr_to_eax(lhs, code);
    // Save the dividend while evaluating the divisor.
    // push rax
    code.push(0x50);
    emit_i32_expr_to_eax(rhs, code);
    // Move the divisor into ecx.
    // mov ecx, eax
    code.extend_from_slice(&[0x89, 0xc1]);
    // Restore the dividend into eax.
    // pop rax
    code.push(0x58);
    // Sign-extend eax into edx:eax for signed division.
    // cdq
    code.push(0x99);
    // Signed divide edx:eax by ecx. Quotient goes to eax, remainder to edx.
    // idiv ecx
    code.extend_from_slice(&[0xf7, 0xf9]);
    if modulo {
        // For modulo, return the remainder instead of the quotient.
        // mov eax, edx
        code.extend_from_slice(&[0x89, 0xd0]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;
    use std::process::Command;
    use std::time::{SystemTime, UNIX_EPOCH};

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
        assert_eq!(
            program.operations,
            vec![Operation::Exit(I32Expr::Literal(42))]
        );
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
                Operation::Exit(I32Expr::Literal(0))
            ]
        );
        assert_eq!(program.data, vec![b"Hello, world!\n".to_vec()]);
    }

    #[test]
    fn lowers_i32_ops_fixture_to_runtime_expression_tree() {
        let root = repo_root().join("tests/testcases/i32-ops");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![Operation::Exit(I32Expr::Mod(
                Box::new(I32Expr::Div(
                    Box::new(I32Expr::Mul(
                        Box::new(I32Expr::Sub(
                            Box::new(I32Expr::Add(
                                Box::new(I32Expr::Literal(3)),
                                Box::new(I32Expr::Literal(7)),
                            )),
                            Box::new(I32Expr::Literal(4)),
                        )),
                        Box::new(I32Expr::Literal(61)),
                    )),
                    Box::new(I32Expr::Literal(3)),
                )),
                Box::new(I32Expr::Literal(80)),
            ))]
        );
        assert!(program.data.is_empty());
    }

    #[test]
    fn builds_elf_image_with_exit_syscall() {
        let program = LoweredProgram {
            operations: vec![Operation::Exit(I32Expr::Literal(42))],
            data: Vec::new(),
        };
        let elf = build_linux_x86_64_program_executable(&program).to_bytes();
        assert_eq!(&elf[0..4], b"\x7FELF");
        assert_eq!(&elf[0x1000..0x1005], &[0xb8, 42, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1005..0x1007], &[0x89, 0xc7]);
        assert_eq!(&elf[0x1007..0x100c], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x100c..0x100e], &[0x0f, 0x05]);
    }

    #[test]
    fn builds_elf_image_with_write_and_implicit_exit() {
        let program = LoweredProgram {
            operations: vec![
                Operation::Write {
                    fd: 1,
                    data_index: 0,
                },
                Operation::Exit(I32Expr::Literal(0)),
            ],
            data: vec![b"Hello, world!\n".to_vec()],
        };
        let elf = build_linux_x86_64_program_executable(&program).to_bytes();

        assert_eq!(&elf[0..4], b"\x7FELF");
        assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1005..0x100a], &[0xbf, 0x01, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x100a..0x100c], &[0x48, 0xbe]);
        assert_eq!(&elf[0x100c..0x1014], &0x402000_u64.to_le_bytes());
        assert_eq!(&elf[0x1014..0x1019], &[0xba, 14, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1019..0x101b], &[0x0f, 0x05]);
        assert_eq!(&elf[0x101b..0x1020], &[0xb8, 0x00, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1020..0x1022], &[0x89, 0xc7]);
        assert_eq!(&elf[0x1022..0x1027], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1027..0x1029], &[0x0f, 0x05]);
        assert_eq!(&elf[0x2000..0x200e], b"Hello, world!\n");
    }

    #[test]
    fn builds_elf_image_with_runtime_i32_ops() {
        let program = LoweredProgram {
            operations: vec![Operation::Exit(I32Expr::Mod(
                Box::new(I32Expr::Div(
                    Box::new(I32Expr::Mul(
                        Box::new(I32Expr::Sub(
                            Box::new(I32Expr::Add(
                                Box::new(I32Expr::Literal(3)),
                                Box::new(I32Expr::Literal(7)),
                            )),
                            Box::new(I32Expr::Literal(4)),
                        )),
                        Box::new(I32Expr::Literal(61)),
                    )),
                    Box::new(I32Expr::Literal(3)),
                )),
                Box::new(I32Expr::Literal(80)),
            ))],
            data: Vec::new(),
        };
        let elf = build_linux_x86_64_program_executable(&program).to_bytes();
        let code = &elf[0x1000..];

        assert!(code.windows(2).any(|window| window == [0x01, 0xc8]));
        assert!(code.windows(2).any(|window| window == [0x29, 0xc8]));
        assert!(code.windows(3).any(|window| window == [0x0f, 0xaf, 0xc1]));
        assert!(code.windows(2).any(|window| window == [0xf7, 0xf9]));
        assert!(code.windows(2).any(|window| window == [0x89, 0xd0]));
        assert!(
            !code
                .windows(5)
                .any(|window| window == [0xbf, 42, 0x00, 0x00, 0x00])
        );
        assert!(code.windows(2).any(|window| window == [0x89, 0xc7]));
    }

    #[test]
    fn compiles_and_runs_i32_ops_fixture() {
        let root = repo_root().join("tests/testcases/i32-ops");
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time")
            .as_nanos();
        let output = std::env::temp_dir().join(format!("neco-rs-i32-ops-{unique}"));

        compile_path_to_elf(&root, &output).expect("compile fixture");

        let mut permissions = fs::metadata(&output)
            .expect("binary metadata")
            .permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&output, permissions).expect("binary permissions");

        let status = Command::new(&output).status().expect("run binary");
        assert_eq!(status.code(), Some(42));

        fs::remove_file(&output).expect("cleanup binary");
    }

    #[test]
    fn defaults_output_into_package_neco_directory() {
        let root = repo_root().join("tests/testcases/exit-42");
        let output = default_output_path(&root);
        assert_eq!(output, root.join(".neco").join("exit-42"));
    }
}
