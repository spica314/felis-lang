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
    arrays: Vec<ArrayAllocation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum I32Expr {
    Literal(i32),
    Add(Box<I32Expr>, Box<I32Expr>),
    Sub(Box<I32Expr>, Box<I32Expr>),
    Mul(Box<I32Expr>, Box<I32Expr>),
    Div(Box<I32Expr>, Box<I32Expr>),
    Mod(Box<I32Expr>, Box<I32Expr>),
    ArrayGet {
        array_slot: usize,
        index: Box<I32Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum U8Expr {
    Literal(u8),
    Add(Box<U8Expr>, Box<U8Expr>),
    Sub(Box<U8Expr>, Box<U8Expr>),
    Mul(Box<U8Expr>, Box<U8Expr>),
    Div(Box<U8Expr>, Box<U8Expr>),
    Mod(Box<U8Expr>, Box<U8Expr>),
    ArrayGet {
        array_slot: usize,
        index: Box<I32Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExitCodeExpr {
    I32(I32Expr),
    U8(U8Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArrayElementType {
    I32,
    U8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ArrayAllocation {
    slot: usize,
    len: usize,
    element_type: ArrayElementType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Operation {
    WriteStatic {
        fd: u32,
        data_index: usize,
    },
    WriteArray {
        fd: u32,
        array_slot: usize,
    },
    ArraySetI32 {
        array_slot: usize,
        index: I32Expr,
        value: I32Expr,
    },
    ArraySetU8 {
        array_slot: usize,
        index: I32Expr,
        value: U8Expr,
    },
    Exit(ExitCodeExpr),
}

pub(crate) struct LoweringState {
    environment: HashMap<String, Value>,
    next_array_slot: usize,
}

impl LoweringState {
    fn new() -> Self {
        Self {
            environment: HashMap::new(),
            next_array_slot: 0,
        }
    }

    fn allocate_array(
        &mut self,
        element_type: ArrayElementType,
        len: usize,
        program: &mut LoweredProgram,
    ) -> usize {
        let slot = self.next_array_slot;
        self.next_array_slot += 1;
        program.arrays.push(ArrayAllocation {
            slot,
            len,
            element_type,
        });
        slot
    }
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
        arrays: Vec::new(),
    };
    let mut state = LoweringState::new();
    let mut terminated = false;

    for statement in &main_fn.body.statements {
        if terminated {
            return Err(Error::Unsupported(
                "statements after `IO::exit` are not supported".to_string(),
            ));
        }

        terminated = lower_statement(statement, &mut state, &mut program)?;
    }

    if !terminated {
        program
            .operations
            .push(Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))));
    }

    Ok(program)
}

fn lower_statement(
    statement: &Statement,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    match statement {
        Statement::Let(let_stmt) => match let_stmt.operator {
            neco_rs_parser::LetOperator::Equals => {
                let value = lower_pure_value(let_stmt.value.as_ref(), state, program)?;
                bind_pattern(&let_stmt.binder, value, &mut state.environment);
                Ok(false)
            }
            neco_rs_parser::LetOperator::LeftArrow => {
                lower_effect(&let_stmt.binder, let_stmt.value.as_ref(), state, program)
            }
        },
        Statement::Expression(term) => {
            lower_expression_statement(term.as_ref(), state, program)?;
            Ok(false)
        }
        Statement::Item(_) => Err(Error::Unsupported(
            "items inside entrypoint bodies are not supported".to_string(),
        )),
    }
}

fn lower_pure_value(
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    match term {
        Term::Group(inner) => lower_pure_value(inner, state, program),
        Term::StringLiteral(literal) => {
            let data_index = intern_data(program, literal.as_bytes().to_vec());
            Ok(Value::ByteString(data_index))
        }
        Term::IntegerLiteral(_) | Term::Application { .. } => {
            if let Ok(expr) = lower_i32_expr(term, state) {
                return Ok(Value::I32(expr));
            }
            if let Ok(expr) = lower_u8_expr(term, state) {
                return Ok(Value::U8(expr));
            }
            Err(Error::Unsupported(format!(
                "unsupported pure expression in entrypoint body: {term:?}"
            )))
        }
        Term::MethodCall { receiver, method } if method == "as_bytes" => {
            match resolve_value(receiver.as_ref(), &state.environment)? {
                Value::ByteString(data_index) => Ok(Value::ByteString(data_index)),
                other => Err(Error::Unsupported(format!(
                    "`as_bytes` expects a string reference, got {other:?}"
                ))),
            }
        }
        Term::Path(_) => resolve_value(term, &state.environment),
        _ => Err(Error::Unsupported(format!(
            "unsupported pure expression in entrypoint body: {term:?}"
        ))),
    }
}

pub(crate) fn lower_i32_expr(term: &Term, state: &LoweringState) -> Result<I32Expr> {
    match term {
        Term::Group(inner) => lower_i32_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_i32_literal(literal),
        Term::Path(_) => match resolve_value(term, &state.environment)? {
            Value::I32(expr) => Ok(expr),
            other => Err(Error::Unsupported(format!(
                "expected an `i32` value, got {other:?}"
            ))),
        },
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_i32_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_array_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            lower_i32_primitive_call(callee, arguments, state)
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `i32` expression in entrypoint body: {term:?}"
        ))),
    }
}

pub(crate) fn lower_u8_expr(term: &Term, state: &LoweringState) -> Result<U8Expr> {
    match term {
        Term::Group(inner) => lower_u8_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_u8_literal(literal),
        Term::Path(_) => match resolve_value(term, &state.environment)? {
            Value::U8(expr) => Ok(expr),
            other => Err(Error::Unsupported(format!(
                "expected a `u8` value, got {other:?}"
            ))),
        },
        Term::Application { callee, arguments } => {
            if let Some(expr) = lower_u8_literal_application(callee, arguments)? {
                return Ok(expr);
            }
            if let Some(expr) = lower_u8_array_get_call(callee, arguments, state)? {
                return Ok(expr);
            }
            lower_u8_primitive_call(callee, arguments, state)
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported `u8` expression in entrypoint body: {term:?}"
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
    state: &LoweringState,
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
    let lhs = Box::new(lower_i32_expr(lhs, state)?);
    let rhs = Box::new(lower_i32_expr(rhs, state)?);

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

fn lower_u8_literal_application(callee: &Term, arguments: &[Term]) -> Result<Option<U8Expr>> {
    let [suffix] = arguments else {
        return Ok(None);
    };
    let Term::IntegerLiteral(literal) = callee else {
        return Ok(None);
    };
    if !is_u8_suffix_term(suffix) {
        return Ok(None);
    }
    Ok(Some(parse_bare_u8_literal(literal)?))
}

fn lower_u8_primitive_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<U8Expr> {
    let Term::Path(path) = callee else {
        return Err(Error::Unsupported(
            "unsupported `u8` callee in entrypoint body".to_string(),
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
    let lhs = Box::new(lower_u8_expr(lhs, state)?);
    let rhs = Box::new(lower_u8_expr(rhs, state)?);

    let primitive = segments.last().copied().unwrap_or_default();

    match primitive {
        "u8_add" => Ok(U8Expr::Add(lhs, rhs)),
        "u8_sub" => Ok(U8Expr::Sub(lhs, rhs)),
        "u8_mul" => Ok(U8Expr::Mul(lhs, rhs)),
        "u8_div" => Ok(U8Expr::Div(lhs, rhs)),
        "u8_mod" => Ok(U8Expr::Mod(lhs, rhs)),
        _ => Err(Error::Unsupported(format!(
            "unsupported `u8` primitive call `{}`",
            segments.join("::")
        ))),
    }
}

fn lower_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`get` must receive exactly one index argument".to_string(),
        ));
    };

    let array_slot = match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::Array {
            slot,
            element_type: ArrayElementType::I32,
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`get` expects an `i32` array reference, got {other:?}"
            )));
        }
    };

    Ok(Some(I32Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_i32_expr(index, state)?),
    }))
}

fn lower_u8_array_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<U8Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`get` must receive exactly one index argument".to_string(),
        ));
    };

    let array_slot = match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
        } => slot,
        other => {
            return Err(Error::Unsupported(format!(
                "`get` expects a `u8` array reference, got {other:?}"
            )));
        }
    };

    Ok(Some(U8Expr::ArrayGet {
        array_slot,
        index: Box::new(lower_i32_expr(index, state)?),
    }))
}

fn lower_expression_statement(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    let Term::Application { callee, arguments } = term else {
        return Err(Error::Unsupported(format!(
            "unsupported expression statement in entrypoint body: {term:?}"
        )));
    };
    let Term::MethodCall { receiver, method } = callee.as_ref() else {
        return Err(Error::Unsupported(format!(
            "unsupported expression statement in entrypoint body: {term:?}"
        )));
    };
    if method != "set" {
        return Err(Error::Unsupported(format!(
            "unsupported expression statement in entrypoint body: {term:?}"
        )));
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [index, value] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`set` must receive exactly two arguments".to_string(),
        ));
    };

    let index = lower_i32_expr(index, state)?;
    match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::Array {
            slot,
            element_type: ArrayElementType::I32,
        } => program.operations.push(Operation::ArraySetI32 {
            array_slot: slot,
            index,
            value: lower_i32_expr(value, state)?,
        }),
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
        } => program.operations.push(Operation::ArraySetU8 {
            array_slot: slot,
            index,
            value: lower_u8_expr(value, state)?,
        }),
        other => {
            return Err(Error::Unsupported(format!(
                "`set` expects an array reference, got {other:?}"
            )));
        }
    }
    Ok(())
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

fn parse_suffixed_u8_literal(literal: &str) -> Result<U8Expr> {
    let digits = literal.strip_suffix("u8").ok_or_else(|| {
        Error::Unsupported("integer literal must use the `u8` suffix".to_string())
    })?;
    Ok(U8Expr::Literal(parse_u8_digits(digits, literal)?))
}

fn parse_bare_u8_literal(literal: &str) -> Result<U8Expr> {
    Ok(U8Expr::Literal(parse_u8_digits(literal, literal)?))
}

fn parse_i32_digits(digits: &str, original: &str) -> Result<i32> {
    digits.parse::<i32>().map_err(|_| {
        Error::Unsupported(format!(
            "integer literal `{original}` could not be parsed as i32"
        ))
    })
}

fn parse_u8_digits(digits: &str, original: &str) -> Result<u8> {
    digits.parse::<u8>().map_err(|_| {
        Error::Unsupported(format!(
            "integer literal `{original}` could not be parsed as u8"
        ))
    })
}

fn is_i32_suffix_term(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "i32",
        _ => false,
    }
}

fn is_u8_suffix_term(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "u8",
        _ => false,
    }
}

fn normalize_numeric_literal_arguments(arguments: &[Term]) -> Vec<Term> {
    let mut normalized = Vec::with_capacity(arguments.len());
    let mut index = 0;
    while index < arguments.len() {
        if index + 1 < arguments.len()
            && matches!(arguments[index], Term::IntegerLiteral(_))
            && (is_i32_suffix_term(&arguments[index + 1])
                || is_u8_suffix_term(&arguments[index + 1]))
        {
            normalized.push(Term::Application {
                callee: Box::new(arguments[index].clone()),
                arguments: vec![arguments[index + 1].clone()],
            });
            index += 2;
            continue;
        }
        normalized.push(arguments[index].clone());
        index += 1;
    }
    normalized
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
    let stack_frame_size = stack_frame_size(program);

    if stack_frame_size > 0 {
        code.push(0x55);
        code.extend_from_slice(&[0x48, 0x89, 0xe5]);
        code.extend_from_slice(&[0x48, 0x81, 0xec]);
        code.extend_from_slice(&(stack_frame_size as u32).to_le_bytes());
        emit_array_initializers(program, &mut code);
    }

    for operation in &program.operations {
        match operation {
            Operation::WriteStatic { fd, data_index } => {
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
            Operation::WriteArray { fd, array_slot } => {
                let array = array_allocation(program, *array_slot);
                debug_assert_eq!(array.element_type, ArrayElementType::U8);
                code.extend_from_slice(&[0xb8, 0x01, 0x00, 0x00, 0x00]);
                code.push(0xbf);
                code.extend_from_slice(&fd.to_le_bytes());
                let slot_offset = array_slot_offset(*array_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0xb5]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                code.push(0xba);
                code.extend_from_slice(&(array.len as u32).to_le_bytes());
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::ArraySetI32 {
                array_slot,
                index,
                value,
            } => emit_i32_array_set(*array_slot, index, value, &mut code, program),
            Operation::ArraySetU8 {
                array_slot,
                index,
                value,
            } => emit_u8_array_set(*array_slot, index, value, &mut code, program),
            Operation::Exit(exit_code) => {
                emit_exit_code_expr_to_eax(exit_code, &mut code, program);
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

fn emit_exit_code_expr_to_eax(expr: &ExitCodeExpr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        ExitCodeExpr::I32(expr) => emit_i32_expr_to_eax(expr, code, program),
        ExitCodeExpr::U8(expr) => emit_u8_expr_to_eax(expr, code, program),
    }
}

fn emit_i32_expr_to_eax(expr: &I32Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        I32Expr::Literal(value) => {
            // mov eax, imm32
            code.push(0xb8);
            code.extend_from_slice(&value.to_le_bytes());
        }
        // add eax, ecx
        I32Expr::Add(lhs, rhs) => emit_i32_binary_expr(lhs, rhs, code, program, &[0x01, 0xc8]),
        // sub eax, ecx
        I32Expr::Sub(lhs, rhs) => emit_i32_binary_expr(lhs, rhs, code, program, &[0x29, 0xc8]),
        // imul eax, ecx
        I32Expr::Mul(lhs, rhs) => {
            emit_i32_binary_expr(lhs, rhs, code, program, &[0x0f, 0xaf, 0xc1])
        }
        I32Expr::Div(lhs, rhs) => emit_i32_div_mod_expr(lhs, rhs, code, program, false),
        I32Expr::Mod(lhs, rhs) => emit_i32_div_mod_expr(lhs, rhs, code, program, true),
        I32Expr::ArrayGet { array_slot, index } => {
            emit_array_get(*array_slot, index, code, program)
        }
    }
}

fn emit_i32_binary_expr(
    lhs: &I32Expr,
    rhs: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    opcode: &[u8],
) {
    emit_i32_expr_to_eax(lhs, code, program);
    // Save the left operand while evaluating the right operand into eax.
    // push rax
    code.push(0x50);
    emit_i32_expr_to_eax(rhs, code, program);
    // Move the right operand into ecx.
    // mov ecx, eax
    code.extend_from_slice(&[0x89, 0xc1]);
    // Restore the left operand into eax, then apply the binary opcode.
    // pop rax
    code.push(0x58);
    code.extend_from_slice(opcode);
}

fn emit_i32_div_mod_expr(
    lhs: &I32Expr,
    rhs: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    modulo: bool,
) {
    emit_i32_expr_to_eax(lhs, code, program);
    // Save the dividend while evaluating the divisor.
    // push rax
    code.push(0x50);
    emit_i32_expr_to_eax(rhs, code, program);
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

fn emit_u8_expr_to_eax(expr: &U8Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        U8Expr::Literal(value) => {
            // mov eax, imm32
            code.push(0xb8);
            code.extend_from_slice(&u32::from(*value).to_le_bytes());
        }
        U8Expr::Add(lhs, rhs) => emit_u8_binary_expr(lhs, rhs, code, program, &[0x00, 0xc8]),
        U8Expr::Sub(lhs, rhs) => emit_u8_binary_expr(lhs, rhs, code, program, &[0x28, 0xc8]),
        U8Expr::Mul(lhs, rhs) => emit_u8_mul_expr(lhs, rhs, code, program),
        U8Expr::Div(lhs, rhs) => emit_u8_div_mod_expr(lhs, rhs, code, program, false),
        U8Expr::Mod(lhs, rhs) => emit_u8_div_mod_expr(lhs, rhs, code, program, true),
        U8Expr::ArrayGet { array_slot, index } => emit_u8_array_get(*array_slot, index, code, program),
    }
}

fn emit_u8_binary_expr(
    lhs: &U8Expr,
    rhs: &U8Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    opcode: &[u8],
) {
    emit_u8_expr_to_eax(lhs, code, program);
    // push rax
    code.push(0x50);
    emit_u8_expr_to_eax(rhs, code, program);
    // mov ecx, eax
    code.extend_from_slice(&[0x89, 0xc1]);
    // pop rax
    code.push(0x58);
    code.extend_from_slice(opcode);
    // movzx eax, al
    code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
}

fn emit_u8_mul_expr(lhs: &U8Expr, rhs: &U8Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    emit_u8_expr_to_eax(lhs, code, program);
    // push rax
    code.push(0x50);
    emit_u8_expr_to_eax(rhs, code, program);
    // mov ecx, eax
    code.extend_from_slice(&[0x89, 0xc1]);
    // pop rax
    code.push(0x58);
    // imul eax, ecx
    code.extend_from_slice(&[0x0f, 0xaf, 0xc1]);
    // movzx eax, al
    code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
}

fn emit_u8_div_mod_expr(
    lhs: &U8Expr,
    rhs: &U8Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    modulo: bool,
) {
    emit_u8_expr_to_eax(lhs, code, program);
    // push rax
    code.push(0x50);
    emit_u8_expr_to_eax(rhs, code, program);
    // mov ecx, eax
    code.extend_from_slice(&[0x89, 0xc1]);
    // pop rax
    code.push(0x58);
    // xor edx, edx
    code.extend_from_slice(&[0x31, 0xd2]);
    // div ecx
    code.extend_from_slice(&[0xf7, 0xf1]);
    if modulo {
        // mov eax, edx
        code.extend_from_slice(&[0x89, 0xd0]);
    }
    // movzx eax, al
    code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
}

fn stack_frame_size(program: &LoweredProgram) -> usize {
    let pointer_bytes = program.arrays.len() * 8;
    let array_bytes: usize = program.arrays.iter().map(array_storage_size).sum();
    pointer_bytes + array_bytes
}

fn array_slot_offset(slot: usize) -> i32 {
    -8 * (slot as i32 + 1)
}

fn array_data_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let pointer_bytes = (program.arrays.len() * 8) as i32;
    let mut offset = pointer_bytes;
    for array in &program.arrays {
        offset += array_storage_size(array) as i32;
        if array.slot == slot {
            return -offset;
        }
    }
    panic!("unknown array slot {slot}");
}

fn emit_array_initializers(program: &LoweredProgram, code: &mut Vec<u8>) {
    for array in &program.arrays {
        let slot_offset = array_slot_offset(array.slot);
        let data_offset = array_data_offset(program, array.slot);
        // lea rax, [rbp + disp32]
        code.extend_from_slice(&[0x48, 0x8d, 0x85]);
        code.extend_from_slice(&data_offset.to_le_bytes());
        // mov [rbp + disp32], rax
        code.extend_from_slice(&[0x48, 0x89, 0x85]);
        code.extend_from_slice(&slot_offset.to_le_bytes());
    }
}

fn array_storage_size(array: &ArrayAllocation) -> usize {
    let element_size = match array.element_type {
        ArrayElementType::I32 => 4,
        ArrayElementType::U8 => 1,
    };
    array.len * element_size
}

fn array_allocation(program: &LoweredProgram, slot: usize) -> &ArrayAllocation {
    program
        .arrays
        .iter()
        .find(|array| array.slot == slot)
        .unwrap_or_else(|| panic!("unknown array slot {slot}"))
}

fn emit_i32_array_set(
    array_slot: usize,
    index: &I32Expr,
    value: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    // movsxd rcx, eax
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
    // shl rcx, 2
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x02]);
    // push rcx
    code.push(0x51);
    emit_i32_expr_to_eax(value, code, program);
    // mov edx, eax
    code.extend_from_slice(&[0x89, 0xc2]);
    // pop rcx
    code.push(0x59);
    let slot_offset = array_slot_offset(array_slot);
    // mov rbx, [rbp + disp32]
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    // mov [rbx + rcx], edx
    code.extend_from_slice(&[0x89, 0x14, 0x0b]);
}

fn emit_u8_array_set(
    array_slot: usize,
    index: &I32Expr,
    value: &U8Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
    code.push(0x51);
    emit_u8_expr_to_eax(value, code, program);
    code.extend_from_slice(&[0x89, 0xc2]);
    code.push(0x59);
    let slot_offset = array_slot_offset(array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x88, 0x14, 0x0b]);
}

fn emit_array_get(
    array_slot: usize,
    index: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    // movsxd rcx, eax
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
    // shl rcx, 2
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x02]);
    let slot_offset = array_slot_offset(array_slot);
    // mov rbx, [rbp + disp32]
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    // mov eax, [rbx + rcx]
    code.extend_from_slice(&[0x8b, 0x04, 0x0b]);
}

fn emit_u8_array_get(
    array_slot: usize,
    index: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
    let slot_offset = array_slot_offset(array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x0f, 0xb6, 0x04, 0x0b]);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsString;
    use std::os::unix::fs::PermissionsExt;
    use std::process::{Command, Output};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn repo_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root")
    }

    fn compile_fixture(root: &Path, name: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time")
            .as_nanos();
        let output = std::env::temp_dir().join(format!("neco-rs-{name}-{unique}"));

        compile_path_to_elf(root, &output).expect("compile fixture");

        let mut permissions = fs::metadata(&output)
            .expect("binary metadata")
            .permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&output, permissions).expect("binary permissions");

        output
    }

    fn runtime_test_runner(binary: &Path) -> Command {
        let qemu = std::env::var_os("NECO_RS_TEST_QEMU")
            .unwrap_or_else(|| OsString::from("qemu-x86_64"));
        let mut command = Command::new(&qemu);
        command.arg(binary);
        command
    }

    fn run_fixture_status(root: &Path, name: &str) -> std::process::ExitStatus {
        let output = compile_fixture(root, name);
        let status = runtime_test_runner(&output)
            .status()
            .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
        fs::remove_file(&output).expect("cleanup binary");
        status
    }

    fn run_fixture_output(root: &Path, name: &str) -> Output {
        let output = compile_fixture(root, name);
        let run = runtime_test_runner(&output)
            .output()
            .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
        fs::remove_file(&output).expect("cleanup binary");
        run
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
            vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))]
        );
        assert!(program.data.is_empty());
        assert!(program.arrays.is_empty());
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
                Operation::WriteStatic {
                    fd: 1,
                    data_index: 0
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0)))
            ]
        );
        assert_eq!(program.data, vec![b"Hello, world!\n".to_vec()]);
        assert!(program.arrays.is_empty());
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
            vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Mod(
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
            )))]
        );
        assert!(program.data.is_empty());
        assert!(program.arrays.is_empty());
    }

    #[test]
    fn lowers_u8_ops_fixture_to_runtime_expression_tree() {
        let root = repo_root().join("tests/testcases/u8-ops");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![Operation::Exit(ExitCodeExpr::U8(U8Expr::Mod(
                Box::new(U8Expr::Div(
                    Box::new(U8Expr::Mul(
                        Box::new(U8Expr::Sub(
                            Box::new(U8Expr::Add(
                                Box::new(U8Expr::Literal(3)),
                                Box::new(U8Expr::Literal(7)),
                            )),
                            Box::new(U8Expr::Literal(4)),
                        )),
                        Box::new(U8Expr::Literal(21)),
                    )),
                    Box::new(U8Expr::Literal(3)),
                )),
                Box::new(U8Expr::Literal(80)),
            )))]
        );
        assert!(program.data.is_empty());
        assert!(program.arrays.is_empty());
    }

    #[test]
    fn lowers_array_basic_fixture_to_runtime_array_operations() {
        let root = repo_root().join("tests/testcases/array-basic");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.arrays,
            vec![ArrayAllocation {
                slot: 0,
                len: 4,
                element_type: ArrayElementType::I32,
            }]
        );
        assert_eq!(
            program.operations,
            vec![
                Operation::ArraySetI32 {
                    array_slot: 0,
                    index: I32Expr::Literal(0),
                    value: I32Expr::Literal(7),
                },
                Operation::ArraySetI32 {
                    array_slot: 0,
                    index: I32Expr::Literal(1),
                    value: I32Expr::Literal(14),
                },
                Operation::ArraySetI32 {
                    array_slot: 0,
                    index: I32Expr::Literal(2),
                    value: I32Expr::Literal(21),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                    Box::new(I32Expr::Add(
                        Box::new(I32Expr::ArrayGet {
                            array_slot: 0,
                            index: Box::new(I32Expr::Literal(0)),
                        }),
                        Box::new(I32Expr::ArrayGet {
                            array_slot: 0,
                            index: Box::new(I32Expr::Literal(1)),
                        }),
                    )),
                    Box::new(I32Expr::ArrayGet {
                        array_slot: 0,
                        index: Box::new(I32Expr::Literal(2)),
                    }),
                ))),
            ]
        );
    }

    #[test]
    fn lowers_u8_array_hello_world_fixture_to_runtime_array_operations() {
        let root = repo_root().join("tests/testcases/u8-array-hello-world");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.arrays,
            vec![ArrayAllocation {
                slot: 0,
                len: 13,
                element_type: ArrayElementType::U8,
            }]
        );
        assert_eq!(
            program.operations,
            vec![
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(0),
                    value: U8Expr::Literal(104),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(1),
                    value: U8Expr::Literal(101),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(2),
                    value: U8Expr::Literal(108),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(3),
                    value: U8Expr::Literal(108),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(4),
                    value: U8Expr::Literal(111),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(5),
                    value: U8Expr::Literal(44),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(6),
                    value: U8Expr::Literal(32),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(7),
                    value: U8Expr::Literal(119),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(8),
                    value: U8Expr::Literal(111),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(9),
                    value: U8Expr::Literal(114),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(10),
                    value: U8Expr::Literal(108),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(11),
                    value: U8Expr::Literal(100),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(12),
                    value: U8Expr::Literal(10),
                },
                Operation::WriteArray {
                    fd: 1,
                    array_slot: 0,
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ]
        );
        assert!(program.data.is_empty());
    }

    #[test]
    fn builds_elf_image_with_exit_syscall() {
        let program = LoweredProgram {
            operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))],
            data: Vec::new(),
            arrays: Vec::new(),
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
                Operation::WriteStatic {
                    fd: 1,
                    data_index: 0,
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ],
            data: vec![b"Hello, world!\n".to_vec()],
            arrays: Vec::new(),
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
            operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Mod(
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
            )))],
            data: Vec::new(),
            arrays: Vec::new(),
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
        let status = run_fixture_status(&root, "i32-ops");
        assert_eq!(status.code(), Some(42));
    }

    #[test]
    fn compiles_and_runs_u8_ops_fixture() {
        let root = repo_root().join("tests/testcases/u8-ops");
        let status = run_fixture_status(&root, "u8-ops");
        assert_eq!(status.code(), Some(42));
    }

    #[test]
    fn compiles_and_runs_array_basic_fixture() {
        let root = repo_root().join("tests/testcases/array-basic");
        let status = run_fixture_status(&root, "array-basic");
        assert_eq!(status.code(), Some(42));
    }

    #[test]
    fn compiles_and_runs_u8_array_hello_world_fixture() {
        let root = repo_root().join("tests/testcases/u8-array-hello-world");
        let run = run_fixture_output(&root, "u8-array-hello-world");
        assert_eq!(run.status.code(), Some(0));
        assert_eq!(run.stdout, b"hello, world\n");
        assert!(run.stderr.is_empty());
    }

    #[test]
    fn defaults_output_into_package_neco_directory() {
        let root = repo_root().join("tests/testcases/exit-42");
        let output = default_output_path(&root);
        assert_eq!(output, root.join(".neco").join("exit-42"));
    }
}
