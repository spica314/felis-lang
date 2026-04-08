use std::collections::HashMap;
use std::fmt;
use std::fs;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

mod effect;

use effect::{Value, bind_pattern, lower_effect, resolve_value};
use neco_rs_elf::{Elf64Executable, LoadSegment, SegmentFlags};
use neco_rs_parser::{
    ArrowParameter, BindingPattern, Block, ConstructorDeclaration, DeclaredName,
    FunctionDeclaration, FunctionKind, Item, LetOperator, MatchExpression, ParsedPackage,
    ParsedRoot, PathExpression, Pattern, Statement, Term, parse_root,
};

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
    let options = CliOptions::parse(args)?;
    compile_path_to_elf(&options.input, &options.output)
}

pub fn compile_path_to_elf(input: &Path, output: &Path) -> Result<()> {
    let parsed = parse_root(input)?;
    let package = select_package_for_build(parsed, output)?;

    let program = lower_package_to_program(&package)?;
    let elf = build_linux_x86_64_program_executable(&program).to_bytes()?;
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
    set_output_executable(output)?;
    Ok(())
}

fn select_package_for_build(parsed: ParsedRoot, output: &Path) -> Result<ParsedPackage> {
    match parsed {
        ParsedRoot::Package(package) => select_binary_from_package(package, output),
        ParsedRoot::Workspace(workspace) => {
            let binary_packages: Vec<_> = workspace
                .packages
                .into_iter()
                .filter(|package| !package.manifest.felis_bin_entrypoints.is_empty())
                .collect();

            match binary_packages.len() {
                0 => Err(Error::Unsupported(
                    "workspace root does not contain a binary package".to_string(),
                )),
                1 => {
                    select_binary_from_package(binary_packages.into_iter().next().unwrap(), output)
                }
                _ => {
                    let output_name = output_selection_name(output);
                    let mut matches = binary_packages
                        .into_iter()
                        .filter(|package| output_name_matches(&output_name, &package.manifest.name))
                        .collect::<Vec<_>>();
                    if matches.len() == 1 {
                        return select_binary_from_package(matches.remove(0), output);
                    }

                    Err(Error::Unsupported(
                        "workspace root resolves to multiple binary packages; choose an output path that identifies the target package".to_string(),
                    ))
                }
            }
        }
    }
}

fn select_binary_from_package(package: ParsedPackage, output: &Path) -> Result<ParsedPackage> {
    match package.manifest.felis_bin_entrypoints.len() {
        0 => Ok(package),
        1 => Ok(package),
        _ => {
            let output_name = output_selection_name(output);
            let mut matches = package
                .manifest
                .felis_bin_entrypoints
                .iter()
                .filter(|path| output_name_matches(&output_name, &binary_name(path)))
                .cloned()
                .collect::<Vec<_>>();

            if matches.len() != 1 {
                return Err(Error::Unsupported(
                    "package contains multiple binary entrypoints; choose an output path that identifies the target binary".to_string(),
                ));
            }

            let selected = matches.remove(0);
            let selected_path = package.root_dir.join(&selected);
            let source_files = package
                .source_files
                .into_iter()
                .filter(|file| {
                    file.role != neco_rs_parser::SourceFileRole::BinaryEntrypoint
                        || file.path == selected_path
                })
                .collect();
            let mut manifest = package.manifest;
            manifest.felis_bin_entrypoints = vec![selected];

            Ok(ParsedPackage {
                root_dir: package.root_dir,
                manifest_path: package.manifest_path,
                manifest,
                source_files,
            })
        }
    }
}

fn binary_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or_default()
        .to_string()
}

fn output_selection_name(output: &Path) -> String {
    output
        .file_name()
        .or_else(|| output.file_stem())
        .and_then(|name| name.to_str())
        .unwrap_or_default()
        .to_string()
}

fn output_name_matches(output_name: &str, candidate: &str) -> bool {
    output_name == candidate || output_name.contains(candidate)
}

fn set_output_executable(output: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        let mut permissions = fs::metadata(output)
            .map_err(|source| Error::Io {
                path: Some(output.to_path_buf()),
                source,
            })?
            .permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(output, permissions).map_err(|source| Error::Io {
            path: Some(output.to_path_buf()),
            source,
        })?;
    }

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
    i32_slots: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum I32Expr {
    Literal(i32),
    Local(usize),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ComparisonKind {
    Eq,
    Lte,
    Lt,
    Gte,
    Gt,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConditionExpr {
    I32 {
        kind: ComparisonKind,
        lhs: I32Expr,
        rhs: I32Expr,
    },
    U8 {
        kind: ComparisonKind,
        lhs: U8Expr,
        rhs: U8Expr,
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
    StoreI32 {
        slot: usize,
        value: I32Expr,
    },
    Open {
        path_data_index: usize,
        flags: I32Expr,
        mode: I32Expr,
        result_slot: usize,
    },
    Close {
        fd: I32Expr,
    },
    Read {
        fd: I32Expr,
        array_slot: usize,
        len: I32Expr,
        result_slot: usize,
    },
    WriteStatic {
        fd: I32Expr,
        data_index: usize,
        len: I32Expr,
    },
    WriteArray {
        fd: I32Expr,
        array_slot: usize,
        len: I32Expr,
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
    If {
        condition: ConditionExpr,
        then_operations: Vec<Operation>,
        else_operations: Vec<Operation>,
    },
    Loop {
        body_operations: Vec<Operation>,
    },
    Break,
    Continue,
    Exit(ExitCodeExpr),
}

pub(crate) struct LoweringState {
    environment: HashMap<String, Value>,
    next_array_slot: usize,
    next_i32_slot: usize,
    functions: HashMap<String, PureFunction>,
    constructors: HashMap<String, ConstructorSignature>,
    loop_depth: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PureFunction {
    parameters: Vec<String>,
    body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ConstructorValue {
    type_name: String,
    constructor_name: String,
    fields: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConstructorSignature {
    type_name: String,
    constructor_name: String,
    arity: usize,
}

impl LoweringState {
    fn new() -> Self {
        Self {
            environment: HashMap::new(),
            next_array_slot: 0,
            next_i32_slot: 0,
            functions: HashMap::new(),
            constructors: HashMap::new(),
            loop_depth: 0,
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

    fn allocate_i32_slot(&mut self) -> usize {
        let slot = self.next_i32_slot;
        self.next_i32_slot += 1;
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
        i32_slots: 0,
    };
    let mut state = LoweringState::new();
    state.functions = collect_pure_functions(package)?;
    state.constructors = collect_constructors(package)?;
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

    program.i32_slots = state.next_i32_slot;

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
                lower_let_equals_statement(
                    &let_stmt.binder,
                    let_stmt.value.as_ref(),
                    state,
                    program,
                )?;
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
        Statement::If(if_stmt) => {
            let condition = lower_condition_expr(if_stmt.condition.as_ref(), state)?;
            let mut then_state = LoweringState {
                environment: state.environment.clone(),
                next_array_slot: state.next_array_slot,
                next_i32_slot: state.next_i32_slot,
                functions: state.functions.clone(),
                constructors: state.constructors.clone(),
                loop_depth: state.loop_depth,
            };
            let mut then_operations = Vec::new();
            let mut then_program = LoweredProgram {
                operations: Vec::new(),
                data: std::mem::take(&mut program.data),
                arrays: std::mem::take(&mut program.arrays),
                i32_slots: program.i32_slots,
            };
            for statement in &if_stmt.then_block.statements {
                let terminated = lower_statement(statement, &mut then_state, &mut then_program)?;
                if terminated {
                    break;
                }
            }
            then_operations.append(&mut then_program.operations);
            let mut else_operations = Vec::new();
            let mut next_array_slot = then_state.next_array_slot;
            let mut next_i32_slot = then_state.next_i32_slot;

            if let Some(else_block) = &if_stmt.else_block {
                let mut else_state = LoweringState {
                    environment: state.environment.clone(),
                    next_array_slot: state.next_array_slot,
                    next_i32_slot: state.next_i32_slot,
                    functions: state.functions.clone(),
                    constructors: state.constructors.clone(),
                    loop_depth: state.loop_depth,
                };
                let mut else_program = LoweredProgram {
                    operations: Vec::new(),
                    data: then_program.data,
                    arrays: then_program.arrays,
                    i32_slots: then_program.i32_slots,
                };
                for statement in &else_block.statements {
                    let terminated =
                        lower_statement(statement, &mut else_state, &mut else_program)?;
                    if terminated {
                        break;
                    }
                }
                else_operations.append(&mut else_program.operations);
                program.data = else_program.data;
                program.arrays = else_program.arrays;
                next_array_slot = next_array_slot.max(else_state.next_array_slot);
                next_i32_slot = next_i32_slot.max(else_state.next_i32_slot);
            } else {
                program.data = then_program.data;
                program.arrays = then_program.arrays;
            }
            state.next_array_slot = next_array_slot;
            state.next_i32_slot = next_i32_slot;
            program.operations.push(Operation::If {
                condition,
                then_operations,
                else_operations,
            });
            Ok(false)
        }
        Statement::Loop(loop_stmt) => {
            let mut loop_state = LoweringState {
                environment: state.environment.clone(),
                next_array_slot: state.next_array_slot,
                next_i32_slot: state.next_i32_slot,
                functions: state.functions.clone(),
                constructors: state.constructors.clone(),
                loop_depth: state.loop_depth + 1,
            };
            let mut loop_program = LoweredProgram {
                operations: Vec::new(),
                data: std::mem::take(&mut program.data),
                arrays: std::mem::take(&mut program.arrays),
                i32_slots: program.i32_slots,
            };
            for statement in &loop_stmt.body.statements {
                let terminated = lower_statement(statement, &mut loop_state, &mut loop_program)?;
                if terminated {
                    break;
                }
            }
            program.data = loop_program.data;
            program.arrays = loop_program.arrays;
            state.next_array_slot = state.next_array_slot.max(loop_state.next_array_slot);
            state.next_i32_slot = state.next_i32_slot.max(loop_state.next_i32_slot);
            program.operations.push(Operation::Loop {
                body_operations: loop_program.operations,
            });
            Ok(false)
        }
        Statement::Break => {
            if state.loop_depth == 0 {
                return Err(Error::Unsupported(
                    "`#break` is only supported inside `#loop`".to_string(),
                ));
            }
            program.operations.push(Operation::Break);
            Ok(false)
        }
        Statement::Continue => {
            if state.loop_depth == 0 {
                return Err(Error::Unsupported(
                    "`#continue` is only supported inside `#loop`".to_string(),
                ));
            }
            program.operations.push(Operation::Continue);
            Ok(false)
        }
        Statement::Item(_) => Err(Error::Unsupported(
            "items inside entrypoint bodies are not supported".to_string(),
        )),
    }
}

fn lower_let_equals_statement(
    binder: &BindingPattern,
    value_term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    let value = lower_pure_value(value_term, state, program)?;
    if let BindingPattern::ValueAndReference {
        value: inner,
        reference,
        ..
    } = binder
    {
        if let Value::I32(expr) = value {
            let slot = state.allocate_i32_slot();
            program
                .operations
                .push(Operation::StoreI32 { slot, value: expr });
            bind_pattern(
                inner.as_ref(),
                Value::I32(I32Expr::Local(slot)),
                &mut state.environment,
            );
            state
                .environment
                .insert(reference.clone(), Value::I32Reference(slot));
            return Ok(());
        }
    }

    bind_pattern(binder, value, &mut state.environment);
    Ok(())
}

fn lower_pure_value(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    match term {
        Term::Group(inner) => lower_pure_value(inner, state, program),
        Term::Block(block) => lower_pure_block_value(block, state, program),
        Term::Match(match_expr) => lower_match_value(match_expr, state, program),
        Term::StringLiteral(literal) => {
            let data_index = intern_data(program, nul_terminated_bytes(literal));
            Ok(Value::ByteString(data_index))
        }
        Term::IntegerLiteral(_) | Term::Application { .. } => {
            if let Term::Application { callee, arguments } = term {
                if let Some(value) =
                    lower_constructor_application(callee.as_ref(), arguments, state, program)?
                {
                    return Ok(value);
                }
            }
            if let Some(value) = lower_pure_function_call(term, state, program)? {
                return Ok(value);
            }
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
        Term::Path(path) => lower_path_value(path, state),
        _ => Err(Error::Unsupported(format!(
            "unsupported pure expression in entrypoint body: {term:?}"
        ))),
    }
}

fn collect_pure_functions(package: &ParsedPackage) -> Result<HashMap<String, PureFunction>> {
    let mut functions = HashMap::new();
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Function(function) = item else {
            continue;
        };
        if function.kind != FunctionKind::Fn || function.effect.is_some() {
            continue;
        }
        functions.insert(
            function.name.name.clone(),
            pure_function_from_decl(function)?,
        );
    }
    Ok(functions)
}

fn collect_constructors(package: &ParsedPackage) -> Result<HashMap<String, ConstructorSignature>> {
    let mut constructors = HashMap::new();
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Type(type_decl) = item else {
            continue;
        };

        for constructor in &type_decl.constructors {
            let Some(arity) = constructor_arity(constructor, &type_decl.name) else {
                continue;
            };

            let key = constructor_key(&type_decl.name.name, &constructor.name.name);
            let value = ConstructorSignature {
                type_name: type_decl.name.name.clone(),
                constructor_name: constructor.name.name.clone(),
                arity,
            };
            if constructors.insert(key.clone(), value).is_some() {
                return Err(Error::Unsupported(format!(
                    "duplicate constructor `{key}` is not supported"
                )));
            }
        }
    }
    Ok(constructors)
}

fn constructor_arity(constructor: &ConstructorDeclaration, type_name: &DeclaredName) -> Option<usize> {
    let mut arity = 0usize;
    let mut current = &constructor.ty;
    while let Term::Arrow(arrow) = current {
        arity += 1;
        current = arrow.result.as_ref();
    }

    let Term::Path(path) = current else {
        return None;
    };
    if !path.starts_with_package
        && path.segments.len() == 1
        && path.segments[0].name == type_name.name
        && path.segments[0].suffixes == type_name.suffixes
    {
        Some(arity)
    } else {
        None
    }
}

fn constructor_key(type_name: &str, constructor_name: &str) -> String {
    format!("{type_name}::{constructor_name}")
}

fn pure_function_from_decl(function: &FunctionDeclaration) -> Result<PureFunction> {
    let mut parameters = Vec::new();
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        let ArrowParameter::Binder(binder) = &arrow.parameter else {
            return Err(Error::Unsupported(format!(
                "pure function `{}` must use named parameters",
                function.name.name
            )));
        };
        parameters.push(binder.name.clone());
        current = arrow.result.as_ref();
    }
    Ok(PureFunction {
        parameters,
        body: function.body.clone(),
    })
}

fn lower_path_value(path: &PathExpression, state: &LoweringState) -> Result<Value> {
    if !path.starts_with_package && path.segments.len() == 1 && path.segments[0].suffixes.is_empty()
    {
        let name = path.segments[0].name.as_str();
        if let Some(value) = state.environment.get(name) {
            return Ok(value.clone());
        }
        return Err(Error::Unsupported(format!(
            "unknown entrypoint local `{name}`"
        )));
    }

    let signature = lower_constructor_value(path, &state.constructors)?;
    if signature.arity != 0 {
        return Err(Error::Unsupported(format!(
            "constructor `{}::{}` requires {} arguments",
            signature.type_name, signature.constructor_name, signature.arity
        )));
    }
    Ok(Value::Constructor(ConstructorValue {
        type_name: signature.type_name,
        constructor_name: signature.constructor_name,
        fields: Vec::new(),
    }))
}

fn lower_constructor_value(
    path: &PathExpression,
    constructors: &HashMap<String, ConstructorSignature>,
) -> Result<ConstructorSignature> {
    if path.starts_with_package {
        return Err(Error::Unsupported(
            "package-qualified constructor paths are not supported in entrypoint lowering"
                .to_string(),
        ));
    }
    if path.segments.len() != 2
        || path
            .segments
            .iter()
            .any(|segment| !segment.suffixes.is_empty())
    {
        return Err(Error::Unsupported(
            "only simple `Type::constructor` paths are supported in entrypoint lowering"
                .to_string(),
        ));
    }

    let key = constructor_key(&path.segments[0].name, &path.segments[1].name);
    constructors
        .get(&key)
        .cloned()
        .ok_or_else(|| Error::Unsupported(format!("unknown constructor `{key}`")))
}

fn lower_match_value(
    match_expr: &MatchExpression,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    let scrutinee = lower_pure_value(match_expr.scrutinee.as_ref(), state, program)?;
    for arm in &match_expr.arms {
        if let Some(bindings) =
            pattern_match_bindings(&arm.pattern, &scrutinee, &state.constructors)?
        {
            let mut scoped_environment = state.environment.clone();
            scoped_environment.extend(bindings);
            let scoped_state = LoweringState {
                environment: scoped_environment,
                next_array_slot: state.next_array_slot,
                next_i32_slot: state.next_i32_slot,
                functions: state.functions.clone(),
                constructors: state.constructors.clone(),
                loop_depth: state.loop_depth,
            };
            return lower_pure_value(arm.result.as_ref(), &scoped_state, program);
        }
    }

    Err(Error::Unsupported(
        "`#match` did not match any constructor arm".to_string(),
    ))
}

fn pattern_match_bindings(
    pattern: &Pattern,
    value: &Value,
    constructors: &HashMap<String, ConstructorSignature>,
) -> Result<Option<HashMap<String, Value>>> {
    match pattern {
        Pattern::Wildcard => Ok(Some(HashMap::new())),
        Pattern::Bind(name) => {
            let mut bindings = HashMap::new();
            bindings.insert(name.clone(), value.clone());
            Ok(Some(bindings))
        }
        Pattern::Constructor { path, subpatterns } => {
            let expected = lower_constructor_value(path, constructors)?;
            let Value::Constructor(actual) = value else {
                return Ok(None);
            };

            if actual.type_name != expected.type_name
                || actual.constructor_name != expected.constructor_name
                || actual.fields.len() != subpatterns.len()
            {
                return Ok(None);
            }

            let mut bindings = HashMap::new();
            for (subpattern, field) in subpatterns.iter().zip(actual.fields.iter()) {
                let Some(sub_bindings) =
                    pattern_match_bindings(subpattern, field, constructors)?
                else {
                    return Ok(None);
                };
                bindings.extend(sub_bindings);
            }
            Ok(Some(bindings))
        }
    }
}

fn lower_constructor_application(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    let signature = match lower_constructor_value(path, &state.constructors) {
        Ok(signature) => signature,
        Err(Error::Unsupported(_)) => return Ok(None),
        Err(error) => return Err(error),
    };

    let normalized_arguments = normalize_numeric_literal_arguments(arguments);
    if signature.arity != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "constructor `{}::{}` must receive exactly {} arguments",
            signature.type_name, signature.constructor_name, signature.arity
        )));
    }

    let mut fields = Vec::with_capacity(normalized_arguments.len());
    for argument in &normalized_arguments {
        fields.push(lower_pure_value(argument, state, program)?);
    }

    Ok(Some(Value::Constructor(ConstructorValue {
        type_name: signature.type_name,
        constructor_name: signature.constructor_name,
        fields,
    })))
}

fn lower_pure_function_call(
    term: &Term,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Option<Value>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.starts_with_package || path.segments.len() != 1 || !path.segments[0].suffixes.is_empty()
    {
        return Ok(None);
    }

    let name = path.segments[0].name.as_str();
    let Some(function) = state.functions.get(name) else {
        return Ok(None);
    };
    if state.environment.contains_key(name) {
        return Ok(None);
    }
    let normalized_arguments = normalize_numeric_literal_arguments(arguments);
    if function.parameters.len() != normalized_arguments.len() {
        return Err(Error::Unsupported(format!(
            "pure function `{name}` must receive exactly {} arguments",
            function.parameters.len()
        )));
    }

    let mut scoped_environment = state.environment.clone();
    for (parameter, argument) in function.parameters.iter().zip(normalized_arguments.iter()) {
        let value = lower_pure_value(argument, state, program)?;
        scoped_environment.insert(parameter.clone(), value);
    }

    let scoped_state = LoweringState {
        environment: scoped_environment,
        next_array_slot: state.next_array_slot,
        next_i32_slot: state.next_i32_slot,
        functions: state.functions.clone(),
        constructors: state.constructors.clone(),
        loop_depth: state.loop_depth,
    };

    lower_pure_block_value(&function.body, &scoped_state, program).map(Some)
}

fn lower_pure_block_value(
    block: &Block,
    state: &LoweringState,
    program: &mut LoweredProgram,
) -> Result<Value> {
    let mut scoped_state = LoweringState {
        environment: state.environment.clone(),
        next_array_slot: state.next_array_slot,
        next_i32_slot: state.next_i32_slot,
        functions: state.functions.clone(),
        constructors: state.constructors.clone(),
        loop_depth: state.loop_depth,
    };

    for statement in &block.statements {
        match statement {
            Statement::Let(let_stmt) if let_stmt.operator == LetOperator::Equals => {
                let value = lower_pure_value(let_stmt.value.as_ref(), &scoped_state, program)?;
                bind_pattern(&let_stmt.binder, value, &mut scoped_state.environment);
            }
            Statement::Let(_) => {
                return Err(Error::Unsupported(
                    "effectful statements are not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Expression(_) => {
                return Err(Error::Unsupported(
                    "expression statements are not supported in pure function bodies".to_string(),
                ));
            }
            Statement::If(_) => {
                return Err(Error::Unsupported(
                    "`#if` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Loop(_) => {
                return Err(Error::Unsupported(
                    "`#loop` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Break => {
                return Err(Error::Unsupported(
                    "`#break` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Continue => {
                return Err(Error::Unsupported(
                    "`#continue` is not supported in pure function bodies".to_string(),
                ));
            }
            Statement::Item(_) => {
                return Err(Error::Unsupported(
                    "items inside pure function bodies are not supported".to_string(),
                ));
            }
        }
    }

    let Some(tail) = block.tail.as_deref() else {
        return Err(Error::Unsupported(
            "pure function bodies must end with a value expression".to_string(),
        ));
    };
    lower_pure_value(tail, &scoped_state, program)
}

pub(crate) fn lower_i32_expr(term: &Term, state: &LoweringState) -> Result<I32Expr> {
    match term {
        Term::Group(inner) => lower_i32_expr(inner, state),
        Term::IntegerLiteral(literal) => parse_suffixed_i32_literal(literal),
        Term::MethodCall { receiver, method } if method == "get" => {
            match resolve_value(receiver.as_ref(), &state.environment)? {
                Value::I32Reference(slot) => Ok(I32Expr::Local(slot)),
                other => Err(Error::Unsupported(format!(
                    "`get` expects an `i32` reference, got {other:?}"
                ))),
            }
        }
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
            if let Some(expr) = lower_i32_reference_get_call(callee, arguments, state)? {
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

fn lower_condition_expr(term: &Term, state: &LoweringState) -> Result<ConditionExpr> {
    let Term::Application { callee, arguments } = term else {
        return Err(Error::Unsupported(
            "`#if` condition must be a comparison call".to_string(),
        ));
    };
    let Term::Path(path) = callee.as_ref() else {
        return Err(Error::Unsupported(
            "`#if` condition must use a path callee".to_string(),
        ));
    };

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [lhs, rhs] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`#if` condition must receive exactly two arguments".to_string(),
        ));
    };

    let primitive = path
        .segments
        .last()
        .map(|segment| segment.name.as_str())
        .unwrap_or_default();

    if let Some(kind) = i32_comparison_kind(primitive) {
        return Ok(ConditionExpr::I32 {
            kind,
            lhs: lower_i32_expr(lhs, state)?,
            rhs: lower_i32_expr(rhs, state)?,
        });
    }
    if let Some(kind) = u8_comparison_kind(primitive) {
        return Ok(ConditionExpr::U8 {
            kind,
            lhs: lower_u8_expr(lhs, state)?,
            rhs: lower_u8_expr(rhs, state)?,
        });
    }

    Err(Error::Unsupported(format!(
        "unsupported `#if` condition `{primitive}`"
    )))
}

fn i32_comparison_kind(name: &str) -> Option<ComparisonKind> {
    match name {
        "i32_eq" => Some(ComparisonKind::Eq),
        "i32_lte" => Some(ComparisonKind::Lte),
        "i32_lt" => Some(ComparisonKind::Lt),
        "i32_gte" => Some(ComparisonKind::Gte),
        "i32_gt" => Some(ComparisonKind::Gt),
        _ => None,
    }
}

fn u8_comparison_kind(name: &str) -> Option<ComparisonKind> {
    match name {
        "u8_eq" => Some(ComparisonKind::Eq),
        "u8_lte" => Some(ComparisonKind::Lte),
        "u8_lt" => Some(ComparisonKind::Lt),
        "u8_gte" => Some(ComparisonKind::Gte),
        "u8_gt" => Some(ComparisonKind::Gt),
        _ => None,
    }
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
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [lhs, rhs] = normalized.as_slice() else {
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
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [lhs, rhs] = normalized.as_slice() else {
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

fn lower_i32_reference_get_call(
    callee: &Term,
    arguments: &[Term],
    state: &LoweringState,
) -> Result<Option<I32Expr>> {
    let Term::MethodCall { receiver, method } = callee else {
        return Ok(None);
    };
    if method != "get" || !arguments.is_empty() {
        return Ok(None);
    }

    match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::I32Reference(slot) => Ok(Some(I32Expr::Local(slot))),
        _ => Ok(None),
    }
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

    match resolve_value(receiver.as_ref(), &state.environment)? {
        Value::I32Reference(slot) => {
            let normalized = normalize_numeric_literal_arguments(arguments);
            let [value] = normalized.as_slice() else {
                return Err(Error::Unsupported(
                    "`set` must receive exactly one argument for `i32` references".to_string(),
                ));
            };
            program.operations.push(Operation::StoreI32 {
                slot,
                value: lower_i32_expr(value, state)?,
            });
        }
        Value::Array {
            slot,
            element_type: ArrayElementType::I32,
        } => {
            let normalized = normalize_numeric_literal_arguments(arguments);
            let [index, value] = normalized.as_slice() else {
                return Err(Error::Unsupported(
                    "`set` must receive exactly two arguments for arrays".to_string(),
                ));
            };
            program.operations.push(Operation::ArraySetI32 {
                array_slot: slot,
                index: lower_i32_expr(index, state)?,
                value: lower_i32_expr(value, state)?,
            });
        }
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
        } => {
            let normalized = normalize_numeric_literal_arguments(arguments);
            let [index, value] = normalized.as_slice() else {
                return Err(Error::Unsupported(
                    "`set` must receive exactly two arguments for arrays".to_string(),
                ));
            };
            program.operations.push(Operation::ArraySetU8 {
                array_slot: slot,
                index: lower_i32_expr(index, state)?,
                value: lower_u8_expr(value, state)?,
            });
        }
        other => {
            return Err(Error::Unsupported(format!(
                "`set` expects an `i32` reference or array reference, got {other:?}"
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
    parse_prefixed_i32_digits(digits).map_err(|_| {
        Error::Unsupported(format!(
            "integer literal `{original}` could not be parsed as i32"
        ))
    })
}

fn parse_u8_digits(digits: &str, original: &str) -> Result<u8> {
    parse_prefixed_u8_digits(digits).map_err(|_| {
        Error::Unsupported(format!(
            "integer literal `{original}` could not be parsed as u8"
        ))
    })
}

fn parse_prefixed_i32_digits(digits: &str) -> std::result::Result<i32, std::num::ParseIntError> {
    if let Some(hex) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
    {
        i32::from_str_radix(hex, 16)
    } else {
        digits.parse::<i32>()
    }
}

fn parse_prefixed_u8_digits(digits: &str) -> std::result::Result<u8, std::num::ParseIntError> {
    if let Some(hex) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
    {
        u8::from_str_radix(hex, 16)
    } else {
        digits.parse::<u8>()
    }
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

pub(crate) fn intern_data(program: &mut LoweredProgram, bytes: Vec<u8>) -> usize {
    program.data.push(bytes);
    program.data.len() - 1
}

fn nul_terminated_bytes(value: &str) -> Vec<u8> {
    let mut bytes = value.as_bytes().to_vec();
    bytes.push(0);
    bytes
}

fn build_linux_x86_64_program_executable(program: &LoweredProgram) -> Elf64Executable {
    let code_virtual_address = 0x401000;
    let data_virtual_address = 0x402000;
    let mut elf = Elf64Executable::new(code_virtual_address);
    elf.add_load_segment(LoadSegment::new(
        code_virtual_address,
        0,
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

    emit_operations(
        &program.operations,
        &mut code,
        program,
        &addresses,
        None,
        None,
    );

    code
}

fn emit_operations(
    operations: &[Operation],
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    addresses: &[u64],
    mut break_patches: Option<&mut Vec<usize>>,
    mut continue_patches: Option<&mut Vec<usize>>,
) {
    for operation in operations {
        match operation {
            Operation::StoreI32 { slot, value } => {
                emit_i32_expr_to_eax(value, code, program);
                let slot_offset = i32_slot_offset(program, *slot);
                code.extend_from_slice(&[0x89, 0x85]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
            }
            Operation::Open {
                path_data_index,
                flags,
                mode,
                result_slot,
            } => {
                code.extend_from_slice(&[0x48, 0xbf]);
                code.extend_from_slice(&addresses[*path_data_index].to_le_bytes());
                emit_i32_expr_to_eax(flags, code, program);
                code.extend_from_slice(&[0x89, 0xc6]);
                emit_i32_expr_to_eax(mode, code, program);
                code.extend_from_slice(&[0x89, 0xc2]);
                code.extend_from_slice(&[0xb8, 0x02, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
                let result_offset = i32_slot_offset(program, *result_slot);
                code.extend_from_slice(&[0x89, 0x85]);
                code.extend_from_slice(&result_offset.to_le_bytes());
            }
            Operation::Close { fd } => {
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                code.extend_from_slice(&[0xb8, 0x03, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::Read {
                fd,
                array_slot,
                len,
                result_slot,
            } => {
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                let slot_offset = array_slot_offset(*array_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0xb5]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                emit_i32_expr_to_eax(len, code, program);
                // mov edx, eax
                code.extend_from_slice(&[0x89, 0xc2]);
                // read syscall: 0
                code.extend_from_slice(&[0xb8, 0x00, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
                let result_offset = i32_slot_offset(program, *result_slot);
                // mov [rbp + disp32], eax
                code.extend_from_slice(&[0x89, 0x85]);
                code.extend_from_slice(&result_offset.to_le_bytes());
            }
            Operation::WriteStatic {
                fd,
                data_index,
                len,
            } => {
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                // mov rsi, (imm64): 0x48, 0xbe, (imm64)
                code.extend_from_slice(&[0x48, 0xbe]);
                code.extend_from_slice(&addresses[*data_index].to_le_bytes());
                emit_i32_expr_to_eax(len, code, program);
                // mov edx, eax
                code.extend_from_slice(&[0x89, 0xc2]);
                // mov eax, (imm32): 0xb8, (imm32)
                // write syscall: 1
                code.extend_from_slice(&[0xb8, 0x01, 0x00, 0x00, 0x00]);
                // syscall
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::WriteArray {
                fd,
                array_slot,
                len,
            } => {
                let array = array_allocation(program, *array_slot);
                debug_assert_eq!(array.element_type, ArrayElementType::U8);
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                let slot_offset = array_slot_offset(*array_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0xb5]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                emit_i32_expr_to_eax(len, code, program);
                code.extend_from_slice(&[0x89, 0xc2]);
                code.extend_from_slice(&[0xb8, 0x01, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::If {
                condition,
                then_operations,
                else_operations,
            } => {
                emit_condition_false_jump(condition, code, program);
                let false_patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                let then_start = code.len();
                emit_operations(
                    then_operations,
                    code,
                    program,
                    addresses,
                    break_patches.as_deref_mut(),
                    continue_patches.as_deref_mut(),
                );
                if else_operations.is_empty() {
                    let end = code.len();
                    let false_jump_len = (end - then_start) as i32;
                    code[false_patch_at..false_patch_at + 4]
                        .copy_from_slice(&false_jump_len.to_le_bytes());
                } else {
                    code.extend_from_slice(&[0xe9]);
                    let end_patch_at = code.len();
                    code.extend_from_slice(&0i32.to_le_bytes());
                    let else_start = code.len();
                    emit_operations(
                        else_operations,
                        code,
                        program,
                        addresses,
                        break_patches.as_deref_mut(),
                        continue_patches.as_deref_mut(),
                    );
                    let end = code.len();
                    let false_jump_len = (else_start - then_start) as i32;
                    code[false_patch_at..false_patch_at + 4]
                        .copy_from_slice(&false_jump_len.to_le_bytes());
                    let end_jump_len = (end - else_start) as i32;
                    code[end_patch_at..end_patch_at + 4]
                        .copy_from_slice(&end_jump_len.to_le_bytes());
                }
            }
            Operation::ArraySetI32 {
                array_slot,
                index,
                value,
            } => emit_i32_array_set(*array_slot, index, value, code, program),
            Operation::ArraySetU8 {
                array_slot,
                index,
                value,
            } => emit_u8_array_set(*array_slot, index, value, code, program),
            Operation::Loop { body_operations } => {
                let loop_start = code.len();
                let mut loop_break_patches = Vec::new();
                let mut loop_continue_patches = Vec::new();
                emit_operations(
                    body_operations,
                    code,
                    program,
                    addresses,
                    Some(&mut loop_break_patches),
                    Some(&mut loop_continue_patches),
                );
                code.push(0xe9);
                let back_patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                let loop_end = code.len();
                let back_jump_len = loop_start as i32 - loop_end as i32;
                code[back_patch_at..back_patch_at + 4]
                    .copy_from_slice(&back_jump_len.to_le_bytes());
                for patch_at in loop_continue_patches {
                    let continue_jump_len = loop_start as i32 - (patch_at as i32 + 4);
                    code[patch_at..patch_at + 4].copy_from_slice(&continue_jump_len.to_le_bytes());
                }
                for patch_at in loop_break_patches {
                    let break_jump_len = loop_end as i32 - (patch_at as i32 + 4);
                    code[patch_at..patch_at + 4].copy_from_slice(&break_jump_len.to_le_bytes());
                }
            }
            Operation::Break => {
                code.push(0xe9);
                let patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                break_patches
                    .as_deref_mut()
                    .expect("break must be lowered inside a loop")
                    .push(patch_at);
            }
            Operation::Continue => {
                code.push(0xe9);
                let patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                continue_patches
                    .as_deref_mut()
                    .expect("continue must be lowered inside a loop")
                    .push(patch_at);
            }
            Operation::Exit(exit_code) => {
                emit_exit_code_expr_to_eax(exit_code, code, program);
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
}

fn emit_condition_false_jump(
    condition: &ConditionExpr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    match condition {
        ConditionExpr::I32 { kind, lhs, rhs } => {
            emit_i32_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_i32_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_false(*kind, false, code);
        }
        ConditionExpr::U8 { kind, lhs, rhs } => {
            emit_u8_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_u8_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_false(*kind, true, code);
        }
    }
}

fn emit_jcc_false(kind: ComparisonKind, unsigned: bool, code: &mut Vec<u8>) {
    code.extend_from_slice(&[0x0f, false_jump_opcode(kind, unsigned)]);
}

fn false_jump_opcode(kind: ComparisonKind, unsigned: bool) -> u8 {
    match (kind, unsigned) {
        (ComparisonKind::Eq, _) => 0x85,
        (ComparisonKind::Lte, false) => 0x8f,
        (ComparisonKind::Lt, false) => 0x8d,
        (ComparisonKind::Gte, false) => 0x8c,
        (ComparisonKind::Gt, false) => 0x8e,
        (ComparisonKind::Lte, true) => 0x87,
        (ComparisonKind::Lt, true) => 0x83,
        (ComparisonKind::Gte, true) => 0x82,
        (ComparisonKind::Gt, true) => 0x86,
    }
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
        I32Expr::Local(slot) => {
            let slot_offset = i32_slot_offset(program, *slot);
            // mov eax, [rbp + disp32]
            code.extend_from_slice(&[0x8b, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
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
        U8Expr::ArrayGet { array_slot, index } => {
            emit_u8_array_get(*array_slot, index, code, program)
        }
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
    let i32_slot_bytes = program.i32_slots * 4;
    let array_bytes: usize = program.arrays.iter().map(array_storage_size).sum();
    pointer_bytes + i32_slot_bytes + array_bytes
}

fn array_slot_offset(slot: usize) -> i32 {
    -8 * (slot as i32 + 1)
}

fn array_data_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let pointer_bytes = (program.arrays.len() * 8) as i32;
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let mut offset = pointer_bytes + i32_slot_bytes;
    for array in &program.arrays {
        offset += array_storage_size(array) as i32;
        if array.slot == slot {
            return -offset;
        }
    }
    panic!("unknown array slot {slot}");
}

fn i32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let pointer_bytes = (program.arrays.len() * 8) as i32;
    -(pointer_bytes + 4 * (slot as i32 + 1))
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

    fn repo_root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root")
    }

    fn selected_fixture_package(root: &Path, binary_name: &str) -> ParsedPackage {
        let ParsedRoot::Package(package) = parse_root(root).expect("fixture parses") else {
            panic!("expected package root");
        };
        select_binary_from_package(package, &PathBuf::from(binary_name)).expect("select binary")
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
                    fd: I32Expr::Literal(1),
                    data_index: 0,
                    len: I32Expr::Literal(14),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0)))
            ]
        );
        assert_eq!(program.data, vec![b"Hello, world!\n\0".to_vec()]);
        assert!(program.arrays.is_empty());
        assert_eq!(program.i32_slots, 0);
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
                    fd: I32Expr::Literal(1),
                    array_slot: 0,
                    len: I32Expr::Literal(13),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ]
        );
        assert!(program.data.is_empty());
        assert_eq!(program.i32_slots, 0);
    }

    #[test]
    fn lowers_hex_literals_fixture_to_runtime_array_operations() {
        let root = repo_root().join("tests/testcases/hex-literals");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.arrays,
            vec![ArrayAllocation {
                slot: 0,
                len: 2,
                element_type: ArrayElementType::U8,
            }]
        );
        assert_eq!(
            program.operations,
            vec![
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(0),
                    value: U8Expr::Literal(65),
                },
                Operation::ArraySetU8 {
                    array_slot: 0,
                    index: I32Expr::Literal(1),
                    value: U8Expr::Literal(10),
                },
                Operation::WriteArray {
                    fd: I32Expr::Literal(1),
                    array_slot: 0,
                    len: I32Expr::Literal(2),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ]
        );
        assert!(program.data.is_empty());
        assert_eq!(program.i32_slots, 0);
    }

    #[test]
    fn lowers_enum_match_basic_fixture_to_runtime_exit() {
        let root = repo_root().join("tests/testcases/enum-match-basic");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))]
        );
        assert!(program.arrays.is_empty());
        assert!(program.data.is_empty());
        assert_eq!(program.i32_slots, 0);
    }

    #[test]
    fn lowers_enum_match_payload_single_fixture_to_runtime_exit() {
        let root = repo_root().join("tests/testcases/enum-match-payload");
        let package = selected_fixture_package(&root, "enum-match-payload-single");

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))]
        );
        assert!(program.arrays.is_empty());
        assert!(program.data.is_empty());
        assert_eq!(program.i32_slots, 0);
    }

    #[test]
    fn lowers_enum_match_payload_pair_fixture_to_runtime_exit() {
        let root = repo_root().join("tests/testcases/enum-match-payload");
        let package = selected_fixture_package(&root, "enum-match-payload-pair");

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Add(
                Box::new(I32Expr::Literal(20)),
                Box::new(I32Expr::Literal(22)),
            )))]
        );
        assert!(program.arrays.is_empty());
        assert!(program.data.is_empty());
        assert_eq!(program.i32_slots, 0);
    }

    #[test]
    fn lowers_if_else_true_fixture_to_program() {
        let root = repo_root().join("tests/testcases/if");
        let source_path = root.join("src/if-else-true.fe");
        let source = std::fs::read_to_string(&source_path).expect("read fixture source");
        let (tokens, syntax) = neco_rs_parser::parse_source(&source).expect("parse source");
        let package = ParsedPackage {
            root_dir: root.clone(),
            manifest_path: root.join("neco-package.json"),
            manifest: neco_rs_parser::PackageManifest {
                name: "if".to_string(),
                dependencies: Vec::new(),
                felis_lib_entrypoint: None,
                felis_bin_entrypoints: vec![PathBuf::from("src/if-else-true.fe")],
            },
            source_files: vec![neco_rs_parser::ParsedSourceFile {
                path: source_path,
                role: neco_rs_parser::SourceFileRole::BinaryEntrypoint,
                tokens,
                syntax: syntax.expect("source file syntax"),
            }],
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![
                Operation::If {
                    condition: ConditionExpr::I32 {
                        kind: ComparisonKind::Eq,
                        lhs: I32Expr::Literal(3),
                        rhs: I32Expr::Literal(3),
                    },
                    then_operations: vec![Operation::Exit(ExitCodeExpr::I32(
                        I32Expr::Literal(42,)
                    ))],
                    else_operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(1,)))],
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ]
        );
    }

    #[test]
    fn lowers_loop_fixture_to_runtime_operations() {
        let root = repo_root().join("tests/testcases/loop");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![
                Operation::StoreI32 {
                    slot: 0,
                    value: I32Expr::Literal(1),
                },
                Operation::StoreI32 {
                    slot: 1,
                    value: I32Expr::Literal(0),
                },
                Operation::Loop {
                    body_operations: vec![
                        Operation::StoreI32 {
                            slot: 1,
                            value: I32Expr::Add(
                                Box::new(I32Expr::Local(1)),
                                Box::new(I32Expr::Local(0)),
                            ),
                        },
                        Operation::If {
                            condition: ConditionExpr::I32 {
                                kind: ComparisonKind::Eq,
                                lhs: I32Expr::Local(0),
                                rhs: I32Expr::Literal(10),
                            },
                            then_operations: vec![Operation::Break],
                            else_operations: vec![],
                        },
                        Operation::StoreI32 {
                            slot: 0,
                            value: I32Expr::Add(
                                Box::new(I32Expr::Local(0)),
                                Box::new(I32Expr::Literal(1)),
                            ),
                        },
                    ],
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(1))),
            ]
        );
        assert_eq!(program.i32_slots, 2);
    }

    #[test]
    fn lowers_continue_fixture_to_runtime_operations() {
        let root = repo_root().join("tests/testcases/continue");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.operations,
            vec![
                Operation::StoreI32 {
                    slot: 0,
                    value: I32Expr::Literal(0),
                },
                Operation::StoreI32 {
                    slot: 1,
                    value: I32Expr::Literal(0),
                },
                Operation::Loop {
                    body_operations: vec![
                        Operation::StoreI32 {
                            slot: 0,
                            value: I32Expr::Add(
                                Box::new(I32Expr::Local(0)),
                                Box::new(I32Expr::Literal(1)),
                            ),
                        },
                        Operation::If {
                            condition: ConditionExpr::I32 {
                                kind: ComparisonKind::Eq,
                                lhs: I32Expr::Local(0),
                                rhs: I32Expr::Literal(5),
                            },
                            then_operations: vec![Operation::Continue],
                            else_operations: vec![],
                        },
                        Operation::StoreI32 {
                            slot: 1,
                            value: I32Expr::Add(
                                Box::new(I32Expr::Local(1)),
                                Box::new(I32Expr::Local(0)),
                            ),
                        },
                        Operation::If {
                            condition: ConditionExpr::I32 {
                                kind: ComparisonKind::Eq,
                                lhs: I32Expr::Local(0),
                                rhs: I32Expr::Literal(10),
                            },
                            then_operations: vec![Operation::Break],
                            else_operations: vec![],
                        },
                    ],
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Local(1))),
            ]
        );
        assert_eq!(program.i32_slots, 2);
    }

    #[test]
    fn lowers_stdin_to_stdout_fixture_to_runtime_io_operations() {
        let root = repo_root().join("tests/testcases/stdin-to-stdout");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.arrays,
            vec![ArrayAllocation {
                slot: 0,
                len: 1000,
                element_type: ArrayElementType::U8,
            }]
        );
        assert_eq!(
            program.operations,
            vec![
                Operation::Read {
                    fd: I32Expr::Literal(0),
                    array_slot: 0,
                    len: I32Expr::Literal(1000),
                    result_slot: 0,
                },
                Operation::WriteArray {
                    fd: I32Expr::Literal(1),
                    array_slot: 0,
                    len: I32Expr::Local(0),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ]
        );
        assert!(program.data.is_empty());
        assert_eq!(program.i32_slots, 1);
    }

    #[test]
    fn lowers_open_read_close_fixture_to_runtime_io_operations() {
        let root = repo_root().join("tests/testcases/open-read-close");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert_eq!(
            program.arrays,
            vec![ArrayAllocation {
                slot: 0,
                len: 128,
                element_type: ArrayElementType::U8,
            }]
        );
        assert_eq!(
            program.operations,
            vec![
                Operation::Open {
                    path_data_index: 0,
                    flags: I32Expr::Literal(0),
                    mode: I32Expr::Literal(0),
                    result_slot: 0,
                },
                Operation::Read {
                    fd: I32Expr::Local(0),
                    array_slot: 0,
                    len: I32Expr::Literal(128),
                    result_slot: 1,
                },
                Operation::Close {
                    fd: I32Expr::Local(0),
                },
                Operation::WriteArray {
                    fd: I32Expr::Literal(1),
                    array_slot: 0,
                    len: I32Expr::Local(1),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ]
        );
        assert_eq!(program.data, vec![b"message.txt\0".to_vec()]);
        assert_eq!(program.i32_slots, 2);
    }

    #[test]
    fn lowers_open_write_close_fixture_to_runtime_io_operations() {
        let root = repo_root().join("tests/testcases/open-write-close");
        let ParsedRoot::Package(package) = parse_root(&root).expect("fixture parses") else {
            panic!("expected package root");
        };

        let program = lower_package_to_program(&package).expect("lower fixture");
        assert!(program.arrays.is_empty());
        assert_eq!(
            program.operations,
            vec![
                Operation::Open {
                    path_data_index: 0,
                    flags: I32Expr::Literal(577),
                    mode: I32Expr::Literal(420),
                    result_slot: 0,
                },
                Operation::WriteStatic {
                    fd: I32Expr::Local(0),
                    data_index: 1,
                    len: I32Expr::Literal(25),
                },
                Operation::Close {
                    fd: I32Expr::Local(0),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ]
        );
        assert_eq!(
            program.data,
            vec![
                b"created.txt\0".to_vec(),
                b"open/write/close fixture\n\0".to_vec(),
            ]
        );
        assert_eq!(program.i32_slots, 1);
    }

    #[test]
    fn lowers_fn_call_fixture_to_runtime_expression_tree() {
        let root = repo_root().join("tests/testcases/fn-call");
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
        assert_eq!(program.i32_slots, 0);
    }

    #[test]
    fn builds_elf_image_with_exit_syscall() {
        let program = LoweredProgram {
            operations: vec![Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(42)))],
            data: Vec::new(),
            arrays: Vec::new(),
            i32_slots: 0,
        };
        let elf = build_linux_x86_64_program_executable(&program)
            .to_bytes()
            .expect("serialize ELF");
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
                    fd: I32Expr::Literal(1),
                    data_index: 0,
                    len: I32Expr::Literal(14),
                },
                Operation::Exit(ExitCodeExpr::I32(I32Expr::Literal(0))),
            ],
            data: vec![b"Hello, world!\n\0".to_vec()],
            arrays: Vec::new(),
            i32_slots: 0,
        };
        let elf = build_linux_x86_64_program_executable(&program)
            .to_bytes()
            .expect("serialize ELF");

        assert_eq!(&elf[0..4], b"\x7FELF");
        assert_eq!(&elf[0x1000..0x1005], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1005..0x1007], &[0x89, 0xc7]);
        assert_eq!(&elf[0x1007..0x1009], &[0x48, 0xbe]);
        assert_eq!(&elf[0x1009..0x1011], &0x402000_u64.to_le_bytes());
        assert_eq!(&elf[0x1011..0x1016], &[0xb8, 14, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1016..0x1018], &[0x89, 0xc2]);
        assert_eq!(&elf[0x1018..0x101d], &[0xb8, 0x01, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x101d..0x101f], &[0x0f, 0x05]);
        assert_eq!(&elf[0x101f..0x1024], &[0xb8, 0x00, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x1024..0x1026], &[0x89, 0xc7]);
        assert_eq!(&elf[0x1026..0x102b], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
        assert_eq!(&elf[0x102b..0x102d], &[0x0f, 0x05]);
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
            i32_slots: 0,
        };
        let elf = build_linux_x86_64_program_executable(&program)
            .to_bytes()
            .expect("serialize ELF");
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
    fn defaults_output_into_package_neco_directory() {
        let root = repo_root().join("tests/testcases/exit-42");
        let output = default_output_path(&root);
        assert_eq!(output, root.join(".neco").join("exit-42"));
    }
}
