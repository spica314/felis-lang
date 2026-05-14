mod declarations;
mod expr;
mod pure;
mod typecheck;

use std::collections::{HashMap, HashSet};
use std::path::Path;

use neco_rs_parser::{
    ArrowParameter, BindingPattern, ElseBranch, Item, LetOperator, MatchExpression, ParsedPackage,
    ParsedRoot, ParsedWorkspace, Statement, Term, parse_root,
};

use crate::effect::{Value, bind_pattern, lower_effect, resolve_value};
use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, ExitCodeExpr, I32Expr, LoweredProgram, Operation,
    intern_data,
};
use crate::{Error, Result};

use declarations::{
    ConstructorSignature, PureFunction, StatementFunction, StructSignature, collect_constructors,
    collect_pure_functions, collect_statement_functions, collect_structs, pure_function_from_decl,
};
use expr::lower_condition_expr;
use pure::{
    lower_function_call_statement, lower_function_call_value, lower_pure_block_value,
    lower_pure_value, pattern_match_bindings, substitute_type_bindings,
};
pub(crate) use typecheck::validate_value_against_type;

pub(crate) use expr::{
    lower_array_index_expr, lower_bool_expr, lower_f32_expr, lower_i32_expr, lower_i64_expr,
    lower_u8_expr, normalize_numeric_literal_arguments,
};

#[derive(Clone)]
pub(crate) struct LoweringState {
    pub(crate) environment: HashMap<String, Value>,
    pub(crate) next_array_slot: usize,
    pub(crate) next_i32_slot: usize,
    pub(crate) next_i64_slot: usize,
    pub(crate) next_f32_slot: usize,
    pub(crate) io_effect_allowed: bool,
    pub(crate) compiled_ptx_function_names: HashMap<usize, usize>,
    functions: HashMap<String, PureFunction>,
    statement_functions: HashMap<String, StatementFunction>,
    constructors: HashMap<String, ConstructorSignature>,
    structs: HashMap<String, StructSignature>,
    loop_depth: usize,
}

impl LoweringState {
    fn new() -> Self {
        Self {
            environment: HashMap::new(),
            next_array_slot: 0,
            next_i32_slot: 0,
            next_i64_slot: 0,
            next_f32_slot: 0,
            io_effect_allowed: false,
            compiled_ptx_function_names: HashMap::new(),
            functions: HashMap::new(),
            statement_functions: HashMap::new(),
            constructors: HashMap::new(),
            structs: HashMap::new(),
            loop_depth: 0,
        }
    }

    fn child_scope(&self) -> Self {
        self.clone()
    }

    pub(crate) fn allocate_array(
        &mut self,
        element_type: ArrayElementType,
        kind: ArrayKind,
        len: usize,
        program: &mut LoweredProgram,
    ) -> usize {
        let slot = self.next_array_slot;
        self.next_array_slot += 1;
        program.arrays.push(ArrayAllocation {
            slot,
            len,
            element_type,
            kind,
        });
        slot
    }

    pub(crate) fn allocate_i32_slot(&mut self) -> usize {
        let slot = self.next_i32_slot;
        self.next_i32_slot += 1;
        slot
    }

    pub(crate) fn allocate_i64_slot(&mut self) -> usize {
        let slot = self.next_i64_slot;
        self.next_i64_slot += 1;
        slot
    }

    pub(crate) fn allocate_f32_slot(&mut self) -> usize {
        let slot = self.next_f32_slot;
        self.next_f32_slot += 1;
        slot
    }
}

pub(super) fn allocate_heap_slot(program: &mut LoweredProgram) -> usize {
    let slot = program.heap_slots;
    program.heap_slots += 1;
    slot
}

pub(crate) fn lower_package_to_program(package: &ParsedPackage) -> Result<LoweredProgram> {
    let entrypoint_names = package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
        .filter_map(|item| match item {
            Item::EntryPoint(entrypoint) => Some(entrypoint.token_ident.lexeme.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let [entrypoint_name] = entrypoint_names.as_slice() else {
        return match entrypoint_names.as_slice() {
            [] => Err(Error::Unsupported(
                "missing #entrypoint declaration".to_string(),
            )),
            names => Err(Error::Unsupported(format!(
                "multiple #entrypoint declarations are not supported: {}",
                names.join(", ")
            ))),
        };
    };

    let main_fn = package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
        .find_map(|item| match item {
            Item::Function(function) if function.name.name == *entrypoint_name => Some(function),
            _ => None,
        })
        .ok_or_else(|| {
            Error::Unsupported(format!(
                "missing function body for entrypoint `{entrypoint_name}`"
            ))
        })?;

    validate_entrypoint_signature(main_fn)?;

    if !matches!(main_fn.body.tail.as_deref(), Some(Term::Unit)) {
        return Err(Error::Unsupported(
            "entrypoint body must end with `()`".to_string(),
        ));
    }

    let mut program = LoweredProgram {
        operations: Vec::new(),
        data: Vec::new(),
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        requires_argv: false,
    };
    let mut state = LoweringState::new();
    let callable_packages = collect_callable_packages(package)?;
    state.functions = collect_pure_functions(&callable_packages)?;
    state.statement_functions = collect_statement_functions(&callable_packages)?;
    state.constructors = collect_constructors(&callable_packages)?;
    state.structs = collect_structs(&callable_packages)?;
    initialize_compile_ptx_bindings(package, &mut state, &mut program)?;
    initialize_zero_arg_use_bindings(package, &callable_packages, &mut state, &mut program)?;
    state.io_effect_allowed = function_has_io_effect(main_fn);
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
    program.i64_slots = state.next_i64_slot;
    program.f32_slots = state.next_f32_slot;

    Ok(program)
}

fn validate_entrypoint_signature(function: &neco_rs_parser::FunctionDeclaration) -> Result<()> {
    if function
        .effect
        .as_ref()
        .is_some_and(|effect| effect.lexeme != "IO")
    {
        return Err(Error::Unsupported(format!(
            "entrypoint `{}` effect must be `IO`",
            function.name.name
        )));
    }

    let mut parameter_count = 0;
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        parameter_count += 1;
        current = arrow.result.as_ref();
    }

    if parameter_count != 0 {
        return Err(Error::Unsupported(format!(
            "entrypoint `{}` must not declare parameters",
            function.name.name
        )));
    }

    if !matches!(current, Term::Unit) {
        return Err(Error::Unsupported(format!(
            "entrypoint `{}` must declare result type `()`",
            function.name.name
        )));
    }

    Ok(())
}

fn function_has_io_effect(function: &neco_rs_parser::FunctionDeclaration) -> bool {
    function
        .effect
        .as_ref()
        .is_some_and(|effect| effect.lexeme == "IO")
}

fn initialize_compile_ptx_bindings(
    package: &ParsedPackage,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::CompilePtx(compile_ptx) = item else {
            continue;
        };
        let function = package
            .source_files
            .iter()
            .flat_map(|file| file.syntax.items.iter())
            .find_map(|item| match item {
                Item::Function(function) if function.name.name == compile_ptx.function_name => {
                    Some(function)
                }
                _ => None,
            })
            .ok_or_else(|| {
                Error::Unsupported(format!(
                    "`#compile_ptx` target `{}` was not found",
                    compile_ptx.function_name
                ))
            })?;
        let ptx = compile_empty_ptx_function(function)?;
        let len = i32::try_from(ptx.len()).map_err(|_| {
            Error::Unsupported(format!(
                "compiled PTX for `{}` is too large",
                compile_ptx.function_name
            ))
        })?;
        let data_index = intern_data(program, ptx);
        let name_data_index = intern_data(program, nul_terminated_identifier(&function.name.name));
        state
            .compiled_ptx_function_names
            .insert(data_index, name_data_index);
        let previous = state.environment.insert(
            compile_ptx.value_name.clone(),
            Value::StaticSlice { data_index, len },
        );
        if previous.is_some() {
            return Err(Error::Unsupported(format!(
                "duplicate compiled PTX value `{}`",
                compile_ptx.value_name
            )));
        }
    }
    Ok(())
}

fn compile_empty_ptx_function(function: &neco_rs_parser::FunctionDeclaration) -> Result<Vec<u8>> {
    if !function
        .effect
        .as_ref()
        .is_some_and(|effect| effect.lexeme == "PTX")
    {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` target `{}` must use `#with PTX`",
            function.name.name
        )));
    }
    validate_empty_ptx_signature(function)?;
    if !function.body.statements.is_empty()
        || !matches!(function.body.tail.as_deref(), Some(Term::Unit))
    {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only empty PTX function bodies for `{}`",
            function.name.name
        )));
    }

    let mut ptx = format!(
        ".version 7.0\n.target sm_52\n.address_size 64\n\n.visible .entry {}(\n    .param {} arg0\n)\n{{\n    ret;\n}}\n",
        function.name.name,
        ptx_parameter_type(function)?
    )
    .into_bytes();
    ptx.push(0);
    Ok(ptx)
}

fn validate_empty_ptx_signature(function: &neco_rs_parser::FunctionDeclaration) -> Result<()> {
    let current = &function.ty;
    let Term::Arrow(arrow) = current else {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only one-argument PTX functions for `{}`",
            function.name.name
        )));
    };
    validate_ptx_parameter_type(&arrow.parameter, &function.name.name)?;
    let current = arrow.result.as_ref();
    if matches!(current, Term::Arrow(_)) {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only one-argument PTX functions for `{}`",
            function.name.name
        )));
    }
    if !matches!(current, Term::Unit) {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only PTX functions returning `()` for `{}`",
            function.name.name
        )));
    }
    Ok(())
}

fn ptx_parameter_type(function: &neco_rs_parser::FunctionDeclaration) -> Result<&'static str> {
    let Term::Arrow(arrow) = &function.ty else {
        unreachable!("validated PTX functions always have one parameter");
    };
    ptx_parameter_type_for_arrow_parameter(&arrow.parameter, &function.name.name)
}

fn validate_ptx_parameter_type(parameter: &ArrowParameter, function_name: &str) -> Result<()> {
    ptx_parameter_type_for_arrow_parameter(parameter, function_name).map(|_| ())
}

fn ptx_parameter_type_for_arrow_parameter(
    parameter: &ArrowParameter,
    function_name: &str,
) -> Result<&'static str> {
    let ty = match parameter {
        ArrowParameter::Binder(binder) => binder.ty.as_ref(),
        ArrowParameter::Domain(ty) => ty.as_ref(),
    };
    match simple_type_name(ty) {
        Some("i32") => Ok(".u32"),
        Some("i64") => Ok(".u64"),
        Some("f32") => Ok(".f32"),
        Some("u8") => Ok(".u8"),
        _ => Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only primitive i32/i64/f32/u8 parameters for `{function_name}`"
        ))),
    }
}

fn simple_type_name(ty: &Term) -> Option<&str> {
    let Term::Path(path) = ty else {
        return None;
    };
    if path.token_keyword_package.is_none() && path.segments.len() == 1 {
        Some(path.segments[0].lexeme.as_str())
    } else {
        None
    }
}

fn nul_terminated_identifier(name: &str) -> Vec<u8> {
    let mut bytes = name.as_bytes().to_vec();
    bytes.push(0);
    bytes
}

fn initialize_zero_arg_use_bindings(
    package: &ParsedPackage,
    available_packages: &[ParsedPackage],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Use(use_decl) = item else {
            continue;
        };

        let Some(function) =
            resolve_zero_arg_imported_pure_function(package, available_packages, &use_decl.path)?
        else {
            continue;
        };

        let imported_name = use_decl
            .path
            .segments
            .last()
            .expect("use path always has at least one segment")
            .lexeme
            .clone();
        let value = lower_pure_block_value(&function.body, state, program)?;
        validate_value_against_type(&value, &function.result_ty, program)?;
        state.environment.insert(imported_name, value);
    }

    Ok(())
}

fn resolve_zero_arg_imported_pure_function(
    package: &ParsedPackage,
    available_packages: &[ParsedPackage],
    path: &neco_rs_parser::PathExpression,
) -> Result<Option<PureFunction>> {
    let Some(last_segment) = path.segments.last() else {
        return Ok(None);
    };
    let target_package = if path.token_keyword_package.is_some() || path.segments.len() == 1 {
        package
    } else {
        let package_name = &path.segments[0].lexeme;
        available_packages
            .iter()
            .find(|candidate| candidate.manifest.name == *package_name)
            .unwrap_or(package)
    };

    for item in target_package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::Function(function) = item else {
            continue;
        };
        if function.effect.is_some() {
            continue;
        }
        if function.name.name != last_segment.lexeme {
            continue;
        }

        let function = pure_function_from_decl(function)?;
        if function.parameters.is_empty() {
            return Ok(Some(function));
        }
        return Ok(None);
    }

    Ok(None)
}

fn collect_callable_packages(package: &ParsedPackage) -> Result<Vec<ParsedPackage>> {
    let mut packages = resolve_workspace_dependency_packages(package)?;
    if let Some(std_core) = find_std_core_package(package)? {
        packages.push(std_core);
    }
    packages.push(package.clone());
    Ok(packages)
}

fn resolve_workspace_dependency_packages(package: &ParsedPackage) -> Result<Vec<ParsedPackage>> {
    if package.manifest.dependencies.is_empty() {
        return Ok(Vec::new());
    }

    let Some(workspace) = find_workspace_for_package(package)? else {
        return Ok(Vec::new());
    };

    let mut packages = Vec::new();
    for dependency in &package.manifest.dependencies {
        let dependency_package = workspace
            .packages
            .iter()
            .find(|candidate| candidate.manifest.name == dependency.name)
            .cloned()
            .ok_or_else(|| {
                Error::Unsupported(format!(
                    "workspace dependency `{}` could not be resolved for lowering",
                    dependency.name
                ))
            })?;
        packages.push(dependency_package);
    }
    Ok(packages)
}

fn find_workspace_for_package(package: &ParsedPackage) -> Result<Option<ParsedWorkspace>> {
    let mut current = package.root_dir.parent();
    while let Some(candidate) = current {
        let manifest_path = candidate.join("neco-package.json");
        if manifest_path.exists()
            && let Ok(ParsedRoot::Workspace(workspace)) = parse_root(candidate)
            && workspace
                .packages
                .iter()
                .any(|member| paths_equal(&member.root_dir, &package.root_dir))
        {
            return Ok(Some(workspace));
        }
        current = candidate.parent();
    }
    Ok(None)
}

fn find_std_core_package(package: &ParsedPackage) -> Result<Option<ParsedPackage>> {
    for ancestor in package.root_dir.ancestors() {
        let candidate = ancestor.join("std/std_core");
        if candidate.join("neco-package.json").exists()
            && let Ok(ParsedRoot::Package(std_core)) = parse_root(&candidate)
        {
            return Ok(Some(std_core));
        }
    }
    Ok(None)
}

fn paths_equal(lhs: &Path, rhs: &Path) -> bool {
    lhs == rhs
}

pub(super) fn lower_statement(
    statement: &Statement,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    match statement {
        Statement::Let(let_stmt) => match let_stmt.operator {
            LetOperator::Equals => {
                ensure_not_effectful_let_value(let_stmt.value.as_ref(), state)?;
                ensure_no_nested_io_effects(let_stmt.value.as_ref(), state)?;
                lower_let_equals_statement(
                    &let_stmt.binder,
                    let_stmt.ty.as_ref(),
                    let_stmt.value.as_ref(),
                    state,
                    program,
                )?;
                Ok(false)
            }
            LetOperator::LeftArrow => {
                ensure_io_effect_allowed(state, "effectful operation")?;
                ensure_no_nested_io_effects(let_stmt.value.as_ref(), state)?;
                if let Some(value) =
                    lower_function_call_value(let_stmt.value.as_ref(), state, program)?
                {
                    let ty = substitute_type_bindings(
                        let_stmt.ty.as_ref(),
                        &type_bindings_from_environment(&state.environment),
                    );
                    validate_value_against_type(&value, &ty, program)?;
                    let value = wrap_reference_value(value, &ty);
                    bind_pattern(&let_stmt.binder, value, &mut state.environment);
                    return Ok(false);
                }
                let ty = substitute_type_bindings(
                    let_stmt.ty.as_ref(),
                    &type_bindings_from_environment(&state.environment),
                );
                lower_effect(
                    &let_stmt.binder,
                    &ty,
                    let_stmt.value.as_ref(),
                    state,
                    program,
                )
            }
        },
        Statement::LetRef(letref_stmt) => {
            let ty = substitute_type_bindings(
                letref_stmt.ty.as_ref(),
                &type_bindings_from_environment(&state.environment),
            );
            lower_letref_borrow_statement(
                &letref_stmt.reference,
                letref_stmt.exclusive,
                &ty,
                letref_stmt.source.as_ref(),
                state,
                program,
            )?;
            Ok(false)
        }
        Statement::Expression(term) => {
            lower_expression_statement(term.as_ref(), state, program)?;
            Ok(false)
        }
        Statement::If(if_stmt) => lower_if_statement(if_stmt, state, program),
        Statement::Loop(loop_stmt) => {
            let mut loop_state = state.child_scope();
            loop_state.loop_depth += 1;
            let mut loop_program = LoweredProgram {
                operations: Vec::new(),
                data: std::mem::take(&mut program.data),
                arrays: std::mem::take(&mut program.arrays),
                heap_slots: program.heap_slots,
                i32_slots: program.i32_slots,
                i64_slots: program.i64_slots,
                f32_slots: program.f32_slots,
                requires_argv: program.requires_argv,
            };
            let mut terminated = false;
            for statement in &loop_stmt.body.statements {
                if terminated {
                    return Err(Error::Unsupported(
                        "statements after loop control are not supported".to_string(),
                    ));
                }
                terminated = lower_statement(statement, &mut loop_state, &mut loop_program)?;
            }
            program.data = loop_program.data;
            program.arrays = loop_program.arrays;
            program.heap_slots = program.heap_slots.max(loop_program.heap_slots);
            program.requires_argv = loop_program.requires_argv;
            state.next_array_slot = state.next_array_slot.max(loop_state.next_array_slot);
            state.next_i32_slot = state.next_i32_slot.max(loop_state.next_i32_slot);
            state.next_i64_slot = state.next_i64_slot.max(loop_state.next_i64_slot);
            state.next_f32_slot = state.next_f32_slot.max(loop_state.next_f32_slot);
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
            Ok(true)
        }
        Statement::Continue => {
            if state.loop_depth == 0 {
                return Err(Error::Unsupported(
                    "`#continue` is only supported inside `#loop`".to_string(),
                ));
            }
            program.operations.push(Operation::Continue);
            Ok(true)
        }
        Statement::Item(_) => Err(Error::Unsupported(
            "items inside entrypoint bodies are not supported".to_string(),
        )),
    }
}

fn lower_if_statement(
    if_stmt: &neco_rs_parser::IfStatement,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    ensure_no_io_effects(if_stmt.condition.as_ref(), state)?;
    let condition = lower_condition_expr(if_stmt.condition.as_ref(), state, program)?;
    let mut then_state = state.child_scope();
    let mut then_operations = Vec::new();
    let mut then_program = LoweredProgram {
        operations: Vec::new(),
        data: std::mem::take(&mut program.data),
        arrays: std::mem::take(&mut program.arrays),
        heap_slots: program.heap_slots,
        i32_slots: program.i32_slots,
        i64_slots: program.i64_slots,
        f32_slots: program.f32_slots,
        requires_argv: program.requires_argv,
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
    let mut next_i64_slot = then_state.next_i64_slot;
    let mut next_f32_slot = then_state.next_f32_slot;

    if let Some(else_branch) = &if_stmt.else_branch {
        let mut else_state = state.child_scope();
        let mut else_program = LoweredProgram {
            operations: Vec::new(),
            data: then_program.data,
            arrays: then_program.arrays,
            heap_slots: then_program.heap_slots,
            i32_slots: then_program.i32_slots,
            i64_slots: then_program.i64_slots,
            f32_slots: then_program.f32_slots,
            requires_argv: then_program.requires_argv,
        };
        match else_branch {
            ElseBranch::Block(else_block) => {
                for statement in &else_block.statements {
                    let terminated =
                        lower_statement(statement, &mut else_state, &mut else_program)?;
                    if terminated {
                        break;
                    }
                }
            }
            ElseBranch::If(else_if) => {
                lower_if_statement(else_if, &mut else_state, &mut else_program)?;
            }
        }
        else_operations.append(&mut else_program.operations);
        program.data = else_program.data;
        program.arrays = else_program.arrays;
        program.heap_slots = program.heap_slots.max(else_program.heap_slots);
        program.requires_argv = else_program.requires_argv;
        next_array_slot = next_array_slot.max(else_state.next_array_slot);
        next_i32_slot = next_i32_slot.max(else_state.next_i32_slot);
        next_i64_slot = next_i64_slot.max(else_state.next_i64_slot);
        next_f32_slot = next_f32_slot.max(else_state.next_f32_slot);
    } else {
        program.data = then_program.data;
        program.arrays = then_program.arrays;
        program.heap_slots = program.heap_slots.max(then_program.heap_slots);
        program.requires_argv = then_program.requires_argv;
    }
    state.next_array_slot = next_array_slot;
    state.next_i32_slot = next_i32_slot;
    state.next_i64_slot = next_i64_slot;
    state.next_f32_slot = next_f32_slot;
    program.operations.push(Operation::If {
        condition,
        then_operations,
        else_operations,
    });
    Ok(false)
}

fn lower_let_equals_statement(
    binder: &BindingPattern,
    ty: &Term,
    value_term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    let value = match lower_function_call_value(value_term, state, program)? {
        Some(value) => value,
        None => lower_pure_value(value_term, state, program)?,
    };
    let ty = substitute_type_bindings(ty, &type_bindings_from_environment(&state.environment));
    validate_value_against_type(&value, &ty, program)?;
    let value = wrap_reference_value(value, &ty);
    bind_pattern(binder, value, &mut state.environment);
    Ok(())
}

fn wrap_reference_value(value: Value, ty: &Term) -> Value {
    let Term::Reference { exclusive, .. } = ty else {
        return value;
    };
    match value {
        Value::I32Reference { .. }
        | Value::I64Reference { .. }
        | Value::F32Reference { .. }
        | Value::Reference { .. }
        | Value::StaticSlice { .. }
        | Value::RuntimeArg(_)
        | Value::PathBuf { .. }
        | Value::Array { .. } => value,
        value => Value::Reference {
            value: Box::new(value),
            exclusive: *exclusive,
        },
    }
}

fn type_bindings_from_environment(environment: &HashMap<String, Value>) -> HashMap<String, Term> {
    environment
        .iter()
        .filter_map(|(name, value)| match value {
            Value::Type(ty) => Some((name.clone(), ty.clone())),
            _ => None,
        })
        .collect()
}

fn lower_letref_borrow_statement(
    reference: &str,
    exclusive: bool,
    ty: &Term,
    source: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    validate_letref_annotation(exclusive, ty)?;
    let value = resolve_value(source, &state.environment)?;
    let reference_value = match value {
        Value::I32(expr) => {
            let slot = state.allocate_i32_slot();
            program
                .operations
                .push(Operation::StoreI32 { slot, value: expr });
            Value::I32Reference { slot, exclusive }
        }
        Value::I64(expr) => {
            let slot = state.allocate_i64_slot();
            program
                .operations
                .push(Operation::StoreI64 { slot, value: expr });
            Value::I64Reference { slot, exclusive }
        }
        Value::F32(expr) => {
            let slot = state.allocate_f32_slot();
            program
                .operations
                .push(Operation::StoreF32 { slot, value: expr });
            Value::F32Reference { slot, exclusive }
        }
        other => other,
    };
    validate_value_against_type(&reference_value, ty, program)?;
    let reference_value = wrap_reference_value(reference_value, ty);
    state
        .environment
        .insert(reference.to_string(), reference_value);
    Ok(())
}

fn validate_letref_annotation(exclusive: bool, ty: &Term) -> Result<()> {
    let Term::Reference {
        exclusive: annotated_exclusive,
        ..
    } = ty
    else {
        return Err(Error::Unsupported(
            "`#letref` requires a reference type annotation".to_string(),
        ));
    };
    if *annotated_exclusive != exclusive {
        return Err(Error::Unsupported(
            "`#letref #excl` requires `&^`, and `#letref` requires `&`".to_string(),
        ));
    }
    Ok(())
}

fn lower_expression_statement(
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    ensure_no_nested_io_effects(term, state)?;
    if let Term::Block(block) = term {
        lower_effectful_block(block, state, program)?;
        return Ok(());
    }
    if matches!(term, Term::Unit) {
        return Ok(());
    }
    if let Term::Match(match_expr) = term {
        lower_match_expression_statement(match_expr, state, program)?;
        return Ok(());
    }
    if lower_reference_set_builtin_statement(term, state, program)? {
        return Ok(());
    }
    if lower_array_set_builtin_statement(term, state, program)? {
        return Ok(());
    }
    if lower_function_call_statement(term, state, program)?.is_some() {
        return Ok(());
    }

    Err(Error::Unsupported(format!(
        "unsupported expression statement in entrypoint body: {term:?}"
    )))
}

fn lower_array_set_builtin_statement(
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    let Term::Application { callee, arguments } = term else {
        return Ok(false);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(false);
    };
    if path.token_keyword_package.is_some()
        || path.segments.len() != 1
        || path.segments[0].lexeme != "array_set"
    {
        return Ok(false);
    }
    ensure_io_effect_allowed(state, "array_set")?;
    lower_array_set_statement(callee.as_ref(), arguments, state, program)?;
    Ok(true)
}

fn lower_array_set_statement(
    callee: &Term,
    arguments: &[Term],
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    let Some((receiver, normalized_arguments)) = array_set_call_parts(callee, arguments)? else {
        return Err(Error::Unsupported(format!(
            "unsupported expression statement in entrypoint body: {callee:?}"
        )));
    };

    match crate::effect::resolve_value(&receiver, &state.environment)? {
        Value::Array {
            slot,
            element_type: ArrayElementType::I32,
            ..
        } => {
            let [index, value] = normalized_arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`set` must receive exactly two arguments for arrays".to_string(),
                ));
            };
            program.operations.push(Operation::ArraySetI32 {
                array_slot: slot,
                index: lower_array_index_expr(index, state)?,
                value: lower_i32_expr(value, state)?,
            });
        }
        Value::Array {
            slot,
            element_type: ArrayElementType::I64,
            ..
        } => {
            let [index, value] = normalized_arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`set` must receive exactly two arguments for arrays".to_string(),
                ));
            };
            program.operations.push(Operation::ArraySetI64 {
                array_slot: slot,
                index: lower_array_index_expr(index, state)?,
                value: lower_i64_expr(value, state)?,
            });
        }
        Value::Array {
            slot,
            element_type: ArrayElementType::F32,
            ..
        } => {
            let [index, value] = normalized_arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`set` must receive exactly two arguments for arrays".to_string(),
                ));
            };
            program.operations.push(Operation::ArraySetF32 {
                array_slot: slot,
                index: lower_array_index_expr(index, state)?,
                value: lower_f32_expr(value, state)?,
            });
        }
        Value::Array {
            slot,
            element_type: ArrayElementType::U8,
            ..
        } => {
            let [index, value] = normalized_arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`set` must receive exactly two arguments for arrays".to_string(),
                ));
            };
            program.operations.push(Operation::ArraySetU8 {
                array_slot: slot,
                index: lower_array_index_expr(index, state)?,
                value: lower_u8_expr(value, state)?,
            });
        }
        other => {
            return Err(Error::Unsupported(format!(
                "`set` expects an array reference, got {other:?}"
            )));
        }
    }
    Ok(())
}

fn array_set_call_parts(callee: &Term, arguments: &[Term]) -> Result<Option<(Term, Vec<Term>)>> {
    let Term::Path(path) = callee else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some()
        || path.segments.len() != 1
        || path.segments[0].lexeme != "array_set"
    {
        return Ok(None);
    }

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [receiver, index, value] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`array_set` must receive exactly an array, an index, and a value".to_string(),
        ));
    };
    Ok(Some((receiver.clone(), vec![index.clone(), value.clone()])))
}

fn lower_reference_set_builtin_statement(
    term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    let Term::Application { callee, arguments } = term else {
        return Ok(false);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(false);
    };
    if path.token_keyword_package.is_some()
        || path.segments.len() != 1
        || path.segments[0].lexeme != "ref_set"
    {
        return Ok(false);
    }
    ensure_io_effect_allowed(state, "ref_set")?;

    let normalized = normalize_numeric_literal_arguments(arguments);
    let [_ty, receiver, value] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`ref_set` must receive exactly a type, a reference, and a value".to_string(),
        ));
    };

    if let Some(name) = single_segment_path_name(receiver)
        && let Some(current_value) = state.environment.get(name).cloned()
        && let Value::Reference {
            value: referent,
            exclusive: true,
        } = current_value
    {
        let lowered_value = match referent.as_ref() {
            Value::I32(_) => Value::I32(lower_i32_expr(value, state)?),
            Value::I64(_) => Value::I64(lower_i64_expr(value, state)?),
            Value::F32(_) => Value::F32(lower_f32_expr(value, state)?),
            Value::Constructor(_) | Value::Struct(_) => lower_pure_value(value, state, program)?,
            other => {
                return Err(Error::Unsupported(format!(
                    "`ref_set` does not support this reference target: {other:?}"
                )));
            }
        };
        state.environment.insert(
            name.to_string(),
            Value::Reference {
                value: Box::new(lowered_value),
                exclusive: true,
            },
        );
        return Ok(true);
    }

    match crate::effect::resolve_value(receiver, &state.environment)? {
        Value::I32Reference {
            slot,
            exclusive: true,
        } => {
            program.operations.push(Operation::StoreI32 {
                slot,
                value: lower_i32_expr(value, state)?,
            });
        }
        Value::I64Reference {
            slot,
            exclusive: true,
        } => {
            program.operations.push(Operation::StoreI64 {
                slot,
                value: lower_i64_expr(value, state)?,
            });
        }
        Value::F32Reference {
            slot,
            exclusive: true,
        } => {
            program.operations.push(Operation::StoreF32 {
                slot,
                value: lower_f32_expr(value, state)?,
            });
        }
        Value::Reference {
            exclusive: false, ..
        } => {
            return Err(Error::Unsupported(
                "`ref_set` requires an exclusive reference".to_string(),
            ));
        }
        Value::I32Reference {
            exclusive: false, ..
        }
        | Value::I64Reference {
            exclusive: false, ..
        }
        | Value::F32Reference {
            exclusive: false, ..
        } => {
            return Err(Error::Unsupported(
                "`ref_set` requires an exclusive reference".to_string(),
            ));
        }
        other => {
            return Err(Error::Unsupported(format!(
                "`ref_set` expects an integer or value reference, got {other:?}"
            )));
        }
    }

    Ok(true)
}

pub(crate) fn ensure_io_effect_allowed(state: &LoweringState, operation: &str) -> Result<()> {
    if state.io_effect_allowed {
        return Ok(());
    }
    Err(Error::Unsupported(format!(
        "`{operation}` requires `#with IO`"
    )))
}

fn ensure_no_nested_io_effects(term: &Term, state: &LoweringState) -> Result<()> {
    ensure_no_io_effects_below_root(term, state, true)
}

fn ensure_not_effectful_let_value(term: &Term, state: &LoweringState) -> Result<()> {
    if let Some(operation) = root_io_statement_function_call_name(term, state) {
        return Err(Error::Unsupported(format!(
            "`{operation}` is effectful and must be bound with `<-`"
        )));
    }
    Ok(())
}

fn root_io_statement_function_call_name<'a>(
    term: &'a Term,
    state: &'a LoweringState,
) -> Option<&'a str> {
    match term {
        Term::Group(inner) => root_io_statement_function_call_name(inner, state),
        Term::Application { callee, .. } => {
            let name = single_segment_path_name(callee.as_ref())?;
            let function = state.statement_functions.get(name)?;
            (function.effect.as_deref() == Some("IO")
                && statement_function_uses_io_builtin(function, state, &mut HashSet::new()))
            .then_some(name)
        }
        _ => None,
    }
}

fn statement_function_uses_io_builtin(
    function: &StatementFunction,
    state: &LoweringState,
    visited: &mut HashSet<String>,
) -> bool {
    block_uses_io_builtin(&function.body, state, visited)
}

fn block_uses_io_builtin(
    block: &neco_rs_parser::Block,
    state: &LoweringState,
    visited: &mut HashSet<String>,
) -> bool {
    block.statements.iter().any(|statement| match statement {
        Statement::Let(let_stmt) => term_uses_io_builtin(let_stmt.value.as_ref(), state, visited),
        Statement::LetRef(letref_stmt) => {
            term_uses_io_builtin(letref_stmt.source.as_ref(), state, visited)
        }
        Statement::Expression(term) => term_uses_io_builtin(term.as_ref(), state, visited),
        Statement::If(if_stmt) => if_statement_uses_io_builtin(if_stmt, state, visited),
        Statement::Loop(loop_stmt) => block_uses_io_builtin(&loop_stmt.body, state, visited),
        Statement::Break | Statement::Continue | Statement::Item(_) => false,
    }) || block
        .tail
        .as_deref()
        .is_some_and(|tail| term_uses_io_builtin(tail, state, visited))
}

fn if_statement_uses_io_builtin(
    if_stmt: &neco_rs_parser::IfStatement,
    state: &LoweringState,
    visited: &mut HashSet<String>,
) -> bool {
    term_uses_io_builtin(if_stmt.condition.as_ref(), state, visited)
        || block_uses_io_builtin(&if_stmt.then_block, state, visited)
        || match &if_stmt.else_branch {
            Some(ElseBranch::Block(block)) => block_uses_io_builtin(block, state, visited),
            Some(ElseBranch::If(if_stmt)) => if_statement_uses_io_builtin(if_stmt, state, visited),
            None => false,
        }
}

fn term_uses_io_builtin(term: &Term, state: &LoweringState, visited: &mut HashSet<String>) -> bool {
    match term {
        Term::Group(inner) => term_uses_io_builtin(inner, state, visited),
        Term::Path(path) => simple_path_segments(path)
            .is_some_and(|segments| matches!(segments.as_slice(), ["IO", _])),
        Term::Application { callee, arguments } => {
            if let Term::Path(path) = callee.as_ref()
                && let Some(segments) = simple_path_segments(path)
            {
                if matches!(segments.as_slice(), ["IO", _]) {
                    return true;
                }
                if let [name] = segments.as_slice()
                    && visited.insert((*name).to_string())
                    && let Some(function) = state.statement_functions.get(*name)
                    && function.effect.as_deref() == Some("IO")
                    && statement_function_uses_io_builtin(function, state, visited)
                {
                    return true;
                }
            }
            arguments
                .iter()
                .any(|argument| term_uses_io_builtin(argument, state, visited))
        }
        Term::MethodCall { receiver, .. } => term_uses_io_builtin(receiver, state, visited),
        Term::FieldAccess { receiver, .. }
        | Term::Reference {
            referent: receiver, ..
        } => term_uses_io_builtin(receiver, state, visited),
        Term::StructLiteral { fields, .. } => fields
            .iter()
            .any(|field| term_uses_io_builtin(&field.value, state, visited)),
        Term::Arrow(arrow) => match &arrow.parameter {
            neco_rs_parser::ArrowParameter::Binder(binder) => {
                term_uses_io_builtin(&binder.ty, state, visited)
                    || term_uses_io_builtin(&arrow.result, state, visited)
            }
            neco_rs_parser::ArrowParameter::Domain(domain) => {
                term_uses_io_builtin(domain, state, visited)
                    || term_uses_io_builtin(&arrow.result, state, visited)
            }
        },
        Term::Forall(forall) => {
            term_uses_io_builtin(&forall.binder.ty, state, visited)
                || term_uses_io_builtin(&forall.body, state, visited)
        }
        Term::Block(block) => block_uses_io_builtin(block, state, visited),
        Term::Match(match_expr) => {
            term_uses_io_builtin(match_expr.scrutinee.as_ref(), state, visited)
                || match_expr
                    .arms
                    .iter()
                    .any(|arm| term_uses_io_builtin(arm.result.as_ref(), state, visited))
        }
        Term::Unit
        | Term::StringLiteral(_)
        | Term::CharLiteral(_)
        | Term::IntegerLiteral(_)
        | Term::TypedBinder(_) => false,
    }
}

fn ensure_no_io_effects(term: &Term, state: &LoweringState) -> Result<()> {
    ensure_no_io_effects_below_root(term, state, false)
}

fn ensure_no_io_effects_below_root(
    term: &Term,
    state: &LoweringState,
    allow_root_effect: bool,
) -> Result<()> {
    if !allow_root_effect && let Some(operation) = io_effect_operation_name(term, state) {
        return Err(Error::Unsupported(format!(
            "`{operation}` is effectful and must be used as a top-level statement"
        )));
    }

    match term {
        Term::Group(inner) => ensure_no_io_effects_below_root(inner, state, allow_root_effect),
        Term::Application { arguments, .. } => {
            for argument in arguments {
                ensure_no_io_effects(argument, state)?;
            }
            Ok(())
        }
        Term::MethodCall { receiver, .. } | Term::FieldAccess { receiver, .. } => {
            ensure_no_io_effects(receiver, state)
        }
        Term::StructLiteral { fields, .. } => {
            for field in fields {
                ensure_no_io_effects(&field.value, state)?;
            }
            Ok(())
        }
        Term::Reference { referent, .. } => ensure_no_io_effects(referent, state),
        Term::Arrow(arrow) => {
            match &arrow.parameter {
                neco_rs_parser::ArrowParameter::Binder(binder) => {
                    ensure_no_io_effects(&binder.ty, state)?;
                }
                neco_rs_parser::ArrowParameter::Domain(domain) => {
                    ensure_no_io_effects(domain, state)?;
                }
            }
            ensure_no_io_effects(&arrow.result, state)
        }
        Term::Forall(forall) => {
            ensure_no_io_effects(&forall.binder.ty, state)?;
            ensure_no_io_effects(&forall.body, state)
        }
        Term::Block(block) => {
            for statement in &block.statements {
                match statement {
                    Statement::Let(let_stmt) => {
                        ensure_no_nested_io_effects(let_stmt.value.as_ref(), state)?;
                    }
                    Statement::LetRef(letref_stmt) => {
                        ensure_no_io_effects(letref_stmt.source.as_ref(), state)?;
                    }
                    Statement::Expression(term) => {
                        ensure_no_nested_io_effects(term.as_ref(), state)?;
                    }
                    Statement::If(if_stmt) => {
                        ensure_no_io_effects(if_stmt.condition.as_ref(), state)?;
                    }
                    Statement::Loop(loop_stmt) => {
                        ensure_no_io_effects_below_root(
                            &Term::Block(loop_stmt.body.clone()),
                            state,
                            false,
                        )?;
                    }
                    Statement::Break | Statement::Continue | Statement::Item(_) => {}
                }
            }
            if let Some(tail) = block.tail.as_deref() {
                ensure_no_nested_io_effects(tail, state)?;
            }
            Ok(())
        }
        Term::Match(match_expr) => {
            ensure_no_io_effects(match_expr.scrutinee.as_ref(), state)?;
            for arm in &match_expr.arms {
                ensure_no_nested_io_effects(arm.result.as_ref(), state)?;
            }
            Ok(())
        }
        Term::Unit
        | Term::StringLiteral(_)
        | Term::CharLiteral(_)
        | Term::IntegerLiteral(_)
        | Term::Path(_)
        | Term::TypedBinder(_) => Ok(()),
    }
}

fn io_effect_operation_name(term: &Term, state: &LoweringState) -> Option<String> {
    match term {
        Term::Group(inner) => io_effect_operation_name(inner, state),
        Term::Path(path) => {
            let segments = simple_path_segments(path)?;
            match segments.as_slice() {
                ["IO", name] => Some(format!("IO::{name}")),
                [name] => state.statement_functions.get(*name).and_then(|function| {
                    (function.effect.as_deref() == Some("IO")).then(|| (*name).to_string())
                }),
                _ => None,
            }
        }
        Term::Application { callee, .. } => {
            let Term::Path(path) = callee.as_ref() else {
                return None;
            };
            let segments = simple_path_segments(path)?;
            match segments.as_slice() {
                ["IO", name] => Some(format!("IO::{name}")),
                ["ref_get" | "ref_set" | "array_get" | "array_set" | "array_len"] => {
                    Some(segments[0].to_string())
                }
                [name] => state.statement_functions.get(*name).and_then(|function| {
                    (function.effect.as_deref() == Some("IO")).then(|| (*name).to_string())
                }),
                _ => None,
            }
        }
        _ => None,
    }
}

fn simple_path_segments(path: &neco_rs_parser::PathExpression) -> Option<Vec<&str>> {
    if path.token_keyword_package.is_some() {
        return None;
    }
    Some(
        path.segments
            .iter()
            .map(|segment| segment.lexeme.as_str())
            .collect(),
    )
}

fn single_segment_path_name(term: &Term) -> Option<&str> {
    let Term::Path(path) = term else {
        return None;
    };
    if path.token_keyword_package.is_some() || path.segments.len() != 1 {
        return None;
    }
    Some(path.segments[0].lexeme.as_str())
}

fn lower_effectful_block(
    block: &neco_rs_parser::Block,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    let mut scoped_state = state.child_scope();
    for statement in &block.statements {
        let terminated = lower_statement(statement, &mut scoped_state, program)?;
        if terminated {
            state.next_array_slot = state.next_array_slot.max(scoped_state.next_array_slot);
            state.next_i32_slot = state.next_i32_slot.max(scoped_state.next_i32_slot);
            state.next_i64_slot = state.next_i64_slot.max(scoped_state.next_i64_slot);
            state.next_f32_slot = state.next_f32_slot.max(scoped_state.next_f32_slot);
            return Ok(true);
        }
    }
    if let Some(tail) = block.tail.as_deref() {
        lower_expression_statement(tail, &mut scoped_state, program)?;
    }
    state.next_array_slot = state.next_array_slot.max(scoped_state.next_array_slot);
    state.next_i32_slot = state.next_i32_slot.max(scoped_state.next_i32_slot);
    state.next_i64_slot = state.next_i64_slot.max(scoped_state.next_i64_slot);
    state.next_f32_slot = state.next_f32_slot.max(scoped_state.next_f32_slot);
    Ok(false)
}

fn lower_match_expression_statement(
    match_expr: &MatchExpression,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    let scrutinee = lower_pure_value(match_expr.scrutinee.as_ref(), state, program)?;
    for arm in &match_expr.arms {
        if let Some(bindings) =
            pattern_match_bindings(&arm.pattern, &scrutinee, &state.constructors)?
        {
            let mut scoped_state = state.child_scope();
            scoped_state.environment.extend(bindings);
            lower_expression_statement(arm.result.as_ref(), &mut scoped_state, program)?;
            state.next_array_slot = state.next_array_slot.max(scoped_state.next_array_slot);
            state.next_i32_slot = state.next_i32_slot.max(scoped_state.next_i32_slot);
            state.next_i64_slot = state.next_i64_slot.max(scoped_state.next_i64_slot);
            state.next_f32_slot = state.next_f32_slot.max(scoped_state.next_f32_slot);
            return Ok(());
        }
    }

    Err(Error::Unsupported(
        "`#match` did not match any constructor arm".to_string(),
    ))
}
