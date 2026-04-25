mod declarations;
mod expr;
mod pure;
mod typecheck;

use std::collections::HashMap;
use std::path::Path;

use neco_rs_parser::{
    BindingPattern, ElseBranch, Item, LetOperator, MatchExpression, ParsedPackage, ParsedRoot,
    ParsedWorkspace, Statement, Term, parse_root,
};

use crate::effect::{Value, bind_pattern, lower_effect};
use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, ExitCodeExpr, I32Expr, LoweredProgram, Operation,
};
use crate::{Error, Result};

use declarations::{
    ConstructorSignature, Procedure, PureFunction, collect_constructors, collect_procedures,
    collect_pure_functions, pure_function_from_decl,
};
use expr::lower_condition_expr;
use pure::{
    lower_procedure_call_statement, lower_pure_block_value, lower_pure_value,
    pattern_match_bindings,
};
pub(crate) use typecheck::validate_value_against_type;

pub(crate) use expr::{lower_i32_expr, lower_u8_expr, normalize_numeric_literal_arguments};

#[derive(Clone)]
pub(crate) struct LoweringState {
    pub(crate) environment: HashMap<String, Value>,
    pub(crate) next_array_slot: usize,
    pub(crate) next_i32_slot: usize,
    functions: HashMap<String, PureFunction>,
    procedures: HashMap<String, Procedure>,
    constructors: HashMap<String, ConstructorSignature>,
    loop_depth: usize,
}

impl LoweringState {
    fn new() -> Self {
        Self {
            environment: HashMap::new(),
            next_array_slot: 0,
            next_i32_slot: 0,
            functions: HashMap::new(),
            procedures: HashMap::new(),
            constructors: HashMap::new(),
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
}

pub(super) fn allocate_heap_slot(program: &mut LoweredProgram) -> usize {
    let slot = program.heap_slots;
    program.heap_slots += 1;
    slot
}

pub(crate) fn lower_package_to_program(package: &ParsedPackage) -> Result<LoweredProgram> {
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
        heap_slots: 0,
        i32_slots: 0,
        requires_argv: false,
    };
    let mut state = LoweringState::new();
    let procedure_packages = collect_procedure_packages(package)?;
    state.functions = collect_pure_functions(&procedure_packages)?;
    state.procedures = collect_procedures(&procedure_packages)?;
    state.constructors = collect_constructors(&procedure_packages)?;
    initialize_zero_arg_use_bindings(package, &procedure_packages, &mut state, &mut program)?;
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
            .name
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
    if !last_segment.suffixes.is_empty() {
        return Ok(None);
    }

    let target_package = if path.starts_with_package || path.segments.len() == 1 {
        package
    } else {
        let package_name = &path.segments[0].name;
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
        if function.kind != neco_rs_parser::FunctionKind::Fn || function.effect.is_some() {
            continue;
        }
        if function.name.name != last_segment.name {
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

fn collect_procedure_packages(package: &ParsedPackage) -> Result<Vec<ParsedPackage>> {
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
                lower_let_equals_statement(
                    &let_stmt.binder,
                    let_stmt.ty.as_ref(),
                    let_stmt.value.as_ref(),
                    state,
                    program,
                )?;
                Ok(false)
            }
            LetOperator::LeftArrow => lower_effect(
                &let_stmt.binder,
                let_stmt.ty.as_ref(),
                let_stmt.value.as_ref(),
                state,
                program,
            ),
        },
        Statement::LetRef(letref_stmt) => match letref_stmt.operator {
            LetOperator::Equals => {
                lower_letref_equals_statement(
                    &letref_stmt.reference,
                    letref_stmt.exclusive,
                    letref_stmt.ty.as_ref(),
                    letref_stmt.value.as_ref(),
                    state,
                    program,
                )?;
                Ok(false)
            }
            LetOperator::LeftArrow => lower_letref_effect_statement(
                &letref_stmt.reference,
                letref_stmt.exclusive,
                letref_stmt.ty.as_ref(),
                letref_stmt.value.as_ref(),
                state,
                program,
            ),
        },
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
                requires_argv: program.requires_argv,
            };
            for statement in &loop_stmt.body.statements {
                let terminated = lower_statement(statement, &mut loop_state, &mut loop_program)?;
                if terminated {
                    break;
                }
            }
            program.data = loop_program.data;
            program.arrays = loop_program.arrays;
            program.heap_slots = program.heap_slots.max(loop_program.heap_slots);
            program.requires_argv = loop_program.requires_argv;
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

fn lower_if_statement(
    if_stmt: &neco_rs_parser::IfStatement,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    let condition = lower_condition_expr(if_stmt.condition.as_ref(), state)?;
    let mut then_state = state.child_scope();
    let mut then_operations = Vec::new();
    let mut then_program = LoweredProgram {
        operations: Vec::new(),
        data: std::mem::take(&mut program.data),
        arrays: std::mem::take(&mut program.arrays),
        heap_slots: program.heap_slots,
        i32_slots: program.i32_slots,
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

    if let Some(else_branch) = &if_stmt.else_branch {
        let mut else_state = state.child_scope();
        let mut else_program = LoweredProgram {
            operations: Vec::new(),
            data: then_program.data,
            arrays: then_program.arrays,
            heap_slots: then_program.heap_slots,
            i32_slots: then_program.i32_slots,
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
    } else {
        program.data = then_program.data;
        program.arrays = then_program.arrays;
        program.heap_slots = program.heap_slots.max(then_program.heap_slots);
        program.requires_argv = then_program.requires_argv;
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

fn lower_let_equals_statement(
    binder: &BindingPattern,
    ty: &Term,
    value_term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    let value = lower_pure_value(value_term, state, program)?;
    validate_value_against_type(&value, ty, program)?;
    bind_pattern(binder, value, &mut state.environment);
    Ok(())
}

fn lower_letref_equals_statement(
    reference: &str,
    exclusive: bool,
    ty: &Term,
    value_term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    validate_letref_annotation(exclusive, ty)?;
    let value = lower_pure_value(value_term, state, program)?;
    let reference_value = match value {
        Value::I32(expr) => {
            let slot = state.allocate_i32_slot();
            program
                .operations
                .push(Operation::StoreI32 { slot, value: expr });
            Value::I32Reference(slot)
        }
        other => other,
    };
    validate_value_against_type(&reference_value, ty, program)?;
    state
        .environment
        .insert(reference.to_string(), reference_value);
    Ok(())
}

fn lower_letref_effect_statement(
    reference: &str,
    exclusive: bool,
    ty: &Term,
    value_term: &Term,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    validate_letref_annotation(exclusive, ty)?;
    let binder = BindingPattern::Name(reference.to_string());
    lower_effect(&binder, ty, value_term, state, program)
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
    if lower_procedure_call_statement(term, state, program)?.is_some() {
        return Ok(());
    }

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

    match crate::effect::resolve_value(receiver.as_ref(), &state.environment)? {
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
            ..
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
            ..
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
            return Ok(true);
        }
    }
    if let Some(tail) = block.tail.as_deref() {
        lower_expression_statement(tail, &mut scoped_state, program)?;
    }
    state.next_array_slot = state.next_array_slot.max(scoped_state.next_array_slot);
    state.next_i32_slot = state.next_i32_slot.max(scoped_state.next_i32_slot);
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
            return Ok(());
        }
    }

    Err(Error::Unsupported(
        "`#match` did not match any constructor arm".to_string(),
    ))
}
