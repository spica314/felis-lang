mod declarations;
mod expr;
mod package;
mod ptx;
mod pure;
mod scalar;
mod symbol;
mod typecheck;

use neco_rs_parser::{
    BindingPattern, ElseBranch, Item, LetOperator, MatchExpression, ParsedPackage, Statement, Term,
};
use std::collections::{HashMap, HashSet};

use crate::effect::{Value, bind_pattern, lower_effect, resolve_value};
use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, ConditionExpr, ExitCodeExpr, I32Expr,
    LoweredProgram, Operation,
};
use crate::{Error, Result};

use declarations::{
    ConstructorSignature, PureFunction, StatementFunction, StructSignature,
    collect_builtin_aliases, collect_constructors, collect_pure_functions,
    collect_statement_functions, collect_structs,
};
use expr::lower_condition_expr;
use package::{collect_callable_packages, initialize_zero_arg_use_bindings};
use ptx::{collect_ptx_functions, initialize_compile_ptx_bindings};
use pure::{
    lower_function_call_statement, lower_function_call_value, lower_pure_value,
    pattern_match_bindings, substitute_type_bindings,
};
use symbol::SymbolTable;
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
    pub(crate) next_u8_slot: usize,
    pub(crate) next_bool_slot: usize,
    pub(crate) io_effect_allowed: bool,
    functions: HashMap<String, PureFunction>,
    statement_functions: HashMap<String, StatementFunction>,
    constructors: HashMap<String, ConstructorSignature>,
    structs: HashMap<String, StructSignature>,
    builtin_aliases: HashMap<String, String>,
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
            next_u8_slot: 0,
            next_bool_slot: 0,
            io_effect_allowed: false,
            functions: HashMap::new(),
            statement_functions: HashMap::new(),
            constructors: HashMap::new(),
            structs: HashMap::new(),
            builtin_aliases: HashMap::new(),
            loop_depth: 0,
        }
    }

    fn child_scope(&self) -> Self {
        self.clone()
    }

    fn merge_allocations_from(&mut self, scoped_state: &Self) {
        self.next_array_slot = self.next_array_slot.max(scoped_state.next_array_slot);
        self.next_i32_slot = self.next_i32_slot.max(scoped_state.next_i32_slot);
        self.next_i64_slot = self.next_i64_slot.max(scoped_state.next_i64_slot);
        self.next_f32_slot = self.next_f32_slot.max(scoped_state.next_f32_slot);
        self.next_u8_slot = self.next_u8_slot.max(scoped_state.next_u8_slot);
        self.next_bool_slot = self.next_bool_slot.max(scoped_state.next_bool_slot);
    }

    fn propagate_aggregate_reference_updates_from(&mut self, scoped_state: &Self) {
        let names: Vec<String> = self.environment.keys().cloned().collect();
        for name in names {
            let Some(scoped_value) = scoped_state.environment.get(&name).cloned() else {
                continue;
            };
            if aggregate_reference_update_can_propagate(self.environment.get(&name), &scoped_value)
            {
                self.environment.insert(name, scoped_value);
            }
        }
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

    pub(crate) fn allocate_u8_slot(&mut self) -> usize {
        let slot = self.next_u8_slot;
        self.next_u8_slot += 1;
        slot
    }

    pub(crate) fn allocate_bool_slot(&mut self) -> usize {
        let slot = self.next_bool_slot;
        self.next_bool_slot += 1;
        slot
    }

    pub(crate) fn resolve_builtin_alias<'a>(&'a self, name: &'a str) -> &'a str {
        self.builtin_aliases
            .get(name)
            .map(|builtin| builtin.as_str())
            .unwrap_or(name)
    }
}

fn aggregate_reference_update_can_propagate(
    parent_value: Option<&Value>,
    scoped_value: &Value,
) -> bool {
    match (parent_value, scoped_value) {
        (
            Some(Value::Reference {
                value: parent_referent,
                ..
            }),
            Value::Reference {
                value: scoped_referent,
                ..
            },
        ) => {
            matches!(
                parent_referent.as_ref(),
                Value::Constructor(_) | Value::Struct(_)
            ) && matches!(
                scoped_referent.as_ref(),
                Value::Constructor(_) | Value::Struct(_)
            )
        }
        (
            Some(Value::Constructor(_) | Value::Struct(_)),
            Value::Constructor(_) | Value::Struct(_),
        ) => true,
        _ => false,
    }
}

fn propagate_converged_aggregate_reference_updates(
    state: &mut LoweringState,
    then_state: &LoweringState,
    else_state: &LoweringState,
) {
    let names: Vec<String> = state.environment.keys().cloned().collect();
    for name in names {
        let Some(then_value) = then_state.environment.get(&name) else {
            continue;
        };
        let Some(else_value) = else_state.environment.get(&name) else {
            continue;
        };
        if then_value == else_value
            && aggregate_reference_update_can_propagate(state.environment.get(&name), then_value)
        {
            state.environment.insert(name, then_value.clone());
        }
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
        compiled_ptx: Vec::new(),
        arrays: Vec::new(),
        heap_slots: 0,
        i32_slots: 0,
        i64_slots: 0,
        f32_slots: 0,
        u8_slots: 0,
        bool_slots: 0,
        requires_argv: false,
    };
    let mut state = LoweringState::new();
    let callable_packages = collect_callable_packages(package)?;
    let symbols = SymbolTable::build(&callable_packages);
    state.functions = collect_pure_functions(&symbols)?;
    state.statement_functions = collect_statement_functions(&symbols)?;
    state.constructors = collect_constructors(&symbols)?;
    state.structs = collect_structs(&symbols)?;
    state.builtin_aliases = collect_builtin_aliases(&symbols)?;
    let ptx_functions = collect_ptx_functions(&symbols)?;
    initialize_compile_ptx_bindings(package, &symbols, &ptx_functions, &mut state, &mut program)?;
    initialize_zero_arg_use_bindings(package, &symbols, &mut state, &mut program)?;
    state.io_effect_allowed = function_has_io_effect(main_fn);
    let mut terminated = false;

    for statement in &main_fn.body.statements {
        if terminated {
            return Err(Error::Unsupported(
                "statements after `IO::sys_exit` are not supported".to_string(),
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
    program.u8_slots = state.next_u8_slot;
    program.bool_slots = state.next_bool_slot;

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
                if root_ref_get_call_name(let_stmt.value.as_ref()).is_some() {
                    lower_let_equals_statement(
                        &let_stmt.binder,
                        let_stmt.ty.as_ref(),
                        let_stmt.value.as_ref(),
                        state,
                        program,
                    )?;
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
        Statement::Expression(term) => lower_expression_statement(term.as_ref(), state, program),
        Statement::If(if_stmt) => lower_if_statement(if_stmt, state, program),
        Statement::Loop(loop_stmt) => {
            reject_control_flow_block_tail(loop_stmt.body.tail.as_deref(), "#loop")?;
            let mut loop_state = state.child_scope();
            loop_state.loop_depth += 1;
            let mut loop_program = LoweredProgram {
                operations: Vec::new(),
                data: std::mem::take(&mut program.data),
                compiled_ptx: program.compiled_ptx.clone(),
                arrays: std::mem::take(&mut program.arrays),
                heap_slots: program.heap_slots,
                i32_slots: program.i32_slots,
                i64_slots: program.i64_slots,
                f32_slots: program.f32_slots,
                u8_slots: program.u8_slots,
                bool_slots: program.bool_slots,
                requires_argv: program.requires_argv,
            };
            let mut terminated = false;
            for statement in &loop_stmt.body.statements {
                if terminated {
                    return Err(statements_after_termination_error(&loop_program.operations));
                }
                terminated = lower_statement(statement, &mut loop_state, &mut loop_program)?;
            }
            state.propagate_aggregate_reference_updates_from(&loop_state);
            program.data = loop_program.data;
            program.arrays = loop_program.arrays;
            program.heap_slots = program.heap_slots.max(loop_program.heap_slots);
            program.requires_argv = loop_program.requires_argv;
            state.merge_allocations_from(&loop_state);
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
    reject_control_flow_block_tail(if_stmt.then_block.tail.as_deref(), "#if")?;
    let condition = lower_condition_expr(if_stmt.condition.as_ref(), state, program)?;
    let mut then_state = state.child_scope();
    let mut then_operations = Vec::new();
    let mut then_program = LoweredProgram {
        operations: Vec::new(),
        data: std::mem::take(&mut program.data),
        compiled_ptx: program.compiled_ptx.clone(),
        arrays: std::mem::take(&mut program.arrays),
        heap_slots: program.heap_slots,
        i32_slots: program.i32_slots,
        i64_slots: program.i64_slots,
        f32_slots: program.f32_slots,
        u8_slots: program.u8_slots,
        bool_slots: program.bool_slots,
        requires_argv: program.requires_argv,
    };
    let mut then_terminated = false;
    for statement in &if_stmt.then_block.statements {
        if then_terminated {
            return Err(statements_after_termination_error(&then_program.operations));
        }
        then_terminated = lower_statement(statement, &mut then_state, &mut then_program)?;
    }
    then_operations.append(&mut then_program.operations);
    let mut else_operations = Vec::new();
    let mut next_array_slot = then_state.next_array_slot;
    let mut next_i32_slot = then_state.next_i32_slot;
    let mut next_i64_slot = then_state.next_i64_slot;
    let mut next_f32_slot = then_state.next_f32_slot;
    let mut next_u8_slot = then_state.next_u8_slot;
    let mut next_bool_slot = then_state.next_bool_slot;

    if let Some(else_branch) = &if_stmt.else_branch {
        let mut else_state = state.child_scope();
        let mut else_program = LoweredProgram {
            operations: Vec::new(),
            data: then_program.data,
            compiled_ptx: then_program.compiled_ptx.clone(),
            arrays: then_program.arrays,
            heap_slots: then_program.heap_slots,
            i32_slots: then_program.i32_slots,
            i64_slots: then_program.i64_slots,
            f32_slots: then_program.f32_slots,
            u8_slots: then_program.u8_slots,
            bool_slots: then_program.bool_slots,
            requires_argv: then_program.requires_argv,
        };
        match else_branch {
            ElseBranch::Block(else_block) => {
                reject_control_flow_block_tail(else_block.tail.as_deref(), "#else")?;
                let mut else_terminated = false;
                for statement in &else_block.statements {
                    if else_terminated {
                        return Err(statements_after_termination_error(&else_program.operations));
                    }
                    else_terminated =
                        lower_statement(statement, &mut else_state, &mut else_program)?;
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
        next_u8_slot = next_u8_slot.max(else_state.next_u8_slot);
        next_bool_slot = next_bool_slot.max(else_state.next_bool_slot);
        match &condition {
            ConditionExpr::Literal(true) => {
                state.propagate_aggregate_reference_updates_from(&then_state);
            }
            ConditionExpr::Literal(false) => {
                state.propagate_aggregate_reference_updates_from(&else_state);
            }
            _ => {
                propagate_converged_aggregate_reference_updates(state, &then_state, &else_state);
            }
        }
    } else {
        program.data = then_program.data;
        program.arrays = then_program.arrays;
        program.heap_slots = program.heap_slots.max(then_program.heap_slots);
        program.requires_argv = then_program.requires_argv;
        if matches!(condition, ConditionExpr::Literal(true)) {
            state.propagate_aggregate_reference_updates_from(&then_state);
        }
    }
    state.next_array_slot = next_array_slot;
    state.next_i32_slot = next_i32_slot;
    state.next_i64_slot = next_i64_slot;
    state.next_f32_slot = next_f32_slot;
    state.next_u8_slot = next_u8_slot;
    state.next_bool_slot = next_bool_slot;
    program.operations.push(Operation::If {
        condition,
        then_operations,
        else_operations,
    });
    Ok(false)
}

fn reject_control_flow_block_tail(tail: Option<&Term>, block_kind: &str) -> Result<()> {
    if tail.is_some() {
        return Err(Error::Unsupported(format!(
            "tail expressions in `{block_kind}` statement blocks are not supported"
        )));
    }
    Ok(())
}

fn statements_after_termination_error(operations: &[Operation]) -> Error {
    match operations.last() {
        Some(Operation::Exit(_)) => {
            Error::Unsupported("statements after `IO::sys_exit` are not supported".to_string())
        }
        _ => Error::Unsupported("statements after loop control are not supported".to_string()),
    }
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
        | Value::U8Reference { .. }
        | Value::BoolReference { .. }
        | Value::Reference { .. }
        | Value::StaticSlice { .. }
        | Value::RuntimeArg(_) => value,
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
        Value::U8(expr) => {
            let slot = state.allocate_u8_slot();
            program
                .operations
                .push(Operation::StoreU8 { slot, value: expr });
            Value::U8Reference { slot, exclusive }
        }
        Value::Bool(condition) => {
            let slot = state.allocate_bool_slot();
            program
                .operations
                .push(Operation::StoreBool { slot, condition });
            Value::BoolReference { slot, exclusive }
        }
        Value::Constructor(mut constructor) if constructor.heap_slot.is_some() => {
            let source_heap_slot = constructor.heap_slot.expect("checked above");
            let reference_heap_slot = allocate_heap_slot(program);
            program.operations.push(Operation::HeapSlotReplace {
                dest_heap_slot: reference_heap_slot,
                source_heap_slot,
            });
            constructor.heap_slot = Some(reference_heap_slot);
            constructor.runtime_tag = true;
            Value::Reference {
                value: Box::new(Value::Constructor(constructor)),
                exclusive,
            }
        }
        other => Value::Reference {
            value: Box::new(other),
            exclusive,
        },
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
) -> Result<bool> {
    ensure_no_nested_io_effects(term, state)?;
    if let Term::Block(block) = term {
        return lower_effectful_block(block, state, program);
    }
    if matches!(term, Term::Unit) {
        return Ok(false);
    }
    if let Term::Match(match_expr) = term {
        return lower_match_expression_statement(match_expr, state, program);
    }
    if lower_reference_set_builtin_statement(term, state, program)? {
        return Ok(false);
    }
    if lower_array_set_builtin_statement(term, state, program)? {
        return Ok(false);
    }
    if lower_function_call_statement(term, state, program)?.is_some() {
        return Ok(false);
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

    match exclusive_array_set_receiver(&receiver, state)? {
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
                "`array_set` requires an exclusive array reference, got {other:?}"
            )));
        }
    }
    Ok(())
}

fn exclusive_array_set_receiver(term: &Term, state: &LoweringState) -> Result<Value> {
    match crate::effect::resolve_value(term, &state.environment)? {
        Value::Reference {
            value,
            exclusive: true,
        } => Ok(*value),
        Value::Reference {
            exclusive: false, ..
        } => Err(Error::Unsupported(
            "`array_set` requires an exclusive array reference".to_string(),
        )),
        other => Err(Error::Unsupported(format!(
            "`array_set` requires an exclusive array reference, got {other:?}"
        ))),
    }
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
        let ty = substitute_type_bindings(_ty, &type_bindings_from_environment(&state.environment));
        validate_value_against_type(referent.as_ref(), &ty, program)?;
        let lowered_value = match referent.as_ref() {
            Value::I32(_) => Value::I32(lower_i32_expr(value, state)?),
            Value::I64(_) => Value::I64(lower_i64_expr(value, state)?),
            Value::F32(_) => Value::F32(lower_f32_expr(value, state)?),
            Value::U8(_) => Value::U8(lower_u8_expr(value, state)?),
            Value::Bool(_) => Value::Bool(lower_bool_expr(value, state)?),
            Value::Constructor(_) | Value::Struct(_) => lower_pure_value(value, state, program)?,
            other => {
                return Err(Error::Unsupported(format!(
                    "`ref_set` does not support this reference target: {other:?}"
                )));
            }
        };
        validate_value_against_type(&lowered_value, &ty, program)?;
        if let (Value::Constructor(current_constructor), Value::Constructor(mut new_constructor)) =
            (referent.as_ref(), lowered_value.clone())
            && let (Some(dest_heap_slot), Some(source_heap_slot)) =
                (current_constructor.heap_slot, new_constructor.heap_slot)
        {
            program.operations.push(Operation::HeapSlotReplace {
                dest_heap_slot,
                source_heap_slot,
            });
            new_constructor.heap_slot = Some(dest_heap_slot);
            new_constructor.runtime_tag = true;
            state.environment.insert(
                name.to_string(),
                Value::Reference {
                    value: Box::new(Value::Constructor(new_constructor)),
                    exclusive: true,
                },
            );
            return Ok(true);
        }
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
        Value::U8Reference {
            slot,
            exclusive: true,
        } => {
            program.operations.push(Operation::StoreU8 {
                slot,
                value: lower_u8_expr(value, state)?,
            });
        }
        Value::BoolReference {
            slot,
            exclusive: true,
        } => {
            program.operations.push(Operation::StoreBool {
                slot,
                condition: lower_bool_expr(value, state)?,
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
        }
        | Value::U8Reference {
            exclusive: false, ..
        }
        | Value::BoolReference {
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
    if root_ref_get_call_name(term).is_some() {
        return Err(Error::Unsupported(
            "`ref_get` is effectful and must be bound with `<-`".to_string(),
        ));
    }
    if let Some(operation) = root_io_statement_function_call_name(term, state) {
        return Err(Error::Unsupported(format!(
            "`{operation}` is effectful and must be bound with `<-`"
        )));
    }
    Ok(())
}

fn root_ref_get_call_name(term: &Term) -> Option<&'static str> {
    match term {
        Term::Group(inner) => root_ref_get_call_name(inner),
        Term::Application { callee, .. } => {
            let name = single_segment_path_name(callee.as_ref())?;
            (name == "ref_get").then_some("ref_get")
        }
        _ => None,
    }
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
                if matches!(
                    segments.as_slice(),
                    ["ref_get" | "ref_set" | "array_get" | "array_set" | "array_len"]
                ) {
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
    let mut terminated = false;
    for statement in &block.statements {
        if terminated {
            return Err(statements_after_termination_error(&program.operations));
        }
        terminated = lower_statement(statement, &mut scoped_state, program)?;
        if terminated {
            state.propagate_aggregate_reference_updates_from(&scoped_state);
            state.merge_allocations_from(&scoped_state);
            return Ok(true);
        }
    }
    if let Some(tail) = block.tail.as_deref() {
        terminated = lower_expression_statement(tail, &mut scoped_state, program)?;
    }
    state.propagate_aggregate_reference_updates_from(&scoped_state);
    state.merge_allocations_from(&scoped_state);
    Ok(terminated)
}

fn lower_match_expression_statement(
    match_expr: &MatchExpression,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<bool> {
    let scrutinee = lower_pure_value(match_expr.scrutinee.as_ref(), state, program)?;
    for arm in &match_expr.arms {
        if let Some(bindings) =
            pattern_match_bindings(&arm.pattern, &scrutinee, &state.constructors)?
        {
            let mut scoped_state = state.child_scope();
            scoped_state.environment.extend(bindings);
            let terminated =
                lower_expression_statement(arm.result.as_ref(), &mut scoped_state, program)?;
            state.propagate_aggregate_reference_updates_from(&scoped_state);
            state.merge_allocations_from(&scoped_state);
            return Ok(terminated);
        }
    }

    Err(Error::Unsupported(
        "`#match` did not match any constructor arm".to_string(),
    ))
}
