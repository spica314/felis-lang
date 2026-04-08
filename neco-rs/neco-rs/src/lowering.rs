use std::collections::HashMap;

use neco_rs_parser::{
    ArrowParameter, BindingPattern, Block, ConstructorDeclaration, DeclaredName,
    FunctionDeclaration, FunctionKind, Item, LetOperator, MatchExpression, ParsedPackage,
    PathExpression, Pattern, Statement, Term,
};

use crate::effect::{Value, bind_pattern, lower_effect, resolve_value};
use crate::ir::{
    ArrayAllocation, ArrayElementType, ComparisonKind, ConditionExpr, ConstructorValue,
    ExitCodeExpr, I32Expr, LoweredProgram, Operation, U8Expr, intern_data,
};
use crate::{Error, Result};

pub(crate) struct LoweringState {
    pub(crate) environment: HashMap<String, Value>,
    pub(crate) next_array_slot: usize,
    pub(crate) next_i32_slot: usize,
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
struct ConstructorSignature {
    type_name: String,
    constructor_name: String,
    arity: usize,
    is_rc: bool,
    tag: i32,
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

    pub(crate) fn allocate_array(
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

    pub(crate) fn allocate_i32_slot(&mut self) -> usize {
        let slot = self.next_i32_slot;
        self.next_i32_slot += 1;
        slot
    }
}

fn allocate_heap_slot(program: &mut LoweredProgram) -> usize {
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
            LetOperator::Equals => {
                lower_let_equals_statement(
                    &let_stmt.binder,
                    let_stmt.value.as_ref(),
                    state,
                    program,
                )?;
                Ok(false)
            }
            LetOperator::LeftArrow => {
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
                heap_slots: program.heap_slots,
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
                    heap_slots: then_program.heap_slots,
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
                program.heap_slots = program.heap_slots.max(else_program.heap_slots);
                next_array_slot = next_array_slot.max(else_state.next_array_slot);
                next_i32_slot = next_i32_slot.max(else_state.next_i32_slot);
            } else {
                program.data = then_program.data;
                program.arrays = then_program.arrays;
                program.heap_slots = program.heap_slots.max(then_program.heap_slots);
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
                heap_slots: program.heap_slots,
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
            program.heap_slots = program.heap_slots.max(loop_program.heap_slots);
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
        && let Value::I32(expr) = value {
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
        Term::MethodCall { receiver, method } if method == "as_bytes" => {
            match resolve_value(receiver.as_ref(), &state.environment)? {
                Value::ByteString(data_index) => Ok(Value::ByteString(data_index)),
                other => Err(Error::Unsupported(format!(
                    "`as_bytes` expects a string reference, got {other:?}"
                ))),
            }
        }
        Term::IntegerLiteral(_) | Term::Application { .. } | Term::MethodCall { .. } => {
            if let Term::Application { callee, arguments } = term
                && let Some(value) =
                    lower_constructor_application(callee.as_ref(), arguments, state, program)?
                {
                    return Ok(value);
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
                is_rc: type_decl.modifier.as_deref() == Some("rc"),
                tag: constructors.len() as i32,
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

fn constructor_arity(
    constructor: &ConstructorDeclaration,
    type_name: &DeclaredName,
) -> Option<usize> {
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
        heap_slot: None,
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
    pattern_match_bindings_with_mode(pattern, value, constructors, false)
}

fn pattern_match_bindings_with_mode(
    pattern: &Pattern,
    value: &Value,
    constructors: &HashMap<String, ConstructorSignature>,
    bind_as_reference: bool,
) -> Result<Option<HashMap<String, Value>>> {
    match pattern {
        Pattern::Wildcard => Ok(Some(HashMap::new())),
        Pattern::Bind(name) => {
            let mut bindings = HashMap::new();
            if bind_as_reference {
                // TODO: If `type(rc)` later grows borrowed-match semantics, thread them here.
            }
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
            for (index, (subpattern, field)) in
                subpatterns.iter().zip(actual.fields.iter()).enumerate()
            {
                let _ = (index, actual.heap_slot);
                let field_value = field.clone();
                let Some(sub_bindings) = pattern_match_bindings_with_mode(
                    subpattern,
                    &field_value,
                    constructors,
                    expected.is_rc,
                )?
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

    if signature.is_rc {
        let heap_slot = allocate_heap_slot(program);
        let object_len = 8 + normalized_arguments.len() as i32 * 4;
        program.operations.push(Operation::Mmap {
            len: I32Expr::Literal(object_len),
            result_slot: heap_slot,
        });
        program.operations.push(Operation::HeapStoreI32 {
            heap_slot,
            byte_offset: 0,
            value: I32Expr::Literal(signature.tag),
        });
        // TODO: Replace this reserved header word with an actual reference count.
        program.operations.push(Operation::HeapStoreI32 {
            heap_slot,
            byte_offset: 4,
            value: I32Expr::Literal(0),
        });
        for (index, field) in fields.iter().enumerate() {
            let Value::I32(value) = field else {
                return Err(Error::Unsupported(
                    "`type(rc)` currently supports only `i32` payload fields".to_string(),
                ));
            };
            program.operations.push(Operation::HeapStoreI32 {
                heap_slot,
                byte_offset: 8 + index as i32 * 4,
                value: value.clone(),
            });
        }

        return Ok(Some(Value::Constructor(ConstructorValue {
            type_name: signature.type_name,
            constructor_name: signature.constructor_name,
            heap_slot: Some(heap_slot),
            fields,
        })));
    }

    Ok(Some(Value::Constructor(ConstructorValue {
        type_name: signature.type_name,
        constructor_name: signature.constructor_name,
        heap_slot: None,
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

fn nul_terminated_bytes(value: &str) -> Vec<u8> {
    let mut bytes = value.as_bytes().to_vec();
    bytes.push(0);
    bytes
}
