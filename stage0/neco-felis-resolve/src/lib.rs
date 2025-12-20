use neco_felis_syn::*;
use neco_scope::ScopeStack;

pub use crate::phase_resolved::{
    ItemTypeIds, NameId, PhaseResolved, ProcTermApplyIds, ProcTermConstructorCallIds,
    ProcTermFieldAccessIds, ProcTermIds, ProcTermIfIds, ProcTermMatchFieldIds, ProcTermMatchIds,
    ProcTermNumberIds, ProcTermParenIds, ProcTermStructValueIds, ProcTermUnitIds,
    ProcTermVariableIds, StatementCallPtxIds, StatementLetMutIds, TermApplyIds, TermArrowDepIds,
    TermArrowNodepIds, TermConstructorCallIds, TermFieldAccessIds, TermId, TermMatchIds,
    TermNumberIds, TermParenIds, TermStringIds, TermStructIds, TermUnitIds, TermVariableIds,
};

pub mod phase_resolved;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    UnboundVariable { name: String },
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::UnboundVariable { name } => {
                write!(f, "unbound variable: {name}")
            }
        }
    }
}

impl std::error::Error for ResolveError {}

type ResolveResult<T> = Result<T, ResolveError>;

struct ResolveContext {
    file_id: usize,
    next_name_id: usize,
    next_term_id: usize,
    scope: ScopeStack<String, NameId>,
}

impl ResolveContext {
    fn new(file_id: usize) -> Self {
        let mut scope = ScopeStack::new();
        scope.enter_scope(); // Ensure top-level bindings (e.g., builtins) have a scope.
        Self {
            file_id,
            next_name_id: 0,
            next_term_id: 0,
            scope,
        }
    }

    fn generate_name_id(&mut self) -> NameId {
        let id = NameId(self.file_id, self.next_name_id);
        self.next_name_id += 1;
        id
    }

    fn generate_term_id(&mut self) -> TermId {
        let id = TermId(self.file_id, self.next_term_id);
        self.next_term_id += 1;
        id
    }

    fn bind_variable(&mut self, name: &str) -> NameId {
        let id = self.generate_name_id();
        self.scope.set(name.to_string(), id.clone());
        id
    }

    fn bind_variable_with_id(&mut self, name: &str, id: NameId) {
        self.next_name_id = self.next_name_id.max(id.1 + 1);
        self.scope.set(name.to_string(), id);
    }

    fn lookup_variable(&self, name: &str) -> Option<NameId> {
        self.scope.get(&name.to_string()).cloned()
    }

    fn expect_variable(&self, name: &str) -> ResolveResult<NameId> {
        self.lookup_variable(name)
            .ok_or_else(|| ResolveError::UnboundVariable {
                name: name.to_string(),
            })
    }

    fn placeholder_id(&self) -> NameId {
        NameId(self.file_id, usize::MAX)
    }

    fn get_or_placeholder(&self, name: &str) -> NameId {
        self.lookup_variable(name)
            .unwrap_or_else(|| self.placeholder_id())
    }

    fn enter_scope(&mut self) {
        self.scope.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.scope.leave_scope();
    }
}

pub fn resolve_file(file: &File<PhaseParse>) -> ResolveResult<File<PhaseResolved>> {
    resolve_file_with_id(file, 0)
}

pub fn resolve_file_with_id(
    file: &File<PhaseParse>,
    file_id: usize,
) -> ResolveResult<File<PhaseResolved>> {
    let mut context = ResolveContext::new(file_id);

    let mut items = Vec::with_capacity(file.items().len());
    for item in file.items() {
        items.push(resolve_item(&mut context, item)?);
    }

    Ok(File { items, ext: () })
}

fn resolve_item(
    context: &mut ResolveContext,
    item: &Item<PhaseParse>,
) -> ResolveResult<Item<PhaseResolved>> {
    match item {
        Item::Definition(definition) => {
            resolve_definition(context, definition).map(Item::Definition)
        }
        Item::Inductive(inductive) => resolve_inductive(context, inductive).map(Item::Inductive),
        Item::Theorem(theorem) => resolve_theorem(context, theorem).map(Item::Theorem),
        Item::Entrypoint(entrypoint) => {
            resolve_entrypoint(context, entrypoint).map(Item::Entrypoint)
        }
        Item::UseBuiltin(use_builtin) => {
            resolve_use_builtin(context, use_builtin).map(Item::UseBuiltin)
        }
        Item::Proc(proc) => resolve_proc(context, proc).map(|p| Item::Proc(Box::new(p))),
        Item::Type(type_item) => resolve_type(context, type_item).map(Item::Type),
    }
}

fn resolve_definition(
    context: &mut ResolveContext,
    definition: &ItemDefinition<PhaseParse>,
) -> ResolveResult<ItemDefinition<PhaseResolved>> {
    let def_id = context.bind_variable(definition.name().s());

    context.enter_scope();
    let ty = resolve_term(context, definition.type_())?;
    let body = resolve_term(context, definition.body())?;
    context.leave_scope();

    Ok(ItemDefinition {
        keyword_definition: definition.keyword_definition.clone(),
        name: definition.name.clone(),
        colon: definition.colon.clone(),
        type_: Box::new(ty),
        brace_l: definition.brace_l.clone(),
        body: Box::new(body),
        brace_r: definition.brace_r.clone(),
        ext: def_id,
    })
}

fn resolve_inductive(
    context: &mut ResolveContext,
    inductive: &ItemInductive<PhaseParse>,
) -> ResolveResult<ItemInductive<PhaseResolved>> {
    let ind_id = context.bind_variable(inductive.name().s());

    context.enter_scope();
    let ty = resolve_term(context, inductive.ty())?;
    let branches = inductive
        .branches()
        .iter()
        .map(|branch| resolve_inductive_branch(context, branch))
        .collect::<ResolveResult<Vec<_>>>()?;
    context.leave_scope();

    Ok(ItemInductive {
        keyword_inductive: inductive.keyword_inductive.clone(),
        name: inductive.name.clone(),
        colon: inductive.colon.clone(),
        ty: Box::new(ty),
        brace_l: inductive.brace_l.clone(),
        branches,
        brace_r: inductive.brace_r.clone(),
        ext: ind_id,
    })
}

fn resolve_inductive_branch(
    context: &mut ResolveContext,
    branch: &ItemInductiveBranch<PhaseParse>,
) -> ResolveResult<ItemInductiveBranch<PhaseResolved>> {
    context.enter_scope();
    let _ctor_id = context.bind_variable(branch.name().s());
    let ty = resolve_term(context, branch.ty())?;
    context.leave_scope();

    Ok(ItemInductiveBranch {
        name: branch.name.clone(),
        colon: branch.colon.clone(),
        ty: Box::new(ty),
        comma: branch.comma.clone(),
        ext: (),
    })
}

fn resolve_theorem(
    context: &mut ResolveContext,
    theorem: &ItemTheorem<PhaseParse>,
) -> ResolveResult<ItemTheorem<PhaseResolved>> {
    let thm_id = context.bind_variable(theorem.name().s());

    context.enter_scope();
    let ty = resolve_term(context, theorem.type_())?;
    let body = resolve_term(context, theorem.body())?;
    context.leave_scope();

    Ok(ItemTheorem {
        keyword_theorem: theorem.keyword_theorem.clone(),
        name: theorem.name.clone(),
        colon: theorem.colon.clone(),
        type_: Box::new(ty),
        brace_l: theorem.brace_l.clone(),
        body: Box::new(body),
        brace_r: theorem.brace_r.clone(),
        ext: thm_id,
    })
}

fn resolve_entrypoint(
    context: &mut ResolveContext,
    entrypoint: &ItemEntrypoint<PhaseParse>,
) -> ResolveResult<ItemEntrypoint<PhaseResolved>> {
    let target_id = context.get_or_placeholder(entrypoint.name.s());

    Ok(ItemEntrypoint {
        keyword_entrypoint: entrypoint.keyword_entrypoint.clone(),
        name: entrypoint.name.clone(),
        semicolon: entrypoint.semicolon.clone(),
        ext: target_id,
    })
}

fn resolve_use_builtin(
    context: &mut ResolveContext,
    use_builtin: &ItemUseBuiltin<PhaseParse>,
) -> ResolveResult<ItemUseBuiltin<PhaseResolved>> {
    let alias_id = context.bind_variable(use_builtin.name.s());

    Ok(ItemUseBuiltin {
        keyword_use_builtin: use_builtin.keyword_use_builtin.clone(),
        builtin_name: use_builtin.builtin_name.clone(),
        keyword_as: use_builtin.keyword_as.clone(),
        name: use_builtin.name.clone(),
        semicolon: use_builtin.semicolon.clone(),
        ext: alias_id,
    })
}

fn resolve_proc(
    context: &mut ResolveContext,
    proc: &ItemProc<PhaseParse>,
) -> ResolveResult<ItemProc<PhaseResolved>> {
    let proc_id = context.bind_variable(proc.name.s());

    // Resolve type with existing context (arrow scopes are handled inside resolve_term)
    let ty = resolve_term(context, proc.ty.as_ref())?;
    let params = collect_proc_parameters(&ty);

    // Resolve procedure body in a new scope with parameters bound to their IDs
    context.enter_scope();
    for (name, id) in &params {
        context.bind_variable_with_id(name, id.clone());
    }
    let proc_block = resolve_proc_block(context, &proc.proc_block)?;
    context.leave_scope();

    Ok(ItemProc {
        keyword_proc: proc.keyword_proc.clone(),
        name: proc.name.clone(),
        colon: proc.colon.clone(),
        ty: Box::new(ty),
        proc_block,
        ext: proc_id,
    })
}

fn collect_proc_parameters(term: &Term<PhaseResolved>) -> Vec<(String, NameId)> {
    fn collect(term: &Term<PhaseResolved>, out: &mut Vec<(String, NameId)>) {
        match term {
            Term::ArrowDep(arrow) => {
                out.push((
                    arrow.from.variable.s().to_string(),
                    arrow.ext.param_id.clone(),
                ));
                collect(&arrow.to, out);
            }
            Term::ArrowNodep(arrow) => {
                collect(&arrow.to, out);
            }
            _ => {}
        }
    }

    let mut params = Vec::new();
    collect(term, &mut params);
    params
}

fn resolve_proc_block(
    context: &mut ResolveContext,
    block: &ItemProcBlock<PhaseParse>,
) -> ResolveResult<ItemProcBlock<PhaseResolved>> {
    let statements = resolve_statements(context, &block.statements)?;

    Ok(ItemProcBlock {
        brace_l: block.brace_l.clone(),
        statements,
        brace_r: block.brace_r.clone(),
        ext: (),
    })
}

fn resolve_type(
    context: &mut ResolveContext,
    type_item: &ItemType<PhaseParse>,
) -> ResolveResult<ItemType<PhaseResolved>> {
    let type_id = context.bind_variable(type_item.name.s());

    let constructors = type_item
        .constructors()
        .iter()
        .map(|constructor| resolve_type_constructor(context, constructor))
        .collect::<ResolveResult<Vec<_>>>()?;

    Ok(ItemType {
        keyword_type: type_item.keyword_type.clone(),
        name: type_item.name.clone(),
        brace_l: type_item.brace_l.clone(),
        constructors,
        brace_r: type_item.brace_r.clone(),
        ext: ItemTypeIds { name_id: type_id },
    })
}

fn resolve_type_constructor(
    context: &mut ResolveContext,
    constructor: &ItemTypeConstructor<PhaseParse>,
) -> ResolveResult<ItemTypeConstructor<PhaseResolved>> {
    let mut fields = Vec::with_capacity(constructor.fields.len());
    for field in &constructor.fields {
        let ty = resolve_term(context, field.ty.as_ref())?;
        fields.push(ItemStructField {
            name: field.name.clone(),
            colon: field.colon.clone(),
            ty: Box::new(ty),
            comma: field.comma.clone(),
            ext: (),
        });
    }
    Ok(ItemTypeConstructor {
        name: constructor.name.clone(),
        brace_l: constructor.brace_l.clone(),
        fields,
        brace_r: constructor.brace_r.clone(),
        comma: constructor.comma.clone(),
        ext: (),
    })
}

fn resolve_statements(
    context: &mut ResolveContext,
    statements: &Statements<PhaseParse>,
) -> ResolveResult<Statements<PhaseResolved>> {
    match statements {
        Statements::Nil => Ok(Statements::Nil),
        Statements::Statement(statement) => {
            let stmt = resolve_statement(context, statement)?;
            Ok(Statements::Statement(Box::new(stmt)))
        }
        Statements::Then(then) => {
            let head = resolve_statement(context, &then.head)?;
            let tail = resolve_statements(context, &then.tail)?;
            Ok(Statements::Then(StatementsThen {
                head: Box::new(head),
                semicolon: then.semicolon.clone(),
                tail: Box::new(tail),
                ext: (),
            }))
        }
    }
}

fn resolve_statement(
    context: &mut ResolveContext,
    statement: &Statement<PhaseParse>,
) -> ResolveResult<Statement<PhaseResolved>> {
    match statement {
        Statement::Let(let_stmt) => {
            let value = resolve_proc_term(context, &let_stmt.value)?;
            let var_id = context.bind_variable(let_stmt.variable.s());
            Ok(Statement::Let(StatementLet {
                let_keyword: let_stmt.let_keyword.clone(),
                variable: let_stmt.variable.clone(),
                equals: let_stmt.equals.clone(),
                value: Box::new(value),
                ext: var_id,
            }))
        }
        Statement::LetMut(let_mut_stmt) => {
            let value = resolve_proc_term(context, &let_mut_stmt.value)?;
            let value_id = context.bind_variable(let_mut_stmt.variable.s());
            let reference_id = context.bind_variable(let_mut_stmt.reference_variable.s());
            Ok(Statement::LetMut(StatementLetMut {
                let_keyword: let_mut_stmt.let_keyword.clone(),
                mut_keyword: let_mut_stmt.mut_keyword.clone(),
                variable: let_mut_stmt.variable.clone(),
                at_operator: let_mut_stmt.at_operator.clone(),
                reference_variable: let_mut_stmt.reference_variable.clone(),
                equals: let_mut_stmt.equals.clone(),
                value: Box::new(value),
                ext: StatementLetMutIds {
                    value_id,
                    reference_id,
                },
            }))
        }
        Statement::Assign(assign_stmt) => {
            let target_id = context.expect_variable(assign_stmt.variable.s())?;
            let value = resolve_proc_term(context, &assign_stmt.value)?;
            Ok(Statement::Assign(StatementAssign {
                variable: assign_stmt.variable.clone(),
                equals: assign_stmt.equals.clone(),
                value: Box::new(value),
                ext: target_id,
            }))
        }
        Statement::FieldAssign(field_assign_stmt) => {
            let method_chain = resolve_proc_method_chain(context, &field_assign_stmt.method_chain)?;
            let value = resolve_proc_term(context, &field_assign_stmt.value)?;
            Ok(Statement::FieldAssign(StatementMethodChainAssign {
                method_chain,
                equals: field_assign_stmt.equals.clone(),
                value: Box::new(value),
                ext: (),
            }))
        }
        Statement::Loop(loop_stmt) => {
            context.enter_scope();
            let body = resolve_statements(context, &loop_stmt.body)?;
            context.leave_scope();
            Ok(Statement::Loop(StatementLoop {
                keyword_loop: loop_stmt.keyword_loop.clone(),
                brace_l: loop_stmt.brace_l.clone(),
                body: Box::new(body),
                brace_r: loop_stmt.brace_r.clone(),
                ext: (),
            }))
        }
        Statement::Break(break_stmt) => Ok(Statement::Break(StatementBreak {
            keyword_break: break_stmt.keyword_break.clone(),
            semicolon: break_stmt.semicolon.clone(),
            ext: (),
        })),
        Statement::Return(return_stmt) => {
            let value = resolve_proc_term(context, &return_stmt.value)?;
            Ok(Statement::Return(StatementReturn {
                keyword_return: return_stmt.keyword_return.clone(),
                value: Box::new(value),
                semicolon: return_stmt.semicolon.clone(),
                ext: (),
            }))
        }
        Statement::Expr(proc_term) => {
            let resolved = resolve_proc_term(context, proc_term)?;
            Ok(Statement::Expr(resolved))
        }
        Statement::Ext(_) => unreachable!("Ext statements are not supported in PhaseParse"),
    }
}

fn resolve_proc_term(
    context: &mut ResolveContext,
    proc_term: &ProcTerm<PhaseParse>,
) -> ResolveResult<ProcTerm<PhaseResolved>> {
    match proc_term {
        ProcTerm::Apply(apply) => {
            let term_id = context.generate_term_id();
            let f = resolve_proc_term(context, &apply.f)?;
            let args = apply
                .args
                .iter()
                .map(|arg| resolve_proc_term(context, arg))
                .collect::<ResolveResult<Vec<_>>>()?;
            Ok(ProcTerm::Apply(ProcTermApply {
                f: Box::new(f),
                args,
                ext: ProcTermApplyIds { term_id },
            }))
        }
        ProcTerm::Variable(var) => {
            let term_id = context.generate_term_id();
            let name_id = context.get_or_placeholder(var.variable.s());
            Ok(ProcTerm::Variable(ProcTermVariable {
                variable: var.variable.clone(),
                ext: ProcTermVariableIds { term_id, name_id },
            }))
        }
        ProcTerm::Unit(unit) => {
            let term_id = context.generate_term_id();
            Ok(ProcTerm::Unit(ProcTermUnit {
                paren_l: unit.paren_l.clone(),
                paren_r: unit.paren_r.clone(),
                ext: ProcTermUnitIds { term_id },
            }))
        }
        ProcTerm::Number(number) => {
            let term_id = context.generate_term_id();
            Ok(ProcTerm::Number(ProcTermNumber {
                number: number.number.clone(),
                ext: ProcTermNumberIds { term_id },
            }))
        }
        ProcTerm::FieldAccess(field_access) => {
            let term_id = context.generate_term_id();
            let object_id = context.get_or_placeholder(field_access.object.s());
            let index = match &field_access.index {
                Some(idx) => Some(Box::new(resolve_proc_term(context, idx)?)),
                None => None,
            };
            Ok(ProcTerm::FieldAccess(ProcTermFieldAccess {
                object: field_access.object.clone(),
                dot: field_access.dot.clone(),
                field: field_access.field.clone(),
                index,
                ext: ProcTermFieldAccessIds { term_id, object_id },
            }))
        }
        ProcTerm::MethodChain(method_chain) => {
            let resolved = resolve_proc_method_chain(context, method_chain)?;
            Ok(ProcTerm::MethodChain(resolved))
        }
        ProcTerm::StructValue(struct_value) => {
            let term_id = context.generate_term_id();
            let fields = struct_value
                .fields
                .iter()
                .map(|field| {
                    let value = resolve_proc_term(context, &field.value)?;
                    Ok(ProcTermStructField {
                        name: field.name.clone(),
                        colon: field.colon.clone(),
                        value: Box::new(value),
                        comma: field.comma.clone(),
                    })
                })
                .collect::<ResolveResult<Vec<_>>>()?;
            Ok(ProcTerm::StructValue(ProcTermStructValue {
                struct_name: struct_value.struct_name.clone(),
                colon2: struct_value.colon2.clone(),
                constructor_name: struct_value.constructor_name.clone(),
                brace_l: struct_value.brace_l.clone(),
                fields,
                brace_r: struct_value.brace_r.clone(),
                ext: ProcTermStructValueIds { term_id },
            }))
        }
        ProcTerm::If(if_expr) => {
            let term_id = context.generate_term_id();
            context.enter_scope();
            let condition = resolve_statements(context, &if_expr.condition)?;
            context.leave_scope();

            context.enter_scope();
            let then_body = resolve_statements(context, &if_expr.then_body)?;
            context.leave_scope();

            let else_clause = if let Some(else_clause) = &if_expr.else_clause {
                context.enter_scope();
                let else_body = resolve_statements(context, &else_clause.else_body)?;
                context.leave_scope();
                Some(ProcTermIfElse {
                    keyword_else: else_clause.keyword_else.clone(),
                    brace_l: else_clause.brace_l.clone(),
                    else_body: Box::new(else_body),
                    brace_r: else_clause.brace_r.clone(),
                })
            } else {
                None
            };

            Ok(ProcTerm::If(ProcTermIf {
                keyword_if: if_expr.keyword_if.clone(),
                condition: Box::new(condition),
                brace_l: if_expr.brace_l.clone(),
                then_body: Box::new(then_body),
                brace_r: if_expr.brace_r.clone(),
                else_clause,
                ext: ProcTermIfIds { term_id },
            }))
        }
        ProcTerm::Match(match_expr) => {
            let term_id = context.generate_term_id();
            let scrutinee_id = context.get_or_placeholder(match_expr.scrutinee.s());
            let branches = match_expr
                .branches
                .iter()
                .map(|branch| resolve_proc_match_branch(context, branch))
                .collect::<ResolveResult<Vec<_>>>()?;
            Ok(ProcTerm::Match(ProcTermMatch {
                keyword_match: match_expr.keyword_match.clone(),
                scrutinee: match_expr.scrutinee.clone(),
                brace_l: match_expr.brace_l.clone(),
                branches,
                brace_r: match_expr.brace_r.clone(),
                ext: ProcTermMatchIds {
                    term_id,
                    scrutinee_id,
                },
            }))
        }
        ProcTerm::Paren(paren) => {
            let inner = resolve_proc_term(context, &paren.proc_term)?;
            let term_id = context.generate_term_id();
            Ok(ProcTerm::Paren(ProcTermParen {
                paren_l: paren.paren_l.clone(),
                proc_term: Box::new(inner),
                paren_r: paren.paren_r.clone(),
                ext: ProcTermParenIds { term_id },
            }))
        }
        ProcTerm::Ext(_) => unreachable!("Ext proc terms are not supported in PhaseParse"),
    }
}

fn resolve_proc_method_chain(
    context: &mut ResolveContext,
    method_chain: &ProcTermMethodChain<PhaseParse>,
) -> ResolveResult<ProcTermMethodChain<PhaseResolved>> {
    let term_id = context.generate_term_id();
    let object_id = context.get_or_placeholder(method_chain.object.s());
    let index = match &method_chain.index {
        Some(idx) => Some(Box::new(resolve_proc_term(context, idx)?)),
        None => None,
    };
    Ok(ProcTermMethodChain {
        object: method_chain.object.clone(),
        dot: method_chain.dot.clone(),
        field: method_chain.field.clone(),
        index,
        ext: ProcTermFieldAccessIds { term_id, object_id },
    })
}

fn resolve_proc_match_branch(
    context: &mut ResolveContext,
    branch: &ProcTermMatchBranch<PhaseParse>,
) -> ResolveResult<ProcTermMatchBranch<PhaseResolved>> {
    context.enter_scope();
    let mut fields = Vec::with_capacity(branch.pattern.fields.len());
    for field in &branch.pattern.fields {
        let binder_id = context.bind_variable(field.binder.s());
        fields.push(ProcTermMatchField {
            field_name: field.field_name.clone(),
            colon: field.colon.clone(),
            binder: field.binder.clone(),
            comma: field.comma.clone(),
            ext: ProcTermMatchFieldIds { binder_id },
        });
    }
    let body = resolve_statements(context, &branch.body)?;
    context.leave_scope();

    Ok(ProcTermMatchBranch {
        pattern: ProcTermMatchPattern {
            type_name: branch.pattern.type_name.clone(),
            colon2: branch.pattern.colon2.clone(),
            constructor: branch.pattern.constructor.clone(),
            brace_l: branch.pattern.brace_l.clone(),
            fields,
            brace_r: branch.pattern.brace_r.clone(),
        },
        arrow: branch.arrow.clone(),
        body: Box::new(body),
        comma: branch.comma.clone(),
        ext: (),
    })
}

fn resolve_term(
    context: &mut ResolveContext,
    term: &Term<PhaseParse>,
) -> ResolveResult<Term<PhaseResolved>> {
    match term {
        Term::Variable(var) => {
            let term_id = context.generate_term_id();
            let name_id = context.get_or_placeholder(var.variable().s());
            Ok(Term::Variable(TermVariable {
                variable: var.variable.clone(),
                ext: TermVariableIds { term_id, name_id },
            }))
        }
        Term::Apply(apply) => {
            let term_id = context.generate_term_id();
            let f = resolve_term(context, apply.f())?;
            let args = apply
                .args()
                .iter()
                .map(|arg| resolve_term(context, arg))
                .collect::<ResolveResult<Vec<_>>>()?;
            Ok(Term::Apply(TermApply {
                f: Box::new(f),
                args,
                ext: TermApplyIds { term_id },
            }))
        }
        Term::ArrowDep(arrow) => {
            let term_id = context.generate_term_id();
            context.enter_scope();
            let param_id = context.bind_variable(arrow.from().variable().s());
            let from = TermVariable {
                variable: arrow.from.variable.clone(),
                ext: TermVariableIds {
                    term_id: context.generate_term_id(),
                    name_id: param_id.clone(),
                },
            };
            let from_ty = resolve_term(context, arrow.from_ty())?;
            let to = resolve_term(context, arrow.to())?;
            context.leave_scope();
            Ok(Term::ArrowDep(TermArrowDep {
                paren_l: arrow.paren_l.clone(),
                from,
                colon: arrow.colon.clone(),
                from_ty: Box::new(from_ty),
                paren_r: arrow.paren_r.clone(),
                arrow: arrow.arrow.clone(),
                to: Box::new(to),
                ext: TermArrowDepIds { term_id, param_id },
            }))
        }
        Term::ArrowNodep(arrow) => {
            let term_id = context.generate_term_id();
            let from = resolve_term(context, arrow.from())?;
            let to = resolve_term(context, arrow.to())?;
            Ok(Term::ArrowNodep(TermArrowNodep {
                from: Box::new(from),
                arrow: arrow.arrow.clone(),
                to: Box::new(to),
                ext: TermArrowNodepIds { term_id },
            }))
        }
        Term::Match(match_term) => {
            let term_id = context.generate_term_id();
            let scrutinee_id = context.get_or_placeholder(match_term.scrutinee().s());
            let branches = match_term
                .branches()
                .iter()
                .map(|branch| resolve_match_branch(context, branch))
                .collect::<ResolveResult<Vec<_>>>()?;
            Ok(Term::Match(TermMatch {
                keyword_match: match_term.keyword_match.clone(),
                scrutinee: match_term.scrutinee.clone(),
                brace_l: match_term.brace_l.clone(),
                branches,
                brace_r: match_term.brace_r.clone(),
                ext: TermMatchIds {
                    term_id,
                    scrutinee_id,
                },
            }))
        }
        Term::Paren(paren) => {
            let inner = resolve_term(context, paren.term())?;
            let term_id = context.generate_term_id();
            Ok(Term::Paren(TermParen {
                paren_l: paren.paren_l.clone(),
                term: Box::new(inner),
                paren_r: paren.paren_r.clone(),
                ext: TermParenIds { term_id },
            }))
        }
        Term::Unit(unit) => {
            let term_id = context.generate_term_id();
            Ok(Term::Unit(TermUnit {
                paren_l: unit.paren_l.clone(),
                paren_r: unit.paren_r.clone(),
                ext: TermUnitIds { term_id },
            }))
        }
        Term::Number(number) => {
            let term_id = context.generate_term_id();
            Ok(Term::Number(TermNumber {
                number: number.number.clone(),
                ext: TermNumberIds { term_id },
            }))
        }
        Term::Struct(struct_term) => {
            let term_id = context.generate_term_id();
            let fields = struct_term
                .fields()
                .iter()
                .map(|field| {
                    let ty = resolve_term(context, field.ty.as_ref())?;
                    Ok(TermStructField {
                        name: field.name.clone(),
                        colon: field.colon.clone(),
                        ty: Box::new(ty),
                        comma: field.comma.clone(),
                    })
                })
                .collect::<ResolveResult<Vec<_>>>()?;
            Ok(Term::Struct(TermStruct {
                keyword_struct: struct_term.keyword_struct.clone(),
                brace_l: struct_term.brace_l.clone(),
                fields,
                brace_r: struct_term.brace_r.clone(),
                ext: TermStructIds { term_id },
            }))
        }
    }
}

fn resolve_match_branch(
    context: &mut ResolveContext,
    branch: &TermMatchBranch<PhaseParse>,
) -> ResolveResult<TermMatchBranch<PhaseResolved>> {
    context.enter_scope();
    resolve_pattern_bindings(context, &branch.pattern)?;
    let body = resolve_term(context, branch.body())?;
    context.leave_scope();

    Ok(TermMatchBranch {
        pattern: branch.pattern.clone(),
        arrow: branch.arrow.clone(),
        body: Box::new(body),
        comma: branch.comma.clone(),
        ext: (),
    })
}

fn resolve_pattern_bindings(context: &mut ResolveContext, pattern: &Pattern) -> ResolveResult<()> {
    match pattern {
        Pattern::Variable(var) => {
            context.bind_variable(var.s());
        }
        Pattern::Constructor(_, args) => {
            for arg in args {
                context.bind_variable(arg.s());
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use neco_felis_syn::{FileIdGenerator, Parse, token::Token};

    #[test]
    fn test_resolve_simple_variable() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let source = "#definition x : nat { y }";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let parsed_file = File::parse(&tokens, &mut i).unwrap().unwrap();

        let resolved_file = resolve_file(&parsed_file).unwrap();

        assert_eq!(resolved_file.items.len(), 1);

        if let Item::Definition(def) = &resolved_file.items[0] {
            if let Term::Variable(var) = def.body.as_ref() {
                let TermVariableIds {
                    term_id: TermId(file_id, _),
                    name_id: NameId(name_file_id, _),
                } = &var.ext;
                assert_eq!(*file_id, 0);
                assert_eq!(*name_file_id, 0);
                assert_eq!(var.variable.s(), "y");
            } else {
                panic!("Expected variable in apply function position");
            }
        } else {
            panic!("Expected definition item");
        }
    }
}
