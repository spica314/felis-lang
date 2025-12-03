use neco_felis_syn::*;
use neco_scope::ScopeStack;

pub use crate::phase_elaborated::{
    NameId, PhaseElaborated, ProcTermApplyIds, ProcTermConstructorCallIds, ProcTermFieldAccessIds,
    ProcTermIds, ProcTermIfIds, ProcTermNumberIds, ProcTermParenIds, ProcTermStructValueIds,
    ProcTermUnitIds, ProcTermVariableIds, StatementCallPtxIds, StatementLetMutIds, TermApplyIds,
    TermArrowDepIds, TermArrowNodepIds, TermConstructorCallIds, TermFieldAccessIds, TermId,
    TermMatchIds, TermNumberIds, TermParenIds, TermStringIds, TermStructIds, TermUnitIds,
    TermVariableIds,
};

pub mod phase_elaborated;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElaborationError {
    UnboundVariable { name: String },
}

impl std::fmt::Display for ElaborationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ElaborationError::UnboundVariable { name } => {
                write!(f, "unbound variable: {name}")
            }
        }
    }
}

impl std::error::Error for ElaborationError {}

type ElaborationResult<T> = Result<T, ElaborationError>;

struct ElaborationContext {
    file_id: usize,
    next_name_id: usize,
    next_term_id: usize,
    scope: ScopeStack<String, NameId>,
}

impl ElaborationContext {
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

    fn expect_variable(&self, name: &str) -> ElaborationResult<NameId> {
        self.lookup_variable(name)
            .ok_or_else(|| ElaborationError::UnboundVariable {
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

pub fn elaborate_file(file: &File<PhaseParse>) -> ElaborationResult<File<PhaseElaborated>> {
    elaborate_file_with_id(file, 0)
}

pub fn elaborate_file_with_id(
    file: &File<PhaseParse>,
    file_id: usize,
) -> ElaborationResult<File<PhaseElaborated>> {
    let mut context = ElaborationContext::new(file_id);

    let mut items = Vec::with_capacity(file.items().len());
    for item in file.items() {
        items.push(elaborate_item(&mut context, item)?);
    }

    Ok(File { items, ext: () })
}

fn elaborate_item(
    context: &mut ElaborationContext,
    item: &Item<PhaseParse>,
) -> ElaborationResult<Item<PhaseElaborated>> {
    match item {
        Item::Definition(definition) => {
            elaborate_definition(context, definition).map(Item::Definition)
        }
        Item::Inductive(inductive) => elaborate_inductive(context, inductive).map(Item::Inductive),
        Item::Theorem(theorem) => elaborate_theorem(context, theorem).map(Item::Theorem),
        Item::Entrypoint(entrypoint) => {
            elaborate_entrypoint(context, entrypoint).map(Item::Entrypoint)
        }
        Item::UseBuiltin(use_builtin) => {
            elaborate_use_builtin(context, use_builtin).map(Item::UseBuiltin)
        }
        Item::Proc(proc) => elaborate_proc(context, proc).map(|p| Item::Proc(Box::new(p))),
        Item::Struct(struct_item) => elaborate_struct(context, struct_item).map(Item::Struct),
    }
}

fn elaborate_definition(
    context: &mut ElaborationContext,
    definition: &ItemDefinition<PhaseParse>,
) -> ElaborationResult<ItemDefinition<PhaseElaborated>> {
    let def_id = context.bind_variable(definition.name().s());

    context.enter_scope();
    let ty = elaborate_term(context, definition.type_())?;
    let body = elaborate_term(context, definition.body())?;
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

fn elaborate_inductive(
    context: &mut ElaborationContext,
    inductive: &ItemInductive<PhaseParse>,
) -> ElaborationResult<ItemInductive<PhaseElaborated>> {
    let ind_id = context.bind_variable(inductive.name().s());

    context.enter_scope();
    let ty = elaborate_term(context, inductive.ty())?;
    let branches = inductive
        .branches()
        .iter()
        .map(|branch| elaborate_inductive_branch(context, branch))
        .collect::<ElaborationResult<Vec<_>>>()?;
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

fn elaborate_inductive_branch(
    context: &mut ElaborationContext,
    branch: &ItemInductiveBranch<PhaseParse>,
) -> ElaborationResult<ItemInductiveBranch<PhaseElaborated>> {
    context.enter_scope();
    let _ctor_id = context.bind_variable(branch.name().s());
    let ty = elaborate_term(context, branch.ty())?;
    context.leave_scope();

    Ok(ItemInductiveBranch {
        name: branch.name.clone(),
        colon: branch.colon.clone(),
        ty: Box::new(ty),
        comma: branch.comma.clone(),
        ext: (),
    })
}

fn elaborate_theorem(
    context: &mut ElaborationContext,
    theorem: &ItemTheorem<PhaseParse>,
) -> ElaborationResult<ItemTheorem<PhaseElaborated>> {
    let thm_id = context.bind_variable(theorem.name().s());

    context.enter_scope();
    let ty = elaborate_term(context, theorem.type_())?;
    let body = elaborate_term(context, theorem.body())?;
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

fn elaborate_entrypoint(
    context: &mut ElaborationContext,
    entrypoint: &ItemEntrypoint<PhaseParse>,
) -> ElaborationResult<ItemEntrypoint<PhaseElaborated>> {
    let target_id = context.get_or_placeholder(entrypoint.name.s());

    Ok(ItemEntrypoint {
        keyword_entrypoint: entrypoint.keyword_entrypoint.clone(),
        name: entrypoint.name.clone(),
        semicolon: entrypoint.semicolon.clone(),
        ext: target_id,
    })
}

fn elaborate_use_builtin(
    context: &mut ElaborationContext,
    use_builtin: &ItemUseBuiltin<PhaseParse>,
) -> ElaborationResult<ItemUseBuiltin<PhaseElaborated>> {
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

fn elaborate_proc(
    context: &mut ElaborationContext,
    proc: &ItemProc<PhaseParse>,
) -> ElaborationResult<ItemProc<PhaseElaborated>> {
    let proc_id = context.bind_variable(proc.name.s());

    // Elaborate type with existing context (arrow scopes are handled inside elaborate_term)
    let ty = elaborate_term(context, proc.ty.as_ref())?;
    let params = collect_proc_parameters(&ty);

    // Elaborate procedure body in a new scope with parameters bound to their IDs
    context.enter_scope();
    for (name, id) in &params {
        context.bind_variable_with_id(name, id.clone());
    }
    let proc_block = elaborate_proc_block(context, &proc.proc_block)?;
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

fn collect_proc_parameters(term: &Term<PhaseElaborated>) -> Vec<(String, NameId)> {
    fn collect(term: &Term<PhaseElaborated>, out: &mut Vec<(String, NameId)>) {
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

fn elaborate_proc_block(
    context: &mut ElaborationContext,
    block: &ItemProcBlock<PhaseParse>,
) -> ElaborationResult<ItemProcBlock<PhaseElaborated>> {
    let statements = elaborate_statements(context, &block.statements)?;

    Ok(ItemProcBlock {
        brace_l: block.brace_l.clone(),
        statements,
        brace_r: block.brace_r.clone(),
        ext: (),
    })
}

fn elaborate_struct(
    context: &mut ElaborationContext,
    struct_item: &ItemStruct<PhaseParse>,
) -> ElaborationResult<ItemStruct<PhaseElaborated>> {
    let struct_id = context.bind_variable(struct_item.name.s());

    let fields = struct_item
        .fields()
        .iter()
        .map(|field| {
            let ty = elaborate_term(context, field.ty.as_ref())?;
            Ok(ItemStructField {
                name: field.name.clone(),
                colon: field.colon.clone(),
                ty: Box::new(ty),
                comma: field.comma.clone(),
                ext: (),
            })
        })
        .collect::<ElaborationResult<Vec<_>>>()?;

    Ok(ItemStruct {
        keyword_struct: struct_item.keyword_struct.clone(),
        name: struct_item.name.clone(),
        brace_l: struct_item.brace_l.clone(),
        fields,
        brace_r: struct_item.brace_r.clone(),
        ext: struct_id,
    })
}

fn elaborate_statements(
    context: &mut ElaborationContext,
    statements: &Statements<PhaseParse>,
) -> ElaborationResult<Statements<PhaseElaborated>> {
    match statements {
        Statements::Nil => Ok(Statements::Nil),
        Statements::Statement(statement) => {
            let stmt = elaborate_statement(context, statement)?;
            Ok(Statements::Statement(Box::new(stmt)))
        }
        Statements::Then(then) => {
            let head = elaborate_statement(context, &then.head)?;
            let tail = elaborate_statements(context, &then.tail)?;
            Ok(Statements::Then(StatementsThen {
                head: Box::new(head),
                semicolon: then.semicolon.clone(),
                tail: Box::new(tail),
                ext: (),
            }))
        }
    }
}

fn elaborate_statement(
    context: &mut ElaborationContext,
    statement: &Statement<PhaseParse>,
) -> ElaborationResult<Statement<PhaseElaborated>> {
    match statement {
        Statement::Let(let_stmt) => {
            let value = elaborate_proc_term(context, &let_stmt.value)?;
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
            let value = elaborate_proc_term(context, &let_mut_stmt.value)?;
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
            let value = elaborate_proc_term(context, &assign_stmt.value)?;
            Ok(Statement::Assign(StatementAssign {
                variable: assign_stmt.variable.clone(),
                equals: assign_stmt.equals.clone(),
                value: Box::new(value),
                ext: target_id,
            }))
        }
        Statement::FieldAssign(field_assign_stmt) => {
            let method_chain =
                elaborate_proc_method_chain(context, &field_assign_stmt.method_chain)?;
            let value = elaborate_proc_term(context, &field_assign_stmt.value)?;
            Ok(Statement::FieldAssign(StatementMethodChainAssign {
                method_chain,
                equals: field_assign_stmt.equals.clone(),
                value: Box::new(value),
                ext: (),
            }))
        }
        Statement::Loop(loop_stmt) => {
            context.enter_scope();
            let body = elaborate_statements(context, &loop_stmt.body)?;
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
            let value = elaborate_proc_term(context, &return_stmt.value)?;
            Ok(Statement::Return(StatementReturn {
                keyword_return: return_stmt.keyword_return.clone(),
                value: Box::new(value),
                semicolon: return_stmt.semicolon.clone(),
                ext: (),
            }))
        }
        Statement::Expr(proc_term) => {
            let elaborated = elaborate_proc_term(context, proc_term)?;
            Ok(Statement::Expr(elaborated))
        }
        Statement::Ext(_) => unreachable!("Ext statements are not supported in PhaseParse"),
    }
}

fn elaborate_proc_term(
    context: &mut ElaborationContext,
    proc_term: &ProcTerm<PhaseParse>,
) -> ElaborationResult<ProcTerm<PhaseElaborated>> {
    match proc_term {
        ProcTerm::Apply(apply) => {
            let term_id = context.generate_term_id();
            let f = elaborate_proc_term(context, &apply.f)?;
            let args = apply
                .args
                .iter()
                .map(|arg| elaborate_proc_term(context, arg))
                .collect::<ElaborationResult<Vec<_>>>()?;
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
                Some(idx) => Some(Box::new(elaborate_proc_term(context, idx)?)),
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
            let elaborated = elaborate_proc_method_chain(context, method_chain)?;
            Ok(ProcTerm::MethodChain(elaborated))
        }
        ProcTerm::ConstructorCall(constructor_call) => {
            let term_id = context.generate_term_id();
            let args = constructor_call
                .args
                .iter()
                .map(|arg| elaborate_proc_term(context, arg))
                .collect::<ElaborationResult<Vec<_>>>()?;
            Ok(ProcTerm::ConstructorCall(ProcTermConstructorCall {
                type_name: constructor_call.type_name.clone(),
                type_args: constructor_call.type_args.clone(),
                colon2: constructor_call.colon2.clone(),
                method: constructor_call.method.clone(),
                args,
                ext: ProcTermConstructorCallIds { term_id },
            }))
        }
        ProcTerm::Struct(_item_struct) => {
            unreachable!("Struct definitions are not supported inside proc terms")
        }
        ProcTerm::StructValue(struct_value) => {
            let term_id = context.generate_term_id();
            let fields = struct_value
                .fields
                .iter()
                .map(|field| {
                    let value = elaborate_proc_term(context, &field.value)?;
                    Ok(ProcTermStructField {
                        name: field.name.clone(),
                        colon: field.colon.clone(),
                        value: Box::new(value),
                        comma: field.comma.clone(),
                    })
                })
                .collect::<ElaborationResult<Vec<_>>>()?;
            Ok(ProcTerm::StructValue(ProcTermStructValue {
                struct_name: struct_value.struct_name.clone(),
                brace_l: struct_value.brace_l.clone(),
                fields,
                brace_r: struct_value.brace_r.clone(),
                ext: ProcTermStructValueIds { term_id },
            }))
        }
        ProcTerm::If(if_expr) => {
            let term_id = context.generate_term_id();
            context.enter_scope();
            let condition = elaborate_statements(context, &if_expr.condition)?;
            context.leave_scope();

            context.enter_scope();
            let then_body = elaborate_statements(context, &if_expr.then_body)?;
            context.leave_scope();

            let else_clause = if let Some(else_clause) = &if_expr.else_clause {
                context.enter_scope();
                let else_body = elaborate_statements(context, &else_clause.else_body)?;
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
        ProcTerm::Paren(paren) => {
            let inner = elaborate_proc_term(context, &paren.proc_term)?;
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

fn elaborate_proc_method_chain(
    context: &mut ElaborationContext,
    method_chain: &ProcTermMethodChain<PhaseParse>,
) -> ElaborationResult<ProcTermMethodChain<PhaseElaborated>> {
    let term_id = context.generate_term_id();
    let object_id = context.get_or_placeholder(method_chain.object.s());
    let index = match &method_chain.index {
        Some(idx) => Some(Box::new(elaborate_proc_term(context, idx)?)),
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

fn elaborate_term(
    context: &mut ElaborationContext,
    term: &Term<PhaseParse>,
) -> ElaborationResult<Term<PhaseElaborated>> {
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
            let f = elaborate_term(context, apply.f())?;
            let args = apply
                .args()
                .iter()
                .map(|arg| elaborate_term(context, arg))
                .collect::<ElaborationResult<Vec<_>>>()?;
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
            let from_ty = elaborate_term(context, arrow.from_ty())?;
            let to = elaborate_term(context, arrow.to())?;
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
            let from = elaborate_term(context, arrow.from())?;
            let to = elaborate_term(context, arrow.to())?;
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
                .map(|branch| elaborate_match_branch(context, branch))
                .collect::<ElaborationResult<Vec<_>>>()?;
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
            let inner = elaborate_term(context, paren.term())?;
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
                    let ty = elaborate_term(context, field.ty.as_ref())?;
                    Ok(TermStructField {
                        name: field.name.clone(),
                        colon: field.colon.clone(),
                        ty: Box::new(ty),
                        comma: field.comma.clone(),
                    })
                })
                .collect::<ElaborationResult<Vec<_>>>()?;
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

fn elaborate_match_branch(
    context: &mut ElaborationContext,
    branch: &TermMatchBranch<PhaseParse>,
) -> ElaborationResult<TermMatchBranch<PhaseElaborated>> {
    context.enter_scope();
    elaborate_pattern_bindings(context, &branch.pattern)?;
    let body = elaborate_term(context, branch.body())?;
    context.leave_scope();

    Ok(TermMatchBranch {
        pattern: branch.pattern.clone(),
        arrow: branch.arrow.clone(),
        body: Box::new(body),
        comma: branch.comma.clone(),
        ext: (),
    })
}

fn elaborate_pattern_bindings(
    context: &mut ElaborationContext,
    pattern: &Pattern,
) -> ElaborationResult<()> {
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
    fn test_elaborate_simple_variable() {
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let source = "#definition x : nat { y }";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let parsed_file = File::parse(&tokens, &mut i).unwrap().unwrap();

        let elaborated_file = elaborate_file(&parsed_file).unwrap();

        assert_eq!(elaborated_file.items.len(), 1);

        if let Item::Definition(def) = &elaborated_file.items[0] {
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
