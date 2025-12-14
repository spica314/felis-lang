use crate::{
    BuiltinTypes, IntegerType, StructFieldType, Term, TypeHole, TypeSolutions, TypingError,
    UnificationCtx,
};
use neco_felis_elaboration::{NameId, PhaseElaborated, TermId};
use neco_felis_syn::{
    File, Item, ItemDefinition, ItemInductive, ItemProc, ItemProcBlock, ItemTheorem, ItemType,
    ItemUseBuiltin, ProcTerm, ProcTermMethodChain, Statement, Statements, Term as SynTerm,
};
use neco_scope::ScopeStack;
use std::collections::HashMap;

pub struct TypeChecker {
    builtins: BuiltinTypes,
    env: ScopeStack<NameId, Term>,
    term_types: HashMap<TermId, Term>,
    solutions: TypeSolutions,
    next_hole: usize,
}

#[derive(Debug, Clone)]
pub struct TypingResult {
    pub term_types: HashMap<TermId, Term>,
    pub solutions: TypeSolutions,
}

impl TypingResult {
    pub fn resolved_type(&self, term_id: &TermId) -> Option<Term> {
        self.term_types
            .get(term_id)
            .map(|ty| self.solutions.resolve(ty))
    }
}

impl TypeChecker {
    pub fn new(builtins: BuiltinTypes) -> Self {
        let mut env = ScopeStack::new();
        env.enter_scope();
        let next_hole = builtins.max_type_hole_id().map_or(0, |max_id| max_id + 1);
        Self {
            builtins,
            env,
            term_types: HashMap::new(),
            solutions: TypeSolutions::new(),
            next_hole,
        }
    }

    pub fn check_file(mut self, file: &File<PhaseElaborated>) -> Result<TypingResult, TypingError> {
        for item in &file.items {
            self.check_item(item)?;
        }

        Ok(TypingResult {
            term_types: self.term_types,
            solutions: self.solutions,
        })
    }

    fn fresh_hole(&mut self) -> TypeHole {
        let id = self.next_hole;
        self.next_hole += 1;
        TypeHole(id)
    }

    fn bind(&mut self, name_id: NameId, ty: Term) -> Result<(), TypingError> {
        if self.env.get(&name_id).is_some() {
            return Err(TypingError::DuplicateBinding { name_id });
        }
        self.env.set(name_id, ty);
        Ok(())
    }

    fn type_for_term_id(&mut self, term_id: &TermId) -> Term {
        let hole = Term::Hole(self.fresh_hole());
        self.term_types
            .entry(term_id.clone())
            .or_insert_with(|| hole.clone())
            .clone()
    }

    fn assign_type(&mut self, term_id: &TermId, ty: Term) -> Result<Term, TypingError> {
        let current = self.type_for_term_id(term_id);
        let mut ctx = UnificationCtx::new(&mut self.solutions);
        ctx.unify(&current, &ty)
            .map_err(TypingError::UnificationFailed)?;
        let resolved = self.solutions.resolve(&current);
        self.term_types.insert(term_id.clone(), resolved.clone());
        Ok(resolved)
    }

    fn check_item(&mut self, item: &Item<PhaseElaborated>) -> Result<(), TypingError> {
        match item {
            Item::UseBuiltin(use_builtin) => self.install_builtin(use_builtin),
            Item::Definition(def) => self.check_definition(def),
            Item::Inductive(ind) => self.check_inductive(ind),
            Item::Theorem(theorem) => self.check_theorem(theorem),
            Item::Proc(proc) => self.check_proc(proc),
            Item::Type(type_item) => self.check_type(type_item),
            Item::Entrypoint(_) => Ok(()),
        }
    }

    fn install_builtin(
        &mut self,
        use_builtin: &ItemUseBuiltin<PhaseElaborated>,
    ) -> Result<(), TypingError> {
        let builtin_name = use_builtin.builtin_name.s();
        let ty = self
            .builtins
            .get(builtin_name)
            .ok_or_else(|| TypingError::MissingBuiltinType {
                builtin: builtin_name.to_string(),
            })?
            .clone();
        self.bind(use_builtin.ext.clone(), ty)
    }

    fn check_definition(
        &mut self,
        def: &ItemDefinition<PhaseElaborated>,
    ) -> Result<(), TypingError> {
        let annotated = self.visit_term(&def.type_)?;
        self.bind(def.ext.clone(), annotated)?;
        self.env.enter_scope();
        for (param_id, term_id) in collect_term_parameter_types(def.type_.as_ref()) {
            let ty = self
                .term_types
                .get(&term_id)
                .cloned()
                .unwrap_or_else(|| Term::Hole(self.fresh_hole()));
            self.bind(param_id, ty)?;
        }
        self.visit_term(&def.body)?;
        self.env.leave_scope();
        Ok(())
    }

    fn check_inductive(&mut self, ind: &ItemInductive<PhaseElaborated>) -> Result<(), TypingError> {
        let ty = self.visit_term(&ind.ty)?;
        self.bind(ind.ext.clone(), ty)?;
        for branch in &ind.branches {
            self.visit_term(&branch.ty)?;
        }
        Ok(())
    }

    fn check_theorem(&mut self, theorem: &ItemTheorem<PhaseElaborated>) -> Result<(), TypingError> {
        let ty = self.visit_term(&theorem.type_)?;
        self.bind(theorem.ext.clone(), ty)?;
        self.visit_term(&theorem.body)?;
        Ok(())
    }

    fn check_proc(&mut self, proc: &ItemProc<PhaseElaborated>) -> Result<(), TypingError> {
        let proc_ty = self.visit_term(&proc.ty)?;
        self.bind(proc.ext.clone(), proc_ty)?;

        let params = collect_proc_parameters(proc.ty.as_ref());
        self.env.enter_scope();
        for param in params {
            let hole = Term::Hole(self.fresh_hole());
            self.bind(param, hole)?;
        }
        self.visit_proc_block(&proc.proc_block)?;
        self.env.leave_scope();
        Ok(())
    }

    fn check_type(&mut self, type_item: &ItemType<PhaseElaborated>) -> Result<(), TypingError> {
        let mut fields = Vec::new();
        if let Some(constructor) = type_item.constructors.first() {
            for field in &constructor.fields {
                let field_ty = self.visit_term(&field.ty)?;
                fields.push(StructFieldType {
                    name: field.name.s().to_string(),
                    ty: field_ty,
                });
            }
        }
        let struct_ty = Term::Struct(fields);
        self.bind(type_item.ext.name_id.clone(), struct_ty)?;
        Ok(())
    }

    fn visit_proc_block(
        &mut self,
        block: &ItemProcBlock<PhaseElaborated>,
    ) -> Result<(), TypingError> {
        self.visit_statements(&block.statements)
    }

    fn visit_statements(
        &mut self,
        statements: &Statements<PhaseElaborated>,
    ) -> Result<(), TypingError> {
        match statements {
            Statements::Nil => Ok(()),
            Statements::Statement(stmt) => self.visit_statement(stmt),
            Statements::Then(then) => {
                self.visit_statement(&then.head)?;
                self.visit_statements(&then.tail)
            }
        }
    }

    fn visit_statement(
        &mut self,
        statement: &Statement<PhaseElaborated>,
    ) -> Result<(), TypingError> {
        match statement {
            Statement::Let(stmt) => {
                let value_ty = self.visit_proc_term(&stmt.value)?;
                self.bind(stmt.ext.clone(), value_ty)?;
                Ok(())
            }
            Statement::LetMut(stmt) => {
                let value_ty = self.visit_proc_term(&stmt.value)?;
                self.bind(stmt.ext.value_id.clone(), value_ty.clone())?;
                let ref_ty = Term::Hole(self.fresh_hole());
                self.bind(stmt.ext.reference_id.clone(), ref_ty)?;
                Ok(())
            }
            Statement::Assign(stmt) => {
                let value_ty = self.visit_proc_term(&stmt.value)?;
                let bound =
                    self.env
                        .get(&stmt.ext)
                        .cloned()
                        .ok_or_else(|| TypingError::UnboundName {
                            name_id: stmt.ext.clone(),
                        })?;
                let mut ctx = UnificationCtx::new(&mut self.solutions);
                ctx.unify(&bound, &value_ty)
                    .map_err(TypingError::UnificationFailed)
            }
            Statement::FieldAssign(stmt) => {
                self.visit_proc_method_chain(&stmt.method_chain)?;
                self.visit_proc_term(&stmt.value)?;
                Ok(())
            }
            Statement::Loop(stmt) => {
                self.env.enter_scope();
                self.visit_statements(&stmt.body)?;
                self.env.leave_scope();
                Ok(())
            }
            Statement::Break(_) => Ok(()),
            Statement::Return(stmt) => {
                self.visit_proc_term(&stmt.value)?;
                Ok(())
            }
            Statement::Expr(term) => {
                self.visit_proc_term(term)?;
                Ok(())
            }
            Statement::Ext(_) => Err(TypingError::Unsupported(
                "Statement::Ext is not supported in typing".to_string(),
            )),
        }
    }

    fn visit_proc_method_chain(
        &mut self,
        chain: &ProcTermMethodChain<PhaseElaborated>,
    ) -> Result<Term, TypingError> {
        // Ensure the receiver is in scope even if we don't yet model its fields/methods precisely.
        self.env
            .get(&chain.ext.object_id)
            .cloned()
            .ok_or_else(|| TypingError::UnboundName {
                name_id: chain.ext.object_id.clone(),
            })?;

        if let Some(index) = &chain.index {
            let idx_ty = self.visit_proc_term(index)?;
            let mut ctx = UnificationCtx::new(&mut self.solutions);
            ctx.unify(&Term::Integer(IntegerType::U64), &idx_ty)
                .map_err(TypingError::UnificationFailed)?;
        }

        // Basic shaping for common array helpers; otherwise fall back to a fresh hole.
        let result_ty = match chain.field.s() {
            "#len" => Term::Integer(IntegerType::U64),
            _ => Term::Hole(self.fresh_hole()),
        };

        let assigned = self.assign_type(&chain.ext.term_id, result_ty)?;
        Ok(assigned)
    }

    fn visit_proc_term(&mut self, term: &ProcTerm<PhaseElaborated>) -> Result<Term, TypingError> {
        match term {
            ProcTerm::Unit(unit) => self.assign_type(&unit.ext.term_id, Term::Unit),
            ProcTerm::Number(num) => {
                self.assign_type(&num.ext.term_id, Term::from_number_literal(num.number.s()))
            }
            ProcTerm::Variable(var) => {
                let ty = self.env.get(&var.ext.name_id).cloned().ok_or_else(|| {
                    TypingError::UnboundName {
                        name_id: var.ext.name_id.clone(),
                    }
                })?;
                self.assign_type(&var.ext.term_id, ty)
            }
            ProcTerm::Apply(apply) => {
                let f_ty = self.visit_proc_term(&apply.f)?;
                let arg_types = apply
                    .args
                    .iter()
                    .map(|arg| self.visit_proc_term(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut result_ty = self.type_for_term_id(&apply.ext.term_id);
                let mut callee_ty = f_ty;
                for arg_ty in arg_types {
                    let param_ty = Term::Hole(self.fresh_hole());
                    let ret_ty = Term::Hole(self.fresh_hole());
                    {
                        let mut ctx = UnificationCtx::new(&mut self.solutions);
                        let arrow = Term::Arrow {
                            param: Box::new(param_ty.clone()),
                            param_name: None,
                            result: Box::new(ret_ty.clone()),
                        };
                        ctx.unify(&callee_ty, &arrow)
                            .map_err(TypingError::UnificationFailed)?;
                        ctx.unify(&param_ty, &arg_ty)
                            .map_err(TypingError::UnificationFailed)?;
                    }
                    callee_ty = ret_ty.clone();
                    result_ty = ret_ty;
                }
                self.assign_type(&apply.ext.term_id, result_ty)
            }
            ProcTerm::FieldAccess(field_access) => {
                let object_ty = self
                    .env
                    .get(&field_access.ext.object_id)
                    .cloned()
                    .ok_or_else(|| TypingError::UnboundName {
                        name_id: field_access.ext.object_id.clone(),
                    })?;
                if let Some(index) = &field_access.index {
                    self.visit_proc_term(index)?;
                }
                let result_ty = match self.solutions.resolve(&object_ty) {
                    Term::Struct(fields) => fields
                        .iter()
                        .find(|field| field.name == field_access.field.s())
                        .map(|field| field.ty.clone())
                        .unwrap_or_else(|| Term::Hole(self.fresh_hole())),
                    _ => Term::Hole(self.fresh_hole()),
                };
                self.assign_type(&field_access.ext.term_id, result_ty)
            }
            ProcTerm::MethodChain(chain) => self.visit_proc_method_chain(chain),
            ProcTerm::StructValue(struct_value) => {
                let mut fields = Vec::with_capacity(struct_value.fields.len());
                for field in &struct_value.fields {
                    let ty = self.visit_proc_term(&field.value)?;
                    fields.push(StructFieldType {
                        name: field.name.s().to_string(),
                        ty,
                    });
                }
                let ty = Term::Struct(fields);
                self.assign_type(&struct_value.ext.term_id, ty)
            }
            ProcTerm::If(if_term) => {
                self.env.enter_scope();
                self.visit_statements(&if_term.condition)?;
                self.env.leave_scope();

                self.env.enter_scope();
                self.visit_statements(&if_term.then_body)?;
                self.env.leave_scope();

                if let Some(else_clause) = &if_term.else_clause {
                    self.env.enter_scope();
                    self.visit_statements(&else_clause.else_body)?;
                    self.env.leave_scope();
                }

                let hole = Term::Hole(self.fresh_hole());
                self.assign_type(&if_term.ext.term_id, hole)
            }
            ProcTerm::Match(match_term) => {
                let scrutinee_ty = self
                    .env
                    .get(&match_term.ext.scrutinee_id)
                    .cloned()
                    .ok_or_else(|| TypingError::UnboundName {
                        name_id: match_term.ext.scrutinee_id.clone(),
                    })?;
                let resolved = self.solutions.resolve(&scrutinee_ty);
                let fields = if let Term::Struct(fields) = resolved {
                    fields
                } else {
                    Vec::new()
                };
                for branch in &match_term.branches {
                    self.env.enter_scope();
                    for field in &branch.pattern.fields {
                        let field_ty = fields
                            .iter()
                            .find(|f| f.name == field.field_name.s())
                            .map(|f| f.ty.clone())
                            .unwrap_or_else(|| Term::Hole(self.fresh_hole()));
                        self.bind(field.ext.binder_id.clone(), field_ty)?;
                    }
                    self.visit_statements(&branch.body)?;
                    self.env.leave_scope();
                }
                let hole = Term::Hole(self.fresh_hole());
                self.assign_type(&match_term.ext.term_id, hole)
            }
            ProcTerm::Paren(paren) => {
                let inner_ty = self.visit_proc_term(&paren.proc_term)?;
                self.assign_type(&paren.ext.term_id, inner_ty)
            }
            ProcTerm::Ext(_) => Err(TypingError::Unsupported(
                "ProcTerm::Ext is not supported in typing".to_string(),
            )),
        }
    }

    fn visit_term(&mut self, term: &SynTerm<PhaseElaborated>) -> Result<Term, TypingError> {
        match term {
            SynTerm::Unit(unit) => self.assign_type(&unit.ext.term_id, Term::Unit),
            SynTerm::Number(num) => {
                self.assign_type(&num.ext.term_id, Term::from_number_literal(num.number.s()))
            }
            SynTerm::Variable(var) => {
                let ty = self.env.get(&var.ext.name_id).cloned().ok_or_else(|| {
                    TypingError::UnboundName {
                        name_id: var.ext.name_id.clone(),
                    }
                })?;
                self.assign_type(&var.ext.term_id, ty)
            }
            SynTerm::Paren(paren) => {
                let inner_ty = self.visit_term(&paren.term)?;
                self.assign_type(&paren.ext.term_id, inner_ty)
            }
            SynTerm::ArrowDep(arrow) => {
                let param_type = self.visit_term(&arrow.from_ty)?;
                self.assign_type(&arrow.from.ext.term_id, param_type.clone())?;

                self.env.enter_scope();
                self.bind(arrow.ext.param_id.clone(), param_type.clone())?;
                let result_type = self.visit_term(&arrow.to)?;
                self.env.leave_scope();

                let ty = Term::Arrow {
                    param: Box::new(param_type),
                    param_name: Some(arrow.ext.param_id.clone()),
                    result: Box::new(result_type),
                };
                self.assign_type(&arrow.ext.term_id, ty.clone())?;
                Ok(ty)
            }
            SynTerm::ArrowNodep(arrow) => {
                let param_type = self.visit_term(&arrow.from)?;
                let result_type = self.visit_term(&arrow.to)?;
                let ty = Term::arrow(param_type, result_type);
                self.assign_type(&arrow.ext.term_id, ty.clone())?;
                Ok(ty)
            }
            SynTerm::Apply(apply) => {
                let f_ty = self.visit_term(&apply.f)?;
                let arg_types = apply
                    .args
                    .iter()
                    .map(|arg| self.visit_term(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut result_ty = self.type_for_term_id(&apply.ext.term_id);
                let mut callee_ty = f_ty;
                for arg_ty in arg_types {
                    let param_ty = Term::Hole(self.fresh_hole());
                    let ret_ty = Term::Hole(self.fresh_hole());
                    {
                        let mut ctx = UnificationCtx::new(&mut self.solutions);
                        let arrow = Term::Arrow {
                            param: Box::new(param_ty.clone()),
                            param_name: None,
                            result: Box::new(ret_ty.clone()),
                        };
                        ctx.unify(&callee_ty, &arrow)
                            .map_err(TypingError::UnificationFailed)?;
                        ctx.unify(&param_ty, &arg_ty)
                            .map_err(TypingError::UnificationFailed)?;
                    }
                    callee_ty = ret_ty.clone();
                    result_ty = ret_ty;
                }
                self.assign_type(&apply.ext.term_id, result_ty)
            }
            SynTerm::Match(match_term) => {
                if self.env.get(&match_term.ext.scrutinee_id).is_none() {
                    return Err(TypingError::UnboundName {
                        name_id: match_term.ext.scrutinee_id.clone(),
                    });
                }
                let mut branch_ty: Option<Term> = None;
                for branch in &match_term.branches {
                    let body_ty = self.visit_term(&branch.body)?;
                    if let Some(existing) = &branch_ty {
                        let mut ctx = UnificationCtx::new(&mut self.solutions);
                        ctx.unify(existing, &body_ty)
                            .map_err(TypingError::UnificationFailed)?;
                    } else {
                        branch_ty = Some(body_ty);
                    }
                }
                let ty = match branch_ty {
                    Some(ty) => ty,
                    None => Term::Hole(self.fresh_hole()),
                };
                self.assign_type(&match_term.ext.term_id, ty)
            }
            SynTerm::Struct(term_struct) => {
                let mut fields = Vec::with_capacity(term_struct.fields.len());
                for field in &term_struct.fields {
                    let field_ty = self.visit_term(&field.ty)?;
                    fields.push(StructFieldType {
                        name: field.name.s().to_string(),
                        ty: field_ty,
                    });
                }
                let ty = Term::Struct(fields);
                self.assign_type(&term_struct.ext.term_id, ty)
            }
        }
    }
}

fn collect_proc_parameters(term: &SynTerm<PhaseElaborated>) -> Vec<NameId> {
    fn collect(term: &SynTerm<PhaseElaborated>, out: &mut Vec<NameId>) {
        match term {
            SynTerm::ArrowDep(arrow) => {
                out.push(arrow.ext.param_id.clone());
                collect(&arrow.to, out);
            }
            SynTerm::ArrowNodep(arrow) => collect(&arrow.to, out),
            _ => {}
        }
    }

    let mut params = Vec::new();
    collect(term, &mut params);
    params
}

fn collect_term_parameter_types(term: &SynTerm<PhaseElaborated>) -> Vec<(NameId, TermId)> {
    fn collect(term: &SynTerm<PhaseElaborated>, out: &mut Vec<(NameId, TermId)>) {
        match term {
            SynTerm::ArrowDep(arrow) => {
                out.push((arrow.ext.param_id.clone(), arrow.from.ext.term_id.clone()));
                collect(&arrow.to, out);
            }
            SynTerm::ArrowNodep(arrow) => collect(&arrow.to, out),
            _ => {}
        }
    }

    let mut params = Vec::new();
    collect(term, &mut params);
    params
}
