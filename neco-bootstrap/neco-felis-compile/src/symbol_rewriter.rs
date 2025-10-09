use crate::error::CompileError;
use neco_felis_rename::{PhaseRenamed, StatementCallPtxIds, StatementLetMutIds, VariableId};
use neco_felis_syn::*;
use std::collections::HashSet;

pub fn apply_symbol_ids(
    original: &mut File<PhaseParse>,
    renamed: &File<PhaseRenamed>,
) -> Result<(), CompileError> {
    if original.items.len() != renamed.items.len() {
        return Err(CompileError::UnsupportedConstruct(
            "mismatched item count during renaming".to_string(),
        ));
    }

    let preserve_ids = collect_preserve_ids(renamed);
    let ctx = RewriteContext {
        preserve: &preserve_ids,
    };

    for (orig_item, ren_item) in original.items.iter_mut().zip(renamed.items.iter()) {
        update_item(&ctx, orig_item, ren_item)?;
    }

    Ok(())
}

struct RewriteContext<'a> {
    preserve: &'a HashSet<VariableId>,
}

impl<'a> RewriteContext<'a> {
    fn rename_token(&self, token: &mut TokenVariable, id: &VariableId) {
        if id.1 == usize::MAX || self.preserve.contains(id) {
            return;
        }
        let new_name = format!("{}#{}", token.s(), id.1);
        *token = TokenVariable::new(token.pos().clone(), new_name);
    }
}

fn collect_preserve_ids(file: &File<PhaseRenamed>) -> HashSet<VariableId> {
    let mut set = HashSet::new();
    for item in &file.items {
        match item {
            Item::UseBuiltin(alias) => {
                set.insert(alias.ext.clone());
            }
            Item::Proc(proc) => {
                collect_statements_preserve_ids(&proc.proc_block.statements, &mut set);
            }
            _ => {}
        }
    }
    set
}

fn collect_statements_preserve_ids(
    statements: &Statements<PhaseRenamed>,
    set: &mut HashSet<VariableId>,
) {
    match statements {
        Statements::Then(then) => {
            collect_statement_preserve_ids(&then.head, set);
            collect_statements_preserve_ids(&then.tail, set);
        }
        Statements::Statement(stmt) => collect_statement_preserve_ids(stmt, set),
        Statements::Nil => {}
    }
}

fn collect_statement_preserve_ids(
    statement: &Statement<PhaseRenamed>,
    set: &mut HashSet<VariableId>,
) {
    if let Statement::CallPtx(call) = statement {
        set.insert(call.ext.function_id.clone());
    }
}

fn update_item(
    ctx: &RewriteContext,
    original: &mut Item<PhaseParse>,
    renamed: &Item<PhaseRenamed>,
) -> Result<(), CompileError> {
    match (original, renamed) {
        (Item::Definition(orig), Item::Definition(ren)) => {
            update_term(ctx, orig.type_.as_mut(), ren.type_.as_ref())?;
            update_term(ctx, orig.body.as_mut(), ren.body.as_ref())
        }
        (Item::Inductive(orig), Item::Inductive(ren)) => {
            update_term(ctx, orig.ty.as_mut(), ren.ty.as_ref())?;
            for (orig_branch, ren_branch) in orig.branches.iter_mut().zip(&ren.branches) {
                update_term(ctx, orig_branch.ty.as_mut(), ren_branch.ty.as_ref())?;
            }
            Ok(())
        }
        (Item::Theorem(orig), Item::Theorem(ren)) => {
            update_term(ctx, orig.type_.as_mut(), ren.type_.as_ref())?;
            update_term(ctx, orig.body.as_mut(), ren.body.as_ref())
        }
        (Item::Proc(orig), Item::Proc(ren)) => {
            update_term(ctx, orig.ty.as_mut(), ren.ty.as_ref())?;
            update_proc_block(ctx, &mut orig.proc_block, &ren.proc_block)
        }
        (Item::Struct(orig), Item::Struct(ren)) => {
            for (orig_field, ren_field) in orig.fields.iter_mut().zip(&ren.fields) {
                update_term(ctx, orig_field.ty.as_mut(), ren_field.ty.as_ref())?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn update_proc_block(
    ctx: &RewriteContext,
    original: &mut ItemProcBlock<PhaseParse>,
    renamed: &ItemProcBlock<PhaseRenamed>,
) -> Result<(), CompileError> {
    update_statements(ctx, &mut original.statements, &renamed.statements)
}

fn update_statements(
    ctx: &RewriteContext,
    original: &mut Statements<PhaseParse>,
    renamed: &Statements<PhaseRenamed>,
) -> Result<(), CompileError> {
    match (original, renamed) {
        (Statements::Nil, Statements::Nil) => Ok(()),
        (Statements::Statement(orig_stmt), Statements::Statement(ren_stmt)) => {
            update_statement(ctx, orig_stmt, ren_stmt)
        }
        (Statements::Then(orig_then), Statements::Then(ren_then)) => {
            update_statement(ctx, &mut orig_then.head, &ren_then.head)?;
            update_statements(ctx, &mut orig_then.tail, &ren_then.tail)
        }
        _ => Err(CompileError::UnsupportedConstruct(
            "statement shape mismatch during renaming".to_string(),
        )),
    }
}

fn update_statement(
    ctx: &RewriteContext,
    original: &mut Statement<PhaseParse>,
    renamed: &Statement<PhaseRenamed>,
) -> Result<(), CompileError> {
    match (original, renamed) {
        (Statement::Let(orig), Statement::Let(ren)) => {
            ctx.rename_token(&mut orig.variable, &ren.ext);
            update_proc_term(ctx, orig.value.as_mut(), ren.value.as_ref())
        }
        (Statement::LetMut(orig), Statement::LetMut(ren)) => {
            let StatementLetMutIds {
                value_id,
                reference_id,
            } = &ren.ext;
            ctx.rename_token(&mut orig.variable, value_id);
            ctx.rename_token(&mut orig.reference_variable, reference_id);
            update_proc_term(ctx, orig.value.as_mut(), ren.value.as_ref())
        }
        (Statement::Assign(orig), Statement::Assign(ren)) => {
            ctx.rename_token(&mut orig.variable, &ren.ext);
            update_proc_term(ctx, orig.value.as_mut(), ren.value.as_ref())
        }
        (Statement::FieldAssign(orig), Statement::FieldAssign(ren)) => {
            update_method_chain(ctx, &mut orig.method_chain, &ren.method_chain)?;
            update_proc_term(ctx, orig.value.as_mut(), ren.value.as_ref())
        }
        (Statement::Loop(orig), Statement::Loop(ren)) => {
            update_statements(ctx, orig.body.as_mut(), ren.body.as_ref())
        }
        (Statement::Return(orig), Statement::Return(ren)) => {
            update_proc_term(ctx, orig.value.as_mut(), ren.value.as_ref())
        }
        (Statement::CallPtx(orig), Statement::CallPtx(ren)) => {
            let StatementCallPtxIds { arg_id, .. } = &ren.ext;
            ctx.rename_token(&mut orig.arg, arg_id);
            Ok(())
        }
        (Statement::Expr(orig), Statement::Expr(ren)) => update_proc_term(ctx, orig, ren),
        (Statement::Break(_), Statement::Break(_)) => Ok(()),
        (Statement::Ext(_), Statement::Ext(_)) => Ok(()),
        _ => Err(CompileError::UnsupportedConstruct(
            "statement variant mismatch during renaming".to_string(),
        )),
    }
}

fn update_proc_term(
    ctx: &RewriteContext,
    original: &mut ProcTerm<PhaseParse>,
    renamed: &ProcTerm<PhaseRenamed>,
) -> Result<(), CompileError> {
    match (original, renamed) {
        (ProcTerm::Variable(orig), ProcTerm::Variable(ren)) => {
            ctx.rename_token(&mut orig.variable, &ren.ext);
            Ok(())
        }
        (ProcTerm::Apply(orig), ProcTerm::Apply(ren)) => {
            update_proc_term(ctx, orig.f.as_mut(), ren.f.as_ref())?;
            for (orig_arg, ren_arg) in orig.args.iter_mut().zip(&ren.args) {
                update_proc_term(ctx, orig_arg, ren_arg)?;
            }
            Ok(())
        }
        (ProcTerm::FieldAccess(orig), ProcTerm::FieldAccess(ren)) => {
            ctx.rename_token(&mut orig.object, &ren.ext);
            if let (Some(orig_idx), Some(ren_idx)) = (orig.index.as_mut(), ren.index.as_ref()) {
                update_proc_term(ctx, orig_idx, ren_idx)?;
            }
            Ok(())
        }
        (ProcTerm::MethodChain(orig), ProcTerm::MethodChain(ren)) => {
            update_method_chain(ctx, orig, ren)
        }
        (ProcTerm::ConstructorCall(orig), ProcTerm::ConstructorCall(ren)) => {
            for (orig_arg, ren_arg) in orig.args.iter_mut().zip(&ren.args) {
                update_proc_term(ctx, orig_arg, ren_arg)?;
            }
            Ok(())
        }
        (ProcTerm::StructValue(orig), ProcTerm::StructValue(ren)) => {
            for (orig_field, ren_field) in orig.fields.iter_mut().zip(&ren.fields) {
                update_proc_term(ctx, orig_field.value.as_mut(), ren_field.value.as_ref())?;
            }
            Ok(())
        }
        (ProcTerm::Struct(_), ProcTerm::Struct(_)) => Ok(()),
        (ProcTerm::Ext(_), ProcTerm::Ext(_)) => Ok(()),
        (ProcTerm::If(orig), ProcTerm::If(ren)) => {
            update_statements(ctx, orig.condition.as_mut(), ren.condition.as_ref())?;
            update_statements(ctx, orig.then_body.as_mut(), ren.then_body.as_ref())?;
            match (&mut orig.else_clause, &ren.else_clause) {
                (Some(orig_else), Some(ren_else)) => update_statements(
                    ctx,
                    orig_else.else_body.as_mut(),
                    ren_else.else_body.as_ref(),
                ),
                (None, None) => Ok(()),
                _ => Err(CompileError::UnsupportedConstruct(
                    "if-else clause mismatch during renaming".to_string(),
                )),
            }
        }
        (ProcTerm::Dereference(orig), ProcTerm::Dereference(ren)) => {
            update_proc_term(ctx, orig.term.as_mut(), ren.term.as_ref())
        }
        (ProcTerm::Paren(orig), ProcTerm::Paren(ren)) => {
            update_proc_term(ctx, orig.proc_term.as_mut(), ren.proc_term.as_ref())
        }
        (ProcTerm::Number(_), ProcTerm::Number(_)) => Ok(()),
        (ProcTerm::Unit(_), ProcTerm::Unit(_)) => Ok(()),
        _ => Err(CompileError::UnsupportedConstruct(format!(
            "unsupported proc term combination: {renamed:?}"
        ))),
    }
}

fn update_method_chain(
    ctx: &RewriteContext,
    original: &mut ProcTermMethodChain<PhaseParse>,
    renamed: &ProcTermMethodChain<PhaseRenamed>,
) -> Result<(), CompileError> {
    ctx.rename_token(&mut original.object, &renamed.ext);
    if let (Some(orig_idx), Some(ren_idx)) = (original.index.as_mut(), renamed.index.as_ref()) {
        update_proc_term(ctx, orig_idx, ren_idx)?;
    }
    Ok(())
}

fn update_term(
    ctx: &RewriteContext,
    original: &mut Term<PhaseParse>,
    renamed: &Term<PhaseRenamed>,
) -> Result<(), CompileError> {
    match (original, renamed) {
        (Term::Variable(orig), Term::Variable(ren)) => {
            ctx.rename_token(&mut orig.variable, &ren.ext);
            Ok(())
        }
        (Term::ArrowDep(orig), Term::ArrowDep(ren)) => {
            ctx.rename_token(&mut orig.from.variable, &ren.ext);
            update_term(ctx, orig.from_ty.as_mut(), ren.from_ty.as_ref())?;
            update_term(ctx, orig.to.as_mut(), ren.to.as_ref())
        }
        (Term::ArrowNodep(orig), Term::ArrowNodep(ren)) => {
            update_term(ctx, orig.from.as_mut(), ren.from.as_ref())?;
            update_term(ctx, orig.to.as_mut(), ren.to.as_ref())
        }
        (Term::Apply(orig), Term::Apply(ren)) => {
            update_term(ctx, orig.f.as_mut(), ren.f.as_ref())?;
            for (orig_arg, ren_arg) in orig.args.iter_mut().zip(&ren.args) {
                update_term(ctx, orig_arg, ren_arg)?;
            }
            Ok(())
        }
        (Term::Match(orig), Term::Match(ren)) => {
            ctx.rename_token(&mut orig.scrutinee, &ren.ext);
            for (orig_branch, ren_branch) in orig.branches.iter_mut().zip(&ren.branches) {
                update_term(ctx, orig_branch.body.as_mut(), ren_branch.body.as_ref())?;
            }
            Ok(())
        }
        (Term::Paren(orig), Term::Paren(ren)) => {
            update_term(ctx, orig.term.as_mut(), ren.term.as_ref())
        }
        (Term::Unit(_), Term::Unit(_)) => Ok(()),
        (Term::Number(_), Term::Number(_)) => Ok(()),
        (Term::Struct(orig), Term::Struct(ren)) => {
            for (orig_field, ren_field) in orig.fields.iter_mut().zip(&ren.fields) {
                update_term(ctx, orig_field.ty.as_mut(), ren_field.ty.as_ref())?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}
