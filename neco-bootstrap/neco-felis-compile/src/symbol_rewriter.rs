use crate::error::CompileError;
use neco_felis_elaboration::{NameId, PhaseElaborated, StatementLetMutIds};
use neco_felis_syn::*;
use std::collections::HashSet;

pub fn apply_symbol_ids(
    original: &mut File<PhaseParse>,
    elaborated: &File<PhaseElaborated>,
) -> Result<(), CompileError> {
    if original.items.len() != elaborated.items.len() {
        return Err(CompileError::UnsupportedConstruct(
            "mismatched item count during elaboration".to_string(),
        ));
    }

    let preserve_ids = collect_preserve_ids(elaborated);
    let ctx = RewriteContext {
        preserve: &preserve_ids,
    };

    for (orig_item, elab_item) in original.items.iter_mut().zip(elaborated.items.iter()) {
        update_item(&ctx, orig_item, elab_item)?;
    }

    Ok(())
}

struct RewriteContext<'a> {
    preserve: &'a HashSet<NameId>,
}

impl<'a> RewriteContext<'a> {
    fn apply_elaborated_token(&self, token: &mut TokenVariable, id: &NameId) {
        if id.1 == usize::MAX || self.preserve.contains(id) {
            return;
        }
        let new_name = format!("{}#{}", token.s(), id.1);
        *token = TokenVariable::new(token.pos().clone(), new_name);
    }
}

fn collect_preserve_ids(file: &File<PhaseElaborated>) -> HashSet<NameId> {
    let mut set = HashSet::new();
    for item in &file.items {
        match item {
            Item::UseBuiltin(alias) => {
                set.insert(alias.ext.clone());
            }
            Item::Proc(proc) => {
                collect_statements_preserve_ids(&proc.proc_block.statements);
            }
            _ => {}
        }
    }
    set
}

fn collect_statements_preserve_ids(statements: &Statements<PhaseElaborated>) {
    match statements {
        Statements::Then(then) => {
            collect_statements_preserve_ids(&then.tail);
        }
        Statements::Statement(_) => {}
        Statements::Nil => {}
    }
}

fn update_item(
    ctx: &RewriteContext,
    original: &mut Item<PhaseParse>,
    elaborated: &Item<PhaseElaborated>,
) -> Result<(), CompileError> {
    match (original, elaborated) {
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
    elaborated: &ItemProcBlock<PhaseElaborated>,
) -> Result<(), CompileError> {
    update_statements(ctx, &mut original.statements, &elaborated.statements)
}

fn update_statements(
    ctx: &RewriteContext,
    original: &mut Statements<PhaseParse>,
    elaborated: &Statements<PhaseElaborated>,
) -> Result<(), CompileError> {
    match (original, elaborated) {
        (Statements::Nil, Statements::Nil) => Ok(()),
        (Statements::Statement(orig_stmt), Statements::Statement(ren_stmt)) => {
            update_statement(ctx, orig_stmt, ren_stmt)
        }
        (Statements::Then(orig_then), Statements::Then(ren_then)) => {
            update_statement(ctx, &mut orig_then.head, &ren_then.head)?;
            update_statements(ctx, &mut orig_then.tail, &ren_then.tail)
        }
        _ => Err(CompileError::UnsupportedConstruct(
            "statement shape mismatch during elaboration".to_string(),
        )),
    }
}

fn update_statement(
    ctx: &RewriteContext,
    original: &mut Statement<PhaseParse>,
    elaborated: &Statement<PhaseElaborated>,
) -> Result<(), CompileError> {
    match (original, elaborated) {
        (Statement::Let(orig), Statement::Let(ren)) => {
            ctx.apply_elaborated_token(&mut orig.variable, &ren.ext);
            update_proc_term(ctx, orig.value.as_mut(), ren.value.as_ref())
        }
        (Statement::LetMut(orig), Statement::LetMut(ren)) => {
            let StatementLetMutIds {
                value_id,
                reference_id,
            } = &ren.ext;
            ctx.apply_elaborated_token(&mut orig.variable, value_id);
            ctx.apply_elaborated_token(&mut orig.reference_variable, reference_id);
            update_proc_term(ctx, orig.value.as_mut(), ren.value.as_ref())
        }
        (Statement::Assign(orig), Statement::Assign(ren)) => {
            ctx.apply_elaborated_token(&mut orig.variable, &ren.ext);
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
        (Statement::Expr(orig), Statement::Expr(ren)) => update_proc_term(ctx, orig, ren),
        (Statement::Break(_), Statement::Break(_)) => Ok(()),
        (Statement::Ext(_), Statement::Ext(_)) => Ok(()),
        _ => Err(CompileError::UnsupportedConstruct(
            "statement variant mismatch during elaboration".to_string(),
        )),
    }
}

fn update_proc_term(
    ctx: &RewriteContext,
    original: &mut ProcTerm<PhaseParse>,
    elaborated: &ProcTerm<PhaseElaborated>,
) -> Result<(), CompileError> {
    match (original, elaborated) {
        (ProcTerm::Variable(orig), ProcTerm::Variable(ren)) => {
            ctx.apply_elaborated_token(&mut orig.variable, &ren.ext.name_id);
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
            ctx.apply_elaborated_token(&mut orig.object, &ren.ext.object_id);
            if let (Some(orig_idx), Some(ren_idx)) = (orig.index.as_mut(), ren.index.as_ref()) {
                update_proc_term(ctx, orig_idx, ren_idx)?;
            }
            Ok(())
        }
        (ProcTerm::MethodChain(orig), ProcTerm::MethodChain(ren)) => {
            update_method_chain(ctx, orig, ren)
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
                    "if-else clause mismatch during elaboration".to_string(),
                )),
            }
        }
        (ProcTerm::Paren(orig), ProcTerm::Paren(ren)) => {
            update_proc_term(ctx, orig.proc_term.as_mut(), ren.proc_term.as_ref())
        }
        (ProcTerm::Number(_), ProcTerm::Number(_)) => Ok(()),
        (ProcTerm::Unit(_), ProcTerm::Unit(_)) => Ok(()),
        _ => Err(CompileError::UnsupportedConstruct(format!(
            "unsupported proc term combination: {elaborated:?}"
        ))),
    }
}

fn update_method_chain(
    ctx: &RewriteContext,
    original: &mut ProcTermMethodChain<PhaseParse>,
    elaborated: &ProcTermMethodChain<PhaseElaborated>,
) -> Result<(), CompileError> {
    ctx.apply_elaborated_token(&mut original.object, &elaborated.ext.object_id);
    if let (Some(orig_idx), Some(ren_idx)) = (original.index.as_mut(), elaborated.index.as_ref()) {
        update_proc_term(ctx, orig_idx, ren_idx)?;
    }
    Ok(())
}

fn update_term(
    ctx: &RewriteContext,
    original: &mut Term<PhaseParse>,
    elaborated: &Term<PhaseElaborated>,
) -> Result<(), CompileError> {
    match (original, elaborated) {
        (Term::Variable(orig), Term::Variable(ren)) => {
            ctx.apply_elaborated_token(&mut orig.variable, &ren.ext.name_id);
            Ok(())
        }
        (Term::ArrowDep(orig), Term::ArrowDep(ren)) => {
            ctx.apply_elaborated_token(&mut orig.from.variable, &ren.ext.param_id);
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
            ctx.apply_elaborated_token(&mut orig.scrutinee, &ren.ext.scrutinee_id);
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
