use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm, StatementAssign, StatementBreak,
    StatementCallPtx, StatementLet, StatementLetMut, StatementLoop, StatementMethodChainAssign,
    StatementReturn, token::Token,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement<P: Phase> {
    Let(StatementLet<P>),
    LetMut(StatementLetMut<P>),
    Assign(StatementAssign<P>),
    FieldAssign(StatementMethodChainAssign<P>),
    Loop(StatementLoop<P>),
    Break(StatementBreak<P>),
    Return(StatementReturn<P>),
    CallPtx(StatementCallPtx<P>),
    Expr(ProcTerm<P>),
    Ext(P::StatementExt),
}

impl Parse for Statement<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(statement_let_mut) = StatementLetMut::parse(tokens, i)? {
            return Ok(Some(Statement::LetMut(statement_let_mut)));
        }

        if let Some(statement_let) = StatementLet::parse(tokens, i)? {
            return Ok(Some(Statement::Let(statement_let)));
        }

        if let Some(statement_field_assign) = StatementMethodChainAssign::parse(tokens, i)? {
            return Ok(Some(Statement::FieldAssign(statement_field_assign)));
        }

        if let Some(statement_assign) = StatementAssign::parse(tokens, i)? {
            return Ok(Some(Statement::Assign(statement_assign)));
        }

        if let Some(statement_loop) = StatementLoop::parse(tokens, i)? {
            return Ok(Some(Statement::Loop(statement_loop)));
        }

        if let Some(statement_break) = StatementBreak::parse(tokens, i)? {
            return Ok(Some(Statement::Break(statement_break)));
        }

        if let Some(statement_return) = StatementReturn::parse(tokens, i)? {
            return Ok(Some(Statement::Return(statement_return)));
        }

        if let Some(statement_call_ptx) = StatementCallPtx::parse(tokens, i)? {
            return Ok(Some(Statement::CallPtx(statement_call_ptx)));
        }

        if let Some(proc_term) = ProcTerm::parse(tokens, i)? {
            return Ok(Some(Statement::Expr(proc_term)));
        }

        Ok(None)
    }
}
