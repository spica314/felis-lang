use crate::{
    Parse, ParseError, Phase, PhaseParse, ProcTerm, ProcTermFieldAccess, ProcTermMethodChain,
    ProcTermNumber, ProcTermParen, ProcTermUnit, ProcTermVariable, token::Token,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermApply<P: Phase> {
    pub f: Box<ProcTerm<P>>,
    pub args: Vec<ProcTerm<P>>,
    pub ext: P::ProcTermApplyExt,
}

impl<P: Phase> ProcTermApply<P> {
    pub fn f(&self) -> &ProcTerm<P> {
        &self.f
    }

    pub fn args(&self) -> &[ProcTerm<P>] {
        &self.args
    }
}

impl Parse for ProcTermApply<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(f) = ProcTermForApplyElem::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let mut args = vec![];
        while let Some(arg) = ProcTermForApplyElem::parse(tokens, &mut k)? {
            args.push(arg.into());
        }

        if args.is_empty() {
            return Ok(None);
        }

        let proc_term_apply = ProcTermApply {
            f: Box::new(f.into()),
            args,
            ext: (),
        };
        *i = k;
        Ok(Some(proc_term_apply))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ProcTermForApplyElem {
    Paren(ProcTermParen<PhaseParse>),
    Variable(ProcTermVariable<PhaseParse>),
    Unit(ProcTermUnit<PhaseParse>),
    Number(ProcTermNumber<PhaseParse>),
    FieldAccess(ProcTermFieldAccess<PhaseParse>),
    MethodChain(ProcTermMethodChain<PhaseParse>),
}

impl From<ProcTermForApplyElem> for ProcTerm<PhaseParse> {
    fn from(value: ProcTermForApplyElem) -> Self {
        match value {
            ProcTermForApplyElem::Paren(proc_term_paren) => ProcTerm::Paren(proc_term_paren),
            ProcTermForApplyElem::Variable(proc_term_variable) => {
                ProcTerm::Variable(proc_term_variable)
            }
            ProcTermForApplyElem::Unit(proc_term_unit) => ProcTerm::Unit(proc_term_unit),
            ProcTermForApplyElem::Number(proc_term_number) => ProcTerm::Number(proc_term_number),
            ProcTermForApplyElem::FieldAccess(proc_term_field_access) => {
                ProcTerm::FieldAccess(proc_term_field_access)
            }
            ProcTermForApplyElem::MethodChain(proc_term_method_chain) => {
                ProcTerm::MethodChain(proc_term_method_chain)
            }
        }
    }
}

impl Parse for ProcTermForApplyElem {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(proc_term_unit) = ProcTermUnit::parse(tokens, i)? {
            return Ok(Some(ProcTermForApplyElem::Unit(proc_term_unit)));
        }

        if let Some(proc_term_paren) = ProcTermParen::parse(tokens, i)? {
            return Ok(Some(ProcTermForApplyElem::Paren(proc_term_paren)));
        }

        // Try field access first (no whitespace before .)
        if let Some(proc_term_field_access) = ProcTermFieldAccess::parse(tokens, i)? {
            return Ok(Some(ProcTermForApplyElem::FieldAccess(
                proc_term_field_access,
            )));
        }

        // Then try method chain (whitespace before .)
        if let Some(proc_term_method_chain) = ProcTermMethodChain::parse(tokens, i)? {
            return Ok(Some(ProcTermForApplyElem::MethodChain(
                proc_term_method_chain,
            )));
        }

        if let Some(proc_term_variable) = ProcTermVariable::parse(tokens, i)? {
            return Ok(Some(ProcTermForApplyElem::Variable(proc_term_variable)));
        }

        if let Some(proc_term_number) = ProcTermNumber::parse(tokens, i)? {
            return Ok(Some(ProcTermForApplyElem::Number(proc_term_number)));
        }

        Ok(None)
    }
}
