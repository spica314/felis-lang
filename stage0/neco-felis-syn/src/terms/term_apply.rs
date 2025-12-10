use crate::{
    Parse, ParseError, Phase, PhaseParse, Term, TermNumber, TermParen, TermUnit, TermVariable,
    token::Token,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermApply<P: Phase> {
    pub f: Box<Term<P>>,
    pub args: Vec<Term<P>>,
    pub ext: P::TermApplyExt,
}

impl<P: Phase> TermApply<P> {
    pub fn f(&self) -> &Term<P> {
        &self.f
    }

    pub fn args(&self) -> &[Term<P>] {
        &self.args
    }
}

impl Parse for TermApply<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(f) = TermForApplyElem::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let mut args = vec![];
        while let Some(arg) = TermForApplyElem::parse(tokens, &mut k)? {
            args.push(arg.into());
        }

        if args.is_empty() {
            return Ok(None);
        }

        let term_apply = TermApply {
            f: Box::new(f.into()),
            args,
            ext: (),
        };
        *i = k;
        Ok(Some(term_apply))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TermForApplyElem {
    Paren(TermParen<PhaseParse>),
    Variable(TermVariable<PhaseParse>),
    Unit(TermUnit<PhaseParse>),
    Number(TermNumber<PhaseParse>),
}

impl From<TermForApplyElem> for Term<PhaseParse> {
    fn from(value: TermForApplyElem) -> Self {
        match value {
            TermForApplyElem::Paren(term_paren) => Term::Paren(term_paren),
            TermForApplyElem::Variable(term_variable) => Term::Variable(term_variable),
            TermForApplyElem::Unit(term_unit) => Term::Unit(term_unit),
            TermForApplyElem::Number(term_number) => Term::Number(term_number),
        }
    }
}

impl Parse for TermForApplyElem {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(term_unit) = TermUnit::parse(tokens, i)? {
            return Ok(Some(TermForApplyElem::Unit(term_unit)));
        }

        if let Some(term_paren) = TermParen::parse(tokens, i)? {
            return Ok(Some(TermForApplyElem::Paren(term_paren)));
        }

        if let Some(term_variable) = TermVariable::parse(tokens, i)? {
            return Ok(Some(TermForApplyElem::Variable(term_variable)));
        }

        if let Some(term_number) = TermNumber::parse(tokens, i)? {
            return Ok(Some(TermForApplyElem::Number(term_number)));
        }

        Ok(None)
    }
}
