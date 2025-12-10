use crate::{
    Parse, ParseError, Phase, PhaseParse, Term, TermNumber, TermParen, TermUnit, TermVariable,
    token::{Token, TokenOperator},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermArrowNodep<P: Phase> {
    pub from: Box<Term<P>>,
    pub arrow: TokenOperator,
    pub to: Box<Term<P>>,
    pub ext: P::TermArrowNodepExt,
}

impl<P: Phase> TermArrowNodep<P> {
    pub fn from(&self) -> &Term<P> {
        &self.from
    }

    pub fn to(&self) -> &Term<P> {
        &self.to
    }
}

impl Parse for TermArrowNodep<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(from) = TermForArrowNodepFrom::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(arrow) = TokenOperator::parse_operator(tokens, &mut k, "->")? else {
            // ?
            return Ok(None);
        };

        let Some(to) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("term_arrow_no_dep_1"));
        };

        let term_arrow_nodep = TermArrowNodep {
            from: Box::new(from.into()),
            arrow,
            to: Box::new(to),
            ext: (),
        };

        *i = k;
        Ok(Some(term_arrow_nodep))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TermForArrowNodepFrom {
    Paren(TermParen<PhaseParse>),
    Variable(TermVariable<PhaseParse>),
    Unit(TermUnit<PhaseParse>),
    Number(TermNumber<PhaseParse>),
}

impl From<TermForArrowNodepFrom> for Term<PhaseParse> {
    fn from(value: TermForArrowNodepFrom) -> Self {
        match value {
            TermForArrowNodepFrom::Paren(term_paren) => Term::Paren(term_paren),
            TermForArrowNodepFrom::Variable(term_variable) => Term::Variable(term_variable),
            TermForArrowNodepFrom::Unit(term_unit) => Term::Unit(term_unit),
            TermForArrowNodepFrom::Number(term_number) => Term::Number(term_number),
        }
    }
}

impl Parse for TermForArrowNodepFrom {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(term_unit) = TermUnit::parse(tokens, i)? {
            return Ok(Some(TermForArrowNodepFrom::Unit(term_unit)));
        }

        if let Some(term_paren) = TermParen::parse(tokens, i)? {
            return Ok(Some(TermForArrowNodepFrom::Paren(term_paren)));
        }

        if let Some(term_variable) = TermVariable::parse(tokens, i)? {
            return Ok(Some(TermForArrowNodepFrom::Variable(term_variable)));
        }

        if let Some(term_number) = TermNumber::parse(tokens, i)? {
            return Ok(Some(TermForArrowNodepFrom::Number(term_number)));
        }

        Ok(None)
    }
}
