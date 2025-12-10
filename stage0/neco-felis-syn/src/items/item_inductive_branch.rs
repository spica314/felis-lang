use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{TokenColon, TokenComma, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemInductiveBranch<P: Phase> {
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub ty: Box<Term<P>>,
    pub comma: TokenComma,
    pub ext: P::ItemInductiveBranchExt,
}

impl<P: Phase> ItemInductiveBranch<P> {
    pub fn name(&self) -> &TokenVariable {
        &self.name
    }

    pub fn ty(&self) -> &Term<P> {
        &self.ty
    }
}

impl Parse for ItemInductiveBranch<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_branch_1"));
        };

        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_branch_2"));
        };

        let Some(comma) = TokenComma::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_branch_3"));
        };

        let item_inductive_branch = ItemInductiveBranch {
            name,
            colon,
            ty: Box::new(ty),
            comma,
            ext: (),
        };

        *i = k;
        Ok(Some(item_inductive_branch))
    }
}
