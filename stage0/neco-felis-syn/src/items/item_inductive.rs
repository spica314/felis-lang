use crate::{
    ItemInductiveBranch, Parse, ParseError, Phase, PhaseParse, Term,
    token::{TokenBraceL, TokenBraceR, TokenColon, TokenKeyword, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemInductive<P: Phase> {
    pub keyword_inductive: TokenKeyword,
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub ty: Box<Term<P>>,
    pub brace_l: TokenBraceL,
    pub branches: Vec<ItemInductiveBranch<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemInductiveExt,
}

impl<P: Phase> ItemInductive<P> {
    pub fn name(&self) -> &TokenVariable {
        &self.name
    }

    pub fn ty(&self) -> &Term<P> {
        &self.ty
    }

    pub fn branches(&self) -> &[ItemInductiveBranch<P>] {
        &self.branches
    }
}

impl Parse for ItemInductive<PhaseParse> {
    fn parse(
        tokens: &[crate::token::Token],
        i: &mut usize,
    ) -> Result<Option<Self>, crate::ParseError> {
        let mut k = *i;

        let Some(keyword_inductive) = TokenKeyword::parse_keyword(tokens, &mut k, "inductive")?
        else {
            return Ok(None);
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_1"));
        };

        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_2"));
        };

        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_3"));
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_4"));
        };

        let mut branches = vec![];
        while let Some(branch) = ItemInductiveBranch::parse(tokens, &mut k)? {
            branches.push(branch);
        }

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("item_inductive_5"));
        };

        let item_inductive = ItemInductive {
            keyword_inductive,
            name,
            colon,
            ty: Box::new(ty),
            brace_l,
            branches,
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(item_inductive))
    }
}
