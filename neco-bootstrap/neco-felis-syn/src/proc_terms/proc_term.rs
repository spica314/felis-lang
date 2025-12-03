use crate::{
    ItemStruct, Parse, ParseError, Phase, PhaseParse, ProcTermApply, ProcTermConstructorCall,
    ProcTermFieldAccess, ProcTermIf, ProcTermMethodChain, ProcTermNumber, ProcTermParen,
    ProcTermStructValue, ProcTermUnit, ProcTermVariable, token::Token,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ProcTerm<P: Phase> {
    Paren(ProcTermParen<P>),
    Apply(ProcTermApply<P>),
    Variable(ProcTermVariable<P>),
    Unit(ProcTermUnit<P>),
    Number(ProcTermNumber<P>),
    FieldAccess(ProcTermFieldAccess<P>),
    MethodChain(ProcTermMethodChain<P>),
    ConstructorCall(ProcTermConstructorCall<P>),
    Struct(ItemStruct<P>),
    StructValue(ProcTermStructValue<P>),
    If(ProcTermIf<P>),
    Ext(P::ProcTermExt),
}

impl Parse for ProcTerm<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(proc_term_if) = ProcTermIf::parse(tokens, i)? {
            return Ok(Some(ProcTerm::If(proc_term_if)));
        }

        if let Some(proc_term_constructor_call) = ProcTermConstructorCall::parse(tokens, i)? {
            return Ok(Some(ProcTerm::ConstructorCall(proc_term_constructor_call)));
        }

        if let Some(proc_term_apply) = ProcTermApply::parse(tokens, i)? {
            return Ok(Some(ProcTerm::Apply(proc_term_apply)));
        }

        // Try field access first (no whitespace before dot)
        if let Some(proc_term_field_access) = ProcTermFieldAccess::parse(tokens, i)? {
            return Ok(Some(ProcTerm::FieldAccess(proc_term_field_access)));
        }

        // Then try method chain (whitespace before dot)
        if let Some(proc_term_method_chain) = ProcTermMethodChain::parse(tokens, i)? {
            return Ok(Some(ProcTerm::MethodChain(proc_term_method_chain)));
        }

        if let Some(proc_term_struct_value) = ProcTermStructValue::parse(tokens, i)? {
            return Ok(Some(ProcTerm::StructValue(proc_term_struct_value)));
        }

        if let Some(item_struct) = ItemStruct::parse(tokens, i)? {
            return Ok(Some(ProcTerm::Struct(item_struct)));
        }

        if let Some(proc_term_variable) = ProcTermVariable::parse(tokens, i)? {
            return Ok(Some(ProcTerm::Variable(proc_term_variable)));
        }

        if let Some(proc_term_number) = ProcTermNumber::parse(tokens, i)? {
            return Ok(Some(ProcTerm::Number(proc_term_number)));
        }

        if let Some(proc_term_unit) = ProcTermUnit::parse(tokens, i)? {
            return Ok(Some(ProcTerm::Unit(proc_term_unit)));
        }

        if let Some(proc_term_paren) = ProcTermParen::parse(tokens, i)? {
            return Ok(Some(ProcTerm::Paren(proc_term_paren)));
        }
        Ok(None)
    }
}
