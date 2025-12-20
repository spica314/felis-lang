use neco_felis_syn::Phase;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// (file_id, name_id_in_the_file)
pub struct NameId(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// (file_id, term_id_in_the_file)
pub struct TermId(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhaseResolved;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLetMutIds {
    pub value_id: NameId,
    pub reference_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementCallPtxIds {
    pub function_id: NameId,
    pub arg_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemTypeIds {
    pub name_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermVariableIds {
    pub term_id: TermId,
    pub name_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermArrowDepIds {
    pub term_id: TermId,
    pub param_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermMatchIds {
    pub term_id: TermId,
    pub scrutinee_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermApplyIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermArrowNodepIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermParenIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermStringIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermUnitIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermNumberIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermStructIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermFieldAccessIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermConstructorCallIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermVariableIds {
    pub term_id: TermId,
    pub name_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermFieldAccessIds {
    pub term_id: TermId,
    pub object_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermApplyIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermParenIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermUnitIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermNumberIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermConstructorCallIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermIfIds {
    pub term_id: TermId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermMatchIds {
    pub term_id: TermId,
    pub scrutinee_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermMatchFieldIds {
    pub binder_id: NameId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProcTermStructValueIds {
    pub term_id: TermId,
}

impl Phase for PhaseResolved {
    type FileExt = ();
    type ItemDefinitionExt = NameId;
    type ItemInductiveExt = NameId;
    type ItemInductiveBranchExt = ();
    type ItemTheoremExt = NameId;
    type ItemEntrypointExt = NameId;
    type ItemBuiltinExt = NameId;
    type TermApplyExt = TermApplyIds;
    type TermArrowDepExt = TermArrowDepIds;
    type TermArrowNodepExt = TermArrowNodepIds;
    type TermMatchExt = TermMatchIds;
    type TermMatchBranchExt = ();
    type TermParenExt = TermParenIds;
    type TermVariableExt = TermVariableIds;
    type TermStringExt = TermStringIds;
    type StatementsExt = ();
    type StatementsThenExt = ();
    type ItemProcBlockExt = ();
    type ItemProcExt = NameId;
    type TermUnitExt = TermUnitIds;
    type TermNumberExt = TermNumberIds;
    type ItemStructFieldExt = ();
    type ItemTypeExt = ItemTypeIds;
    type ItemTypeConstructorExt = ();
    type TermStructExt = TermStructIds;
    type TermFieldAccessExt = TermFieldAccessIds;
    type TermConstructorCallExt = TermConstructorCallIds;
    type StatementExt = ();
    type StatementLoopExt = ();
    type StatementBreakExt = ();
    type StatementAssignExt = NameId;
    type StatementFieldAssignExt = ();
    type StatementLetExt = NameId;
    type StatementLetMutExt = StatementLetMutIds;
    type ProcTermExt = ProcTermIds;
    type ProcTermApplyExt = ProcTermApplyIds;
    type ProcTermVariableExt = ProcTermVariableIds;
    type ProcTermParenExt = ProcTermParenIds;
    type ProcTermUnitExt = ProcTermUnitIds;
    type ProcTermNumberExt = ProcTermNumberIds;
    type ProcTermFieldAccessExt = ProcTermFieldAccessIds;
    type ProcTermConstructorCallExt = ProcTermConstructorCallIds;
    type ProcTermIfExt = ProcTermIfIds;
    type ProcTermStructValueExt = ProcTermStructValueIds;
    type ProcTermMatchExt = ProcTermMatchIds;
    type ProcTermMatchBranchExt = ();
    type ProcTermMatchFieldExt = ProcTermMatchFieldIds;
    type StatementReturnExt = ();
    type StatementCallPtxExt = StatementCallPtxIds;
}
