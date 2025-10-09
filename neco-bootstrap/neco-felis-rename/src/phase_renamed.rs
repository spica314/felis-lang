use neco_felis_syn::Phase;

use crate::VariableId;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhaseRenamed();

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementLetMutIds {
    pub value_id: VariableId,
    pub reference_id: VariableId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementCallPtxIds {
    pub function_id: VariableId,
    pub arg_id: VariableId,
}

impl Phase for PhaseRenamed {
    type FileExt = ();
    type ItemDefinitionExt = VariableId;
    type ItemInductiveExt = VariableId;
    type ItemInductiveBranchExt = ();
    type ItemTheoremExt = VariableId;
    type ItemEntrypointExt = VariableId;
    type ItemBuiltinExt = VariableId;
    type TermApplyExt = ();
    type TermArrowDepExt = VariableId;
    type TermArrowNodepExt = ();
    type TermMatchExt = VariableId;
    type TermMatchBranchExt = ();
    type TermParenExt = ();
    type TermVariableExt = VariableId;
    type TermStringExt = ();
    type StatementsExt = ();
    type StatementsThenExt = ();
    type ItemProcBlockExt = ();
    type ItemProcExt = VariableId;
    type TermUnitExt = ();
    type TermNumberExt = ();
    type ItemStructExt = VariableId;
    type ItemStructFieldExt = ();
    type TermStructExt = ();
    type TermFieldAccessExt = VariableId;
    type TermConstructorCallExt = ();
    type StatementExt = ();
    type StatementLoopExt = ();
    type StatementBreakExt = ();
    type StatementAssignExt = VariableId;
    type StatementFieldAssignExt = ();
    type StatementLetExt = VariableId;
    type StatementLetMutExt = StatementLetMutIds;
    type ProcTermExt = ();
    type ProcTermApplyExt = ();
    type ProcTermVariableExt = VariableId;
    type ProcTermParenExt = ();
    type ProcTermUnitExt = ();
    type ProcTermNumberExt = ();
    type ProcTermFieldAccessExt = VariableId;
    type ProcTermConstructorCallExt = ();
    type ProcTermDereferenceExt = ();
    type ProcTermIfExt = ();
    type ProcTermStructValueExt = ();
    type StatementReturnExt = ();
    type StatementCallPtxExt = StatementCallPtxIds;
}
