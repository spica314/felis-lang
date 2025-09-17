use neco_felis_syn::Phase;

use crate::VariableId;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhaseRenamed();

impl Phase for PhaseRenamed {
    type FileExt = ();
    type ItemDefinitionExt = ();
    type ItemInductiveExt = ();
    type ItemInductiveBranchExt = ();
    type ItemTheoremExt = ();
    type ItemEntrypointExt = ();
    type ItemBuiltinExt = ();
    type TermApplyExt = ();
    type TermArrowDepExt = ();
    type TermArrowNodepExt = ();
    type TermMatchExt = ();
    type TermMatchBranchExt = ();
    type TermParenExt = ();
    type TermVariableExt = VariableId;
    type TermStringExt = ();
    type StatementsExt = ();
    type StatementsThenExt = ();
    type ItemProcBlockExt = ();
    type ItemProcExt = ();
    type TermUnitExt = ();
    type TermNumberExt = ();
    type ItemStructExt = ();
    type TermStructExt = ();
    type TermFieldAccessExt = ();
    type TermConstructorCallExt = ();
    type StatementExt = ();
    type StatementLoopExt = ();
    type StatementBreakExt = ();
    type StatementAssignExt = ();
    type StatementFieldAssignExt = ();
    type StatementLetExt = ();
    type StatementLetMutExt = ();
    type ProcTermExt = ();
    type ProcTermApplyExt = ();
    type ProcTermVariableExt = VariableId;
    type ProcTermParenExt = ();
    type ProcTermUnitExt = ();
    type ProcTermNumberExt = ();
    type ProcTermFieldAccessExt = ();
    type ProcTermConstructorCallExt = ();
    type ProcTermDereferenceExt = ();
    type ProcTermIfExt = ();
    type ProcTermStructValueExt = ();
    type StatementReturnExt = ();
    type StatementCallPtxExt = ();
}
