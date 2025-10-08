pub trait Phase {
    type FileExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type ItemDefinitionExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemInductiveExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemInductiveBranchExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemTheoremExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemEntrypointExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemBuiltinExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermApplyExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type TermArrowDepExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermArrowNodepExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermMatchExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type TermMatchBranchExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermParenExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type TermVariableExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermStringExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementsExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementsThenExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemProcBlockExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemProcExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type TermUnitExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type TermNumberExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemStructExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ItemStructFieldExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermStructExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermFieldAccessExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type TermConstructorCallExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type StatementLoopExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementBreakExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementAssignExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementFieldAssignExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementLetExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementLetMutExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermExt: std::fmt::Debug + Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash;
    type ProcTermApplyExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermVariableExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermParenExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermUnitExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermNumberExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermFieldAccessExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermConstructorCallExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermDereferenceExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermIfExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type ProcTermStructValueExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementReturnExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
    type StatementCallPtxExt: std::fmt::Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PhaseParse();

impl Phase for PhaseParse {
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
    type TermVariableExt = ();
    type TermStringExt = ();
    type StatementsExt = ();
    type StatementsThenExt = ();
    type ItemProcBlockExt = ();
    type ItemProcExt = ();
    type TermUnitExt = ();
    type TermNumberExt = ();
    type ItemStructExt = ();
    type ItemStructFieldExt = ();
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
    type ProcTermVariableExt = ();
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
