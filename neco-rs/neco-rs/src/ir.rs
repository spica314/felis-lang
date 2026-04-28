#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LoweredProgram {
    pub(crate) operations: Vec<Operation>,
    pub(crate) data: Vec<Vec<u8>>,
    pub(crate) arrays: Vec<ArrayAllocation>,
    pub(crate) heap_slots: usize,
    pub(crate) i32_slots: usize,
    pub(crate) i64_slots: usize,
    pub(crate) requires_argv: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum OpenPath {
    StaticData(usize),
    RuntimeArg(I32Expr),
    Array(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum I32Expr {
    Literal(i32),
    Local(usize),
    FromU8(Box<U8Expr>),
    Add(Box<I32Expr>, Box<I32Expr>),
    Sub(Box<I32Expr>, Box<I32Expr>),
    Mul(Box<I32Expr>, Box<I32Expr>),
    Div(Box<I32Expr>, Box<I32Expr>),
    Mod(Box<I32Expr>, Box<I32Expr>),
    ArrayGet {
        array_slot: usize,
        index: Box<I32Expr>,
    },
    ArrayLen {
        array_slot: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum I64Expr {
    Literal(i64),
    Local(usize),
    FromI32(Box<I32Expr>),
    FromU8(Box<U8Expr>),
    Add(Box<I64Expr>, Box<I64Expr>),
    Sub(Box<I64Expr>, Box<I64Expr>),
    Mul(Box<I64Expr>, Box<I64Expr>),
    Div(Box<I64Expr>, Box<I64Expr>),
    Mod(Box<I64Expr>, Box<I64Expr>),
    ArrayGet {
        array_slot: usize,
        index: Box<I32Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ComparisonKind {
    Eq,
    Lte,
    Lt,
    Gte,
    Gt,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ConditionExpr {
    I32 {
        kind: ComparisonKind,
        lhs: I32Expr,
        rhs: I32Expr,
    },
    I64 {
        kind: ComparisonKind,
        lhs: I64Expr,
        rhs: I64Expr,
    },
    U8 {
        kind: ComparisonKind,
        lhs: U8Expr,
        rhs: U8Expr,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum U8Expr {
    Literal(u8),
    Add(Box<U8Expr>, Box<U8Expr>),
    Sub(Box<U8Expr>, Box<U8Expr>),
    Mul(Box<U8Expr>, Box<U8Expr>),
    Div(Box<U8Expr>, Box<U8Expr>),
    Mod(Box<U8Expr>, Box<U8Expr>),
    RuntimeArgGet {
        arg_index: Box<I32Expr>,
        index: Box<I32Expr>,
    },
    StaticDataGet {
        data_index: usize,
        index: Box<I32Expr>,
    },
    ArrayGet {
        array_slot: usize,
        index: Box<I32Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExitCodeExpr {
    I32(I32Expr),
    I64(I64Expr),
    U8(U8Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArrayElementType {
    I32,
    I64,
    U8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArrayKind {
    Fixed,
    Dynamic,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ArrayAllocation {
    pub(crate) slot: usize,
    pub(crate) len: usize,
    pub(crate) element_type: ArrayElementType,
    pub(crate) kind: ArrayKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Operation {
    StoreI32 {
        slot: usize,
        value: I32Expr,
    },
    StoreI64 {
        slot: usize,
        value: I64Expr,
    },
    Mmap {
        len: I32Expr,
        result_slot: usize,
    },
    HeapStoreI32 {
        heap_slot: usize,
        byte_offset: i32,
        value: I32Expr,
    },
    HeapStoreI64 {
        heap_slot: usize,
        byte_offset: i32,
        value: I64Expr,
    },
    HeapStorePtr {
        heap_slot: usize,
        byte_offset: i32,
        source_heap_slot: usize,
    },
    Open {
        path: OpenPath,
        flags: I32Expr,
        mode: I32Expr,
        result_slot: usize,
    },
    Close {
        fd: I32Expr,
    },
    Read {
        fd: I32Expr,
        array_slot: usize,
        len: I32Expr,
        result_slot: usize,
    },
    WriteStatic {
        fd: I32Expr,
        data_index: usize,
        len: I32Expr,
    },
    WriteArray {
        fd: I32Expr,
        array_slot: usize,
        len: I32Expr,
    },
    ArraySetI32 {
        array_slot: usize,
        index: I32Expr,
        value: I32Expr,
    },
    ArraySetI64 {
        array_slot: usize,
        index: I32Expr,
        value: I64Expr,
    },
    ArraySetU8 {
        array_slot: usize,
        index: I32Expr,
        value: U8Expr,
    },
    If {
        condition: ConditionExpr,
        then_operations: Vec<Operation>,
        else_operations: Vec<Operation>,
    },
    Loop {
        body_operations: Vec<Operation>,
    },
    Break,
    Continue,
    Exit(ExitCodeExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ConstructorValue {
    pub(crate) type_name: String,
    pub(crate) constructor_name: String,
    pub(crate) heap_slot: Option<usize>,
    pub(crate) fields: Vec<crate::effect::Value>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructValue {
    pub(crate) type_name: String,
    pub(crate) heap_slot: Option<usize>,
    pub(crate) fields: Vec<StructFieldValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructFieldValue {
    pub(crate) name: String,
    pub(crate) value: crate::effect::Value,
}

pub(crate) fn intern_data(program: &mut LoweredProgram, bytes: Vec<u8>) -> usize {
    program.data.push(bytes);
    program.data.len() - 1
}
