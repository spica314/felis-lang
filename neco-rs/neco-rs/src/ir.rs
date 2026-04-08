#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LoweredProgram {
    pub(crate) operations: Vec<Operation>,
    pub(crate) data: Vec<Vec<u8>>,
    pub(crate) arrays: Vec<ArrayAllocation>,
    pub(crate) heap_slots: usize,
    pub(crate) i32_slots: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum I32Expr {
    Literal(i32),
    Local(usize),
    Add(Box<I32Expr>, Box<I32Expr>),
    Sub(Box<I32Expr>, Box<I32Expr>),
    Mul(Box<I32Expr>, Box<I32Expr>),
    Div(Box<I32Expr>, Box<I32Expr>),
    Mod(Box<I32Expr>, Box<I32Expr>),
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
    ArrayGet {
        array_slot: usize,
        index: Box<I32Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExitCodeExpr {
    I32(I32Expr),
    U8(U8Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArrayElementType {
    I32,
    U8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ArrayAllocation {
    pub(crate) slot: usize,
    pub(crate) len: usize,
    pub(crate) element_type: ArrayElementType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Operation {
    StoreI32 {
        slot: usize,
        value: I32Expr,
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
    HeapStorePtr {
        heap_slot: usize,
        byte_offset: i32,
        source_heap_slot: usize,
    },
    Open {
        path_data_index: usize,
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

pub(crate) fn intern_data(program: &mut LoweredProgram, bytes: Vec<u8>) -> usize {
    program.data.push(bytes);
    program.data.len() - 1
}
