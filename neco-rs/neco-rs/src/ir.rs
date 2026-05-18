#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LoweredProgram {
    pub(crate) operations: Vec<Operation>,
    pub(crate) data: Vec<Vec<u8>>,
    pub(crate) arrays: Vec<ArrayAllocation>,
    pub(crate) heap_slots: usize,
    pub(crate) i32_slots: usize,
    pub(crate) i64_slots: usize,
    pub(crate) f32_slots: usize,
    pub(crate) requires_argv: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum OpenPath {
    PathBuf(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum PathBufSource {
    StaticData(usize),
    RuntimeArg(I32Expr),
    Array(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum KernelArgumentRef {
    I32(usize),
    I64(usize),
    F32(usize),
    ArrayPtx(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum I32Expr {
    Literal(i32),
    Local(usize),
    FromU8(Box<U8Expr>),
    FromI64(Box<I64Expr>),
    FromF32(Box<F32Expr>),
    Add(Box<I32Expr>, Box<I32Expr>),
    Sub(Box<I32Expr>, Box<I32Expr>),
    Mul(Box<I32Expr>, Box<I32Expr>),
    Div(Box<I32Expr>, Box<I32Expr>),
    Mod(Box<I32Expr>, Box<I32Expr>),
    ArrayGet {
        array_slot: usize,
        index: Box<I64Expr>,
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
    FromF32(Box<F32Expr>),
    Add(Box<I64Expr>, Box<I64Expr>),
    Sub(Box<I64Expr>, Box<I64Expr>),
    Mul(Box<I64Expr>, Box<I64Expr>),
    Div(Box<I64Expr>, Box<I64Expr>),
    Mod(Box<I64Expr>, Box<I64Expr>),
    ArrayGet {
        array_slot: usize,
        index: Box<I64Expr>,
    },
    ArrayLen {
        array_slot: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum F32Expr {
    LiteralBits(u32),
    Local(usize),
    FromI32(Box<I32Expr>),
    FromI64(Box<I64Expr>),
    FromU8(Box<U8Expr>),
    Add(Box<F32Expr>, Box<F32Expr>),
    Sub(Box<F32Expr>, Box<F32Expr>),
    Mul(Box<F32Expr>, Box<F32Expr>),
    Div(Box<F32Expr>, Box<F32Expr>),
    Sqrt(Box<F32Expr>),
    ArrayGet {
        array_slot: usize,
        index: Box<I64Expr>,
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
    Literal(bool),
    And(Box<ConditionExpr>, Box<ConditionExpr>),
    Or(Box<ConditionExpr>, Box<ConditionExpr>),
    Not(Box<ConditionExpr>),
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
    F32 {
        kind: ComparisonKind,
        lhs: F32Expr,
        rhs: F32Expr,
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
    FromI32(Box<I32Expr>),
    FromI64(Box<I64Expr>),
    FromF32(Box<F32Expr>),
    Add(Box<U8Expr>, Box<U8Expr>),
    Sub(Box<U8Expr>, Box<U8Expr>),
    Mul(Box<U8Expr>, Box<U8Expr>),
    Div(Box<U8Expr>, Box<U8Expr>),
    Mod(Box<U8Expr>, Box<U8Expr>),
    RuntimeArgGet {
        arg_index: Box<I32Expr>,
        index: Box<I64Expr>,
    },
    StaticDataGet {
        data_index: usize,
        index: Box<I64Expr>,
    },
    ArrayGet {
        array_slot: usize,
        index: Box<I64Expr>,
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
    F32,
    U8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArrayKind {
    Fixed,
    Dynamic,
    DeviceDynamic,
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
    StoreF32 {
        slot: usize,
        value: F32Expr,
    },
    Mmap {
        len: I32Expr,
        result_slot: usize,
    },
    ArrayAllocDynamic {
        array_slot: usize,
        len: I32Expr,
    },
    ArrayReplace {
        dest_slot: usize,
        source_slot: usize,
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
    PathBufPush {
        path_slot: usize,
        source: PathBufSource,
    },
    PathBufPop {
        path_slot: usize,
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
    CuInit {
        flags: I32Expr,
        result_slot: usize,
    },
    CuDeviceGet {
        device_slot: usize,
        ordinal: I32Expr,
        result_slot: usize,
    },
    CuCtxCreateV2 {
        ctx_slot: usize,
        flags: I32Expr,
        device: I32Expr,
        result_slot: usize,
    },
    CuModuleLoadData {
        module_slot: usize,
        data_index: usize,
        result_slot: usize,
    },
    CuModuleGetFunction {
        function_slot: usize,
        module: I64Expr,
        name_data_index: usize,
        result_slot: usize,
    },
    CuLaunchKernel {
        function: I64Expr,
        arg: KernelArgumentRef,
        grid_dim_x: I32Expr,
        grid_dim_y: I32Expr,
        grid_dim_z: I32Expr,
        block_dim_x: I32Expr,
        block_dim_y: I32Expr,
        block_dim_z: I32Expr,
        shared_mem_bytes: I32Expr,
        stream: I64Expr,
        result_slot: usize,
    },
    CuMemAllocV2 {
        array_slot: usize,
        len: I32Expr,
        result_slot: usize,
    },
    CuMemcpyHtoDV2 {
        dest_slot: usize,
        source_slot: usize,
        len: I32Expr,
        result_slot: usize,
    },
    CuMemcpyDtoHV2 {
        dest_slot: usize,
        source_slot: usize,
        len: I32Expr,
        result_slot: usize,
    },
    ArraySetI32 {
        array_slot: usize,
        index: I64Expr,
        value: I32Expr,
    },
    ArraySetI64 {
        array_slot: usize,
        index: I64Expr,
        value: I64Expr,
    },
    ArraySetF32 {
        array_slot: usize,
        index: I64Expr,
        value: F32Expr,
    },
    ArraySetU8 {
        array_slot: usize,
        index: I64Expr,
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
