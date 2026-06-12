use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, F32Expr, I32Expr, I64Expr, KernelArgumentRef,
    LoweredProgram, PathBufSource, U8Expr,
};

use super::{
    ARGV_GLOBAL_ADDRESS, DATA_VIRTUAL_ADDRESS, data_addresses, emit_f32_expr_to_xmm0,
    emit_i32_expr_to_eax, emit_i64_expr_to_rax, emit_runtime_arg_ptr_to_rax,
    emit_runtime_error_exit, emit_u8_expr_to_eax,
};

const ARRAY_DESCRIPTOR_SIZE: usize = 16;
const ARRAY_DESCRIPTOR_LEN_OFFSET: i32 = 8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct FrameLayout {
    stack_frame_size: usize,
    heap_slot_offsets: Vec<i32>,
    arrays: Vec<ArraySlotLayout>,
    i32_slot_offsets: Vec<i32>,
    i64_slot_offsets: Vec<i32>,
    f32_slot_offsets: Vec<i32>,
    u8_slot_offsets: Vec<i32>,
    bool_slot_offsets: Vec<i32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArraySlotLayout {
    slot: usize,
    descriptor_offset: i32,
    len_offset: i32,
    data_offset: Option<i32>,
}

impl FrameLayout {
    pub(super) fn new(program: &LoweredProgram) -> Self {
        let mut next_offset = 0i32;

        let heap_slot_offsets = (0..program.heap_slots)
            .map(|_| {
                next_offset += 8;
                -next_offset
            })
            .collect();

        let mut arrays = Vec::with_capacity(program.arrays.len());
        for array in &program.arrays {
            next_offset += array_descriptor_size(array) as i32;
            let descriptor_offset = -next_offset;
            arrays.push(ArraySlotLayout {
                slot: array.slot,
                descriptor_offset,
                len_offset: descriptor_offset + ARRAY_DESCRIPTOR_LEN_OFFSET,
                data_offset: None,
            });
        }

        let i32_slot_offsets = (0..program.i32_slots)
            .map(|_| {
                next_offset += 4;
                -next_offset
            })
            .collect();
        let i64_slot_offsets = (0..program.i64_slots)
            .map(|_| {
                next_offset += 8;
                -next_offset
            })
            .collect();
        let f32_slot_offsets = (0..program.f32_slots)
            .map(|_| {
                next_offset += 4;
                -next_offset
            })
            .collect();
        let u8_slot_offsets = (0..program.u8_slots)
            .map(|_| {
                next_offset += 1;
                -next_offset
            })
            .collect();
        let bool_slot_offsets = (0..program.bool_slots)
            .map(|_| {
                next_offset += 1;
                -next_offset
            })
            .collect();

        for (layout, array) in arrays.iter_mut().zip(&program.arrays) {
            if matches!(array.kind, ArrayKind::DeviceDynamic) {
                continue;
            }
            let storage_size = array_storage_size(array);
            next_offset += storage_size as i32;
            layout.data_offset = Some(-next_offset);
        }

        Self {
            stack_frame_size: (next_offset as usize).next_multiple_of(16),
            heap_slot_offsets,
            arrays,
            i32_slot_offsets,
            i64_slot_offsets,
            f32_slot_offsets,
            u8_slot_offsets,
            bool_slot_offsets,
        }
    }

    pub(super) fn stack_frame_size(&self) -> usize {
        self.stack_frame_size
    }

    pub(super) fn heap_slot_offset(&self, slot: usize) -> i32 {
        self.heap_slot_offsets[slot]
    }

    pub(super) fn array_slot_offset(&self, slot: usize) -> i32 {
        self.array_layout(slot).descriptor_offset
    }

    pub(super) fn array_len_offset(&self, slot: usize) -> i32 {
        self.array_layout(slot).len_offset
    }

    pub(super) fn array_data_offset(&self, slot: usize) -> i32 {
        self.array_layout(slot)
            .data_offset
            .unwrap_or_else(|| panic!("array slot {slot} has no inline storage"))
    }

    pub(super) fn i32_slot_offset(&self, slot: usize) -> i32 {
        self.i32_slot_offsets[slot]
    }

    pub(super) fn i64_slot_offset(&self, slot: usize) -> i32 {
        self.i64_slot_offsets[slot]
    }

    pub(super) fn f32_slot_offset(&self, slot: usize) -> i32 {
        self.f32_slot_offsets[slot]
    }

    pub(super) fn u8_slot_offset(&self, slot: usize) -> i32 {
        self.u8_slot_offsets[slot]
    }

    pub(super) fn bool_slot_offset(&self, slot: usize) -> i32 {
        self.bool_slot_offsets[slot]
    }

    fn array_layout(&self, slot: usize) -> &ArraySlotLayout {
        self.arrays
            .iter()
            .find(|array| array.slot == slot)
            .unwrap_or_else(|| panic!("unknown array slot {slot}"))
    }
}

pub(super) fn stack_frame_size(program: &LoweredProgram) -> usize {
    FrameLayout::new(program).stack_frame_size()
}

pub(super) fn heap_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).heap_slot_offset(slot)
}

pub(super) fn array_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).array_slot_offset(slot)
}

pub(super) fn array_len_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).array_len_offset(slot)
}

pub(super) fn array_logical_len_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).array_len_offset(slot)
}

pub(super) fn static_data_address(program: &LoweredProgram, data_index: usize) -> u64 {
    data_addresses(program, DATA_VIRTUAL_ADDRESS)[data_index]
}

pub(super) fn i32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).i32_slot_offset(slot)
}

pub(super) fn i64_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).i64_slot_offset(slot)
}

pub(super) fn f32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).f32_slot_offset(slot)
}

pub(super) fn u8_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).u8_slot_offset(slot)
}

pub(super) fn bool_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    FrameLayout::new(program).bool_slot_offset(slot)
}

pub(super) fn kernel_argument_ref_offset(program: &LoweredProgram, arg: KernelArgumentRef) -> i32 {
    match arg {
        KernelArgumentRef::I32(slot) => i32_slot_offset(program, slot),
        KernelArgumentRef::I64(slot) => i64_slot_offset(program, slot),
        KernelArgumentRef::F32(slot) => f32_slot_offset(program, slot),
        KernelArgumentRef::ArrayPtx(slot) => array_slot_offset(program, slot),
    }
}

pub(super) fn emit_array_initializers(
    program: &LoweredProgram,
    layout: &FrameLayout,
    code: &mut Vec<u8>,
) {
    for array in &program.arrays {
        let slot_offset = layout.array_slot_offset(array.slot);
        if matches!(array.kind, ArrayKind::DeviceDynamic) {
            code.extend_from_slice(&[0x48, 0xc7, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
            code.extend_from_slice(&0u32.to_le_bytes());
        } else {
            let data_offset = layout.array_data_offset(array.slot);
            code.extend_from_slice(&[0x48, 0x8d, 0x85]);
            code.extend_from_slice(&data_offset.to_le_bytes());
            code.extend_from_slice(&[0x48, 0x89, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
        }
        let len_offset = layout.array_len_offset(array.slot);
        code.extend_from_slice(&[0x48, 0xc7, 0x85]);
        code.extend_from_slice(&len_offset.to_le_bytes());
        code.extend_from_slice(&(array.len as u32).to_le_bytes());
    }
}

pub(super) fn array_descriptor_size(array: &ArrayAllocation) -> usize {
    match array.kind {
        ArrayKind::Fixed => ARRAY_DESCRIPTOR_SIZE,
        ArrayKind::Dynamic => ARRAY_DESCRIPTOR_SIZE,
        ArrayKind::DeviceDynamic => ARRAY_DESCRIPTOR_SIZE,
    }
}

pub(super) fn array_storage_size(array: &ArrayAllocation) -> usize {
    if matches!(array.kind, ArrayKind::DeviceDynamic) {
        return 0;
    }
    let element_size = array_element_size(array);
    array.len * element_size
}

pub(super) fn array_element_size(array: &ArrayAllocation) -> usize {
    match array.element_type {
        ArrayElementType::I32 => 4,
        ArrayElementType::I64 => 8,
        ArrayElementType::F32 => 4,
        ArrayElementType::U8 => 1,
    }
}

pub(super) fn array_allocation(program: &LoweredProgram, slot: usize) -> &ArrayAllocation {
    program
        .arrays
        .iter()
        .find(|array| array.slot == slot)
        .unwrap_or_else(|| panic!("unknown array slot {slot}"))
}

#[cfg(test)]
mod tests {
    use crate::ir::{ArrayAllocation, ArrayElementType, ArrayKind, LoweredProgram, Operation};

    use super::FrameLayout;

    fn program_with_layout_inputs() -> LoweredProgram {
        LoweredProgram {
            operations: Vec::<Operation>::new(),
            data: Vec::new(),
            compiled_ptx: Vec::new(),
            arrays: vec![
                ArrayAllocation {
                    slot: 0,
                    len: 3,
                    element_type: ArrayElementType::I32,
                    kind: ArrayKind::Fixed,
                },
                ArrayAllocation {
                    slot: 1,
                    len: 8,
                    element_type: ArrayElementType::U8,
                    kind: ArrayKind::DeviceDynamic,
                },
            ],
            heap_slots: 1,
            i32_slots: 2,
            i64_slots: 1,
            f32_slots: 1,
            u8_slots: 1,
            bool_slots: 1,
            requires_argv: false,
        }
    }

    #[test]
    fn frame_layout_assigns_named_offsets() {
        let program = program_with_layout_inputs();
        let layout = FrameLayout::new(&program);

        assert_eq!(layout.heap_slot_offset(0), -8);
        assert_eq!(layout.array_slot_offset(0), -24);
        assert_eq!(layout.array_len_offset(0), -16);
        assert_eq!(layout.array_slot_offset(1), -40);
        assert_eq!(layout.array_len_offset(1), -32);
        assert_eq!(layout.i32_slot_offset(0), -44);
        assert_eq!(layout.i32_slot_offset(1), -48);
        assert_eq!(layout.i64_slot_offset(0), -56);
        assert_eq!(layout.f32_slot_offset(0), -60);
        assert_eq!(layout.u8_slot_offset(0), -61);
        assert_eq!(layout.bool_slot_offset(0), -62);
        assert_eq!(layout.array_data_offset(0), -74);
        assert_eq!(layout.stack_frame_size(), 80);
    }

    #[test]
    fn frame_layout_keeps_zero_len_host_array_storage_addressable() {
        let program = LoweredProgram {
            operations: Vec::<Operation>::new(),
            data: Vec::new(),
            compiled_ptx: Vec::new(),
            arrays: vec![ArrayAllocation {
                slot: 0,
                len: 0,
                element_type: ArrayElementType::U8,
                kind: ArrayKind::Dynamic,
            }],
            heap_slots: 0,
            i32_slots: 0,
            i64_slots: 0,
            f32_slots: 0,
            u8_slots: 0,
            bool_slots: 0,
            requires_argv: false,
        };
        let layout = FrameLayout::new(&program);

        assert_eq!(layout.array_slot_offset(0), -16);
        assert_eq!(layout.array_len_offset(0), -8);
        assert_eq!(layout.array_data_offset(0), -16);
        assert_eq!(layout.stack_frame_size(), 16);
    }
}

pub(super) fn emit_i32_array_set(
    array_slot: usize,
    index: &I64Expr,
    value: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x02]);
    code.push(0x51);
    emit_i32_expr_to_eax(value, code, program);
    code.extend_from_slice(&[0x89, 0xc2]);
    code.push(0x59);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x89, 0x14, 0x0b]);
}

pub(super) fn emit_array_copy_byte_len_to_rdx(
    array_slot: usize,
    len: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(len, code, program);
    let element_size = array_element_size(array_allocation(program, array_slot));
    if element_size != 1 {
        code.extend_from_slice(&[0x6b, 0xc0, element_size as u8]);
    }
    code.extend_from_slice(&[0x48, 0x98]);
    code.extend_from_slice(&[0x48, 0x89, 0xc2]);
}

pub(super) fn emit_i64_array_set(
    array_slot: usize,
    index: &I64Expr,
    value: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x03]);
    code.push(0x51);
    emit_i64_expr_to_rax(value, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc2]);
    code.push(0x59);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x48, 0x89, 0x14, 0x0b]);
}

pub(super) fn emit_f32_array_set(
    array_slot: usize,
    index: &I64Expr,
    value: &F32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x02]);
    code.push(0x51);
    emit_f32_expr_to_xmm0(value, code, program);
    code.push(0x59);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0xf3, 0x0f, 0x11, 0x04, 0x0b]);
}

pub(super) fn emit_array_len(array_slot: usize, code: &mut Vec<u8>, program: &LoweredProgram) {
    let len_offset = array_logical_len_offset(program, array_slot);
    code.extend_from_slice(&[0x8b, 0x85]);
    code.extend_from_slice(&len_offset.to_le_bytes());
}

pub(super) fn emit_i64_array_len(array_slot: usize, code: &mut Vec<u8>, program: &LoweredProgram) {
    let len_offset = array_logical_len_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x85]);
    code.extend_from_slice(&len_offset.to_le_bytes());
}

pub(super) fn emit_u8_array_set(
    array_slot: usize,
    index: &I64Expr,
    value: &U8Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    code.push(0x51);
    emit_u8_expr_to_eax(value, code, program);
    code.extend_from_slice(&[0x89, 0xc2]);
    code.push(0x59);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x88, 0x14, 0x0b]);
}

pub(super) fn emit_pathbuf_push(
    path_slot: usize,
    source: &PathBufSource,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    addresses: &[u64],
) {
    let path_offset = array_slot_offset(program, path_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0xbd]);
    code.extend_from_slice(&path_offset.to_le_bytes());

    let find_nul_start = code.len();
    code.extend_from_slice(&[0x80, 0x3f, 0x00]);
    code.extend_from_slice(&[0x0f, 0x84]);
    let found_nul_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    code.extend_from_slice(&[0x48, 0xff, 0xc7]);
    code.push(0xe9);
    let find_back_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    let found_nul = code.len();
    let find_forward_len = found_nul as i32 - (found_nul_patch + 4) as i32;
    code[found_nul_patch..found_nul_patch + 4].copy_from_slice(&find_forward_len.to_le_bytes());
    let find_back_len = find_nul_start as i32 - (find_back_patch + 4) as i32;
    code[find_back_patch..find_back_patch + 4].copy_from_slice(&find_back_len.to_le_bytes());

    match source {
        PathBufSource::StaticData(data_index) => {
            code.extend_from_slice(&[0x48, 0xbe]);
            code.extend_from_slice(&addresses[*data_index].to_le_bytes());
        }
        PathBufSource::RuntimeArg(arg_index) => {
            emit_runtime_arg_ptr_to_rax(arg_index, code, program, ARGV_GLOBAL_ADDRESS);
            code.extend_from_slice(&[0x48, 0x89, 0xc6]);
        }
        PathBufSource::Array(array_slot) => {
            let source_offset = array_slot_offset(program, *array_slot);
            code.extend_from_slice(&[0x48, 0x8b, 0xb5]);
            code.extend_from_slice(&source_offset.to_le_bytes());
        }
    }

    let copy_start = code.len();
    code.extend_from_slice(&[0x8a, 0x06]);
    code.extend_from_slice(&[0x88, 0x07]);
    code.extend_from_slice(&[0x3c, 0x00]);
    code.extend_from_slice(&[0x0f, 0x84]);
    let copy_done_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    code.extend_from_slice(&[0x48, 0xff, 0xc6]);
    code.extend_from_slice(&[0x48, 0xff, 0xc7]);
    code.push(0xe9);
    let copy_back_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    let copy_done = code.len();
    let copy_forward_len = copy_done as i32 - (copy_done_patch + 4) as i32;
    code[copy_done_patch..copy_done_patch + 4].copy_from_slice(&copy_forward_len.to_le_bytes());
    let copy_back_len = copy_start as i32 - (copy_back_patch + 4) as i32;
    code[copy_back_patch..copy_back_patch + 4].copy_from_slice(&copy_back_len.to_le_bytes());
}

pub(super) fn emit_pathbuf_pop(path_slot: usize, code: &mut Vec<u8>, program: &LoweredProgram) {
    let path_offset = array_slot_offset(program, path_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0xbd]);
    code.extend_from_slice(&path_offset.to_le_bytes());
    code.extend_from_slice(&[0x48, 0x89, 0xfb]);
    code.extend_from_slice(&[0x48, 0x31, 0xc9]);

    let scan_start = code.len();
    code.extend_from_slice(&[0x80, 0x3f, 0x00]);
    code.extend_from_slice(&[0x0f, 0x84]);
    let scan_done_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    code.extend_from_slice(&[0x80, 0x3f, b'/']);
    code.extend_from_slice(&[0x0f, 0x85]);
    let not_slash_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    code.extend_from_slice(&[0x48, 0x89, 0xf9]);
    let not_slash = code.len();
    let not_slash_len = not_slash as i32 - (not_slash_patch + 4) as i32;
    code[not_slash_patch..not_slash_patch + 4].copy_from_slice(&not_slash_len.to_le_bytes());
    code.extend_from_slice(&[0x48, 0xff, 0xc7]);
    code.push(0xe9);
    let scan_back_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());

    let scan_done = code.len();
    let scan_done_len = scan_done as i32 - (scan_done_patch + 4) as i32;
    code[scan_done_patch..scan_done_patch + 4].copy_from_slice(&scan_done_len.to_le_bytes());
    let scan_back_len = scan_start as i32 - (scan_back_patch + 4) as i32;
    code[scan_back_patch..scan_back_patch + 4].copy_from_slice(&scan_back_len.to_le_bytes());

    code.extend_from_slice(&[0x48, 0x85, 0xc9]);
    code.extend_from_slice(&[0x0f, 0x85]);
    let has_slash_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    code.extend_from_slice(&[0xc6, 0x03, 0x00]);
    code.push(0xe9);
    let done_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    let has_slash = code.len();
    let has_slash_len = has_slash as i32 - (has_slash_patch + 4) as i32;
    code[has_slash_patch..has_slash_patch + 4].copy_from_slice(&has_slash_len.to_le_bytes());
    code.extend_from_slice(&[0xc6, 0x01, 0x00]);
    let done = code.len();
    let done_len = done as i32 - (done_patch + 4) as i32;
    code[done_patch..done_patch + 4].copy_from_slice(&done_len.to_le_bytes());
}

pub(super) fn emit_array_get(
    array_slot: usize,
    index: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x02]);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x8b, 0x04, 0x0b]);
}

pub(super) fn emit_i64_array_get(
    array_slot: usize,
    index: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x03]);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x48, 0x8b, 0x04, 0x0b]);
}

pub(super) fn emit_f32_array_get(
    array_slot: usize,
    index: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x02]);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0xf3, 0x0f, 0x10, 0x04, 0x0b]);
}

pub(super) fn emit_u8_array_get(
    array_slot: usize,
    index: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    emit_array_bounds_check(array_slot, code, program);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x0f, 0xb6, 0x04, 0x0b]);
}

fn emit_array_bounds_check(array_slot: usize, code: &mut Vec<u8>, program: &LoweredProgram) {
    code.extend_from_slice(&[0x48, 0x85, 0xc9]);
    code.extend_from_slice(&[0x0f, 0x88]);
    let negative_index_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    let len_offset = array_logical_len_offset(program, array_slot);
    code.extend_from_slice(&[0x8b, 0x85]);
    code.extend_from_slice(&len_offset.to_le_bytes());
    code.extend_from_slice(&[0x48, 0x98]);
    code.extend_from_slice(&[0x48, 0x39, 0xc1]);
    code.extend_from_slice(&[0x0f, 0x83]);
    let out_of_range_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    code.push(0xe9);
    let valid_patch = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    let error_start = code.len();
    emit_runtime_error_exit(code);
    let valid_start = code.len();

    for patch in [negative_index_patch, out_of_range_patch] {
        let offset = error_start as i32 - (patch + 4) as i32;
        code[patch..patch + 4].copy_from_slice(&offset.to_le_bytes());
    }
    let valid_offset = valid_start as i32 - (valid_patch + 4) as i32;
    code[valid_patch..valid_patch + 4].copy_from_slice(&valid_offset.to_le_bytes());
}
