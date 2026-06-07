use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, F32Expr, I32Expr, I64Expr, KernelArgumentRef,
    LoweredProgram, PathBufSource, U8Expr,
};

use super::{
    ARGV_GLOBAL_ADDRESS, DATA_VIRTUAL_ADDRESS, data_addresses, emit_f32_expr_to_xmm0,
    emit_i32_expr_to_eax, emit_i64_expr_to_rax, emit_runtime_arg_ptr_to_rax, emit_u8_expr_to_eax,
};

pub(super) fn stack_frame_size(program: &LoweredProgram) -> usize {
    let array_descriptor_bytes = array_descriptor_bytes(program);
    let pointer_bytes = array_descriptor_bytes + program.heap_slots * 8;
    let i32_slot_bytes = program.i32_slots * 4;
    let i64_slot_bytes = program.i64_slots * 8;
    let f32_slot_bytes = program.f32_slots * 4;
    let u8_slot_bytes = program.u8_slots;
    let bool_slot_bytes = program.bool_slots;
    let array_bytes: usize = program.arrays.iter().map(array_storage_size).sum();
    let size = pointer_bytes
        + i32_slot_bytes
        + i64_slot_bytes
        + f32_slot_bytes
        + u8_slot_bytes
        + bool_slot_bytes
        + array_bytes;
    size.next_multiple_of(16)
}

pub(super) fn heap_slot_offset(slot: usize) -> i32 {
    -8 * (slot as i32 + 1)
}

pub(super) fn array_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let mut offset = (program.heap_slots * 8) as i32;
    for array in &program.arrays {
        offset += array_descriptor_size(array) as i32;
        if array.slot == slot {
            return -offset;
        }
    }
    panic!("unknown array slot {slot}");
}

pub(super) fn array_len_offset(program: &LoweredProgram, slot: usize) -> i32 {
    array_slot_offset(program, slot) + 8
}

pub(super) fn array_logical_len_offset(program: &LoweredProgram, slot: usize) -> i32 {
    array_len_offset(program, slot)
}

pub(super) fn array_data_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let pointer_bytes = pointer_bytes(program);
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let i64_slot_bytes = (program.i64_slots * 8) as i32;
    let f32_slot_bytes = (program.f32_slots * 4) as i32;
    let u8_slot_bytes = program.u8_slots as i32;
    let bool_slot_bytes = program.bool_slots as i32;
    let mut offset = pointer_bytes
        + i32_slot_bytes
        + i64_slot_bytes
        + f32_slot_bytes
        + u8_slot_bytes
        + bool_slot_bytes;
    for array in &program.arrays {
        offset += array_storage_size(array) as i32;
        if array.slot == slot {
            return -offset;
        }
    }
    panic!("unknown array slot {slot}");
}

pub(super) fn static_data_address(program: &LoweredProgram, data_index: usize) -> u64 {
    data_addresses(program, DATA_VIRTUAL_ADDRESS)[data_index]
}

pub(super) fn i32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    -(pointer_bytes(program) + 4 * (slot as i32 + 1))
}

pub(super) fn i64_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    -(pointer_bytes(program) + i32_slot_bytes + 8 * (slot as i32 + 1))
}

pub(super) fn f32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let i64_slot_bytes = (program.i64_slots * 8) as i32;
    -(pointer_bytes(program) + i32_slot_bytes + i64_slot_bytes + 4 * (slot as i32 + 1))
}

pub(super) fn u8_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let i64_slot_bytes = (program.i64_slots * 8) as i32;
    let f32_slot_bytes = (program.f32_slots * 4) as i32;
    -(pointer_bytes(program) + i32_slot_bytes + i64_slot_bytes + f32_slot_bytes + slot as i32 + 1)
}

pub(super) fn bool_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let i64_slot_bytes = (program.i64_slots * 8) as i32;
    let f32_slot_bytes = (program.f32_slots * 4) as i32;
    let u8_slot_bytes = program.u8_slots as i32;
    -(pointer_bytes(program)
        + i32_slot_bytes
        + i64_slot_bytes
        + f32_slot_bytes
        + u8_slot_bytes
        + slot as i32
        + 1)
}

pub(super) fn kernel_argument_ref_offset(program: &LoweredProgram, arg: KernelArgumentRef) -> i32 {
    match arg {
        KernelArgumentRef::I32(slot) => i32_slot_offset(program, slot),
        KernelArgumentRef::I64(slot) => i64_slot_offset(program, slot),
        KernelArgumentRef::F32(slot) => f32_slot_offset(program, slot),
        KernelArgumentRef::ArrayPtx(slot) => array_slot_offset(program, slot),
    }
}

pub(super) fn emit_array_initializers(program: &LoweredProgram, code: &mut Vec<u8>) {
    for array in &program.arrays {
        let slot_offset = array_slot_offset(program, array.slot);
        if matches!(array.kind, ArrayKind::DeviceDynamic) {
            code.extend_from_slice(&[0x48, 0xc7, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
            code.extend_from_slice(&0u32.to_le_bytes());
        } else {
            let data_offset = array_data_offset(program, array.slot);
            code.extend_from_slice(&[0x48, 0x8d, 0x85]);
            code.extend_from_slice(&data_offset.to_le_bytes());
            code.extend_from_slice(&[0x48, 0x89, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
        }
        let len_offset = array_len_offset(program, array.slot);
        code.extend_from_slice(&[0x48, 0xc7, 0x85]);
        code.extend_from_slice(&len_offset.to_le_bytes());
        code.extend_from_slice(&(array.len as u32).to_le_bytes());
    }
}

pub(super) fn array_descriptor_size(array: &ArrayAllocation) -> usize {
    match array.kind {
        ArrayKind::Fixed => 16,
        ArrayKind::Dynamic => 16,
        ArrayKind::DeviceDynamic => 16,
    }
}

pub(super) fn array_descriptor_bytes(program: &LoweredProgram) -> usize {
    program.arrays.iter().map(array_descriptor_size).sum()
}

pub(super) fn pointer_bytes(program: &LoweredProgram) -> i32 {
    (program.heap_slots * 8 + array_descriptor_bytes(program)) as i32
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

pub(super) fn emit_i32_array_set(
    array_slot: usize,
    index: &I64Expr,
    value: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
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
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x0f, 0xb6, 0x04, 0x0b]);
}
