use neco_rs_elf::{Elf64Executable, LoadSegment, SegmentFlags};

use crate::ir::{
    ArrayAllocation, ArrayElementType, ComparisonKind, ConditionExpr, ExitCodeExpr, I32Expr,
    LoweredProgram, Operation, U8Expr,
};

pub(crate) fn build_linux_x86_64_program_executable(program: &LoweredProgram) -> Elf64Executable {
    let code_virtual_address = 0x401000;
    let data_virtual_address = 0x402000;
    let mut elf = Elf64Executable::new(code_virtual_address);
    elf.add_load_segment(LoadSegment::new(
        code_virtual_address,
        0,
        SegmentFlags::READ_EXECUTE,
        program_syscall_code(program, data_virtual_address),
    ));
    if !program.data.is_empty() {
        elf.add_load_segment(LoadSegment::new(
            data_virtual_address,
            0x1000,
            SegmentFlags::READ_ONLY,
            flatten_data(program),
        ));
    }
    elf
}

fn flatten_data(program: &LoweredProgram) -> Vec<u8> {
    let total_len = program.data.iter().map(Vec::len).sum();
    let mut data = Vec::with_capacity(total_len);
    for bytes in &program.data {
        data.extend_from_slice(bytes);
    }
    data
}

fn data_addresses(program: &LoweredProgram, data_virtual_address: u64) -> Vec<u64> {
    let mut next_address = data_virtual_address;
    let mut addresses = Vec::with_capacity(program.data.len());
    for bytes in &program.data {
        addresses.push(next_address);
        next_address += bytes.len() as u64;
    }
    addresses
}

fn program_syscall_code(program: &LoweredProgram, data_virtual_address: u64) -> Vec<u8> {
    let addresses = data_addresses(program, data_virtual_address);
    let mut code = Vec::new();
    let stack_frame_size = stack_frame_size(program);

    if stack_frame_size > 0 {
        code.push(0x55);
        code.extend_from_slice(&[0x48, 0x89, 0xe5]);
        code.extend_from_slice(&[0x48, 0x81, 0xec]);
        code.extend_from_slice(&(stack_frame_size as u32).to_le_bytes());
        emit_array_initializers(program, &mut code);
    }

    emit_operations(
        &program.operations,
        &mut code,
        program,
        &addresses,
        None,
        None,
    );

    code
}

fn emit_operations(
    operations: &[Operation],
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    addresses: &[u64],
    mut break_patches: Option<&mut Vec<usize>>,
    mut continue_patches: Option<&mut Vec<usize>>,
) {
    for operation in operations {
        match operation {
            Operation::StoreI32 { slot, value } => {
                emit_i32_expr_to_eax(value, code, program);
                let slot_offset = i32_slot_offset(program, *slot);
                code.extend_from_slice(&[0x89, 0x85]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
            }
            Operation::Mmap { len, result_slot } => {
                code.extend_from_slice(&[0x31, 0xff]);
                emit_i32_expr_to_eax(len, code, program);
                code.extend_from_slice(&[0x89, 0xc6]);
                code.extend_from_slice(&[0xba, 0x03, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x41, 0xba, 0x22, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x41, 0xb8, 0xff, 0xff, 0xff, 0xff]);
                code.extend_from_slice(&[0x45, 0x31, 0xc9]);
                code.extend_from_slice(&[0xb8, 0x09, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
                let slot_offset = heap_slot_offset(*result_slot);
                code.extend_from_slice(&[0x48, 0x89, 0x85]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
            }
            Operation::HeapStoreI32 {
                heap_slot,
                byte_offset,
                value,
            } => {
                emit_i32_expr_to_eax(value, code, program);
                code.extend_from_slice(&[0x89, 0xc2]);
                let slot_offset = heap_slot_offset(*heap_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                code.extend_from_slice(&[0x89, 0x93]);
                code.extend_from_slice(&byte_offset.to_le_bytes());
            }
            Operation::HeapStorePtr {
                heap_slot,
                byte_offset,
                source_heap_slot,
            } => {
                let source_slot_offset = heap_slot_offset(*source_heap_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0x85]);
                code.extend_from_slice(&source_slot_offset.to_le_bytes());
                code.extend_from_slice(&[0x48, 0x89, 0xc1]);
                let target_slot_offset = heap_slot_offset(*heap_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0x95]);
                code.extend_from_slice(&target_slot_offset.to_le_bytes());
                code.extend_from_slice(&[0x48, 0x89, 0x8a]);
                code.extend_from_slice(&byte_offset.to_le_bytes());
            }
            Operation::Open {
                path_data_index,
                flags,
                mode,
                result_slot,
            } => {
                code.extend_from_slice(&[0x48, 0xbf]);
                code.extend_from_slice(&addresses[*path_data_index].to_le_bytes());
                emit_i32_expr_to_eax(flags, code, program);
                code.extend_from_slice(&[0x89, 0xc6]);
                emit_i32_expr_to_eax(mode, code, program);
                code.extend_from_slice(&[0x89, 0xc2]);
                code.extend_from_slice(&[0xb8, 0x02, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
                let result_offset = i32_slot_offset(program, *result_slot);
                code.extend_from_slice(&[0x89, 0x85]);
                code.extend_from_slice(&result_offset.to_le_bytes());
            }
            Operation::Close { fd } => {
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                code.extend_from_slice(&[0xb8, 0x03, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::Read {
                fd,
                array_slot,
                len,
                result_slot,
            } => {
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                let slot_offset = array_slot_offset(program, *array_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0xb5]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                emit_i32_expr_to_eax(len, code, program);
                code.extend_from_slice(&[0x89, 0xc2]);
                code.extend_from_slice(&[0xb8, 0x00, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
                let result_offset = i32_slot_offset(program, *result_slot);
                code.extend_from_slice(&[0x89, 0x85]);
                code.extend_from_slice(&result_offset.to_le_bytes());
            }
            Operation::WriteStatic {
                fd,
                data_index,
                len,
            } => {
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                code.extend_from_slice(&[0x48, 0xbe]);
                code.extend_from_slice(&addresses[*data_index].to_le_bytes());
                emit_i32_expr_to_eax(len, code, program);
                code.extend_from_slice(&[0x89, 0xc2]);
                code.extend_from_slice(&[0xb8, 0x01, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::WriteArray {
                fd,
                array_slot,
                len,
            } => {
                let array = array_allocation(program, *array_slot);
                debug_assert_eq!(array.element_type, ArrayElementType::U8);
                emit_i32_expr_to_eax(fd, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                let slot_offset = array_slot_offset(program, *array_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0xb5]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                emit_i32_expr_to_eax(len, code, program);
                code.extend_from_slice(&[0x89, 0xc2]);
                code.extend_from_slice(&[0xb8, 0x01, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
            }
            Operation::If {
                condition,
                then_operations,
                else_operations,
            } => {
                emit_condition_false_jump(condition, code, program);
                let false_patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                let then_start = code.len();
                emit_operations(
                    then_operations,
                    code,
                    program,
                    addresses,
                    break_patches.as_deref_mut(),
                    continue_patches.as_deref_mut(),
                );
                if else_operations.is_empty() {
                    let end = code.len();
                    let false_jump_len = (end - then_start) as i32;
                    code[false_patch_at..false_patch_at + 4]
                        .copy_from_slice(&false_jump_len.to_le_bytes());
                } else {
                    code.extend_from_slice(&[0xe9]);
                    let end_patch_at = code.len();
                    code.extend_from_slice(&0i32.to_le_bytes());
                    let else_start = code.len();
                    emit_operations(
                        else_operations,
                        code,
                        program,
                        addresses,
                        break_patches.as_deref_mut(),
                        continue_patches.as_deref_mut(),
                    );
                    let end = code.len();
                    let false_jump_len = (else_start - then_start) as i32;
                    code[false_patch_at..false_patch_at + 4]
                        .copy_from_slice(&false_jump_len.to_le_bytes());
                    let end_jump_len = (end - else_start) as i32;
                    code[end_patch_at..end_patch_at + 4]
                        .copy_from_slice(&end_jump_len.to_le_bytes());
                }
            }
            Operation::ArraySetI32 {
                array_slot,
                index,
                value,
            } => emit_i32_array_set(*array_slot, index, value, code, program),
            Operation::ArraySetU8 {
                array_slot,
                index,
                value,
            } => emit_u8_array_set(*array_slot, index, value, code, program),
            Operation::Loop { body_operations } => {
                let loop_start = code.len();
                let mut loop_break_patches = Vec::new();
                let mut loop_continue_patches = Vec::new();
                emit_operations(
                    body_operations,
                    code,
                    program,
                    addresses,
                    Some(&mut loop_break_patches),
                    Some(&mut loop_continue_patches),
                );
                code.push(0xe9);
                let back_patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                let loop_end = code.len();
                let back_jump_len = loop_start as i32 - loop_end as i32;
                code[back_patch_at..back_patch_at + 4]
                    .copy_from_slice(&back_jump_len.to_le_bytes());
                for patch_at in loop_continue_patches {
                    let continue_jump_len = loop_start as i32 - (patch_at as i32 + 4);
                    code[patch_at..patch_at + 4].copy_from_slice(&continue_jump_len.to_le_bytes());
                }
                for patch_at in loop_break_patches {
                    let break_jump_len = loop_end as i32 - (patch_at as i32 + 4);
                    code[patch_at..patch_at + 4].copy_from_slice(&break_jump_len.to_le_bytes());
                }
            }
            Operation::Break => {
                code.push(0xe9);
                let patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                break_patches
                    .as_deref_mut()
                    .expect("break must be lowered inside a loop")
                    .push(patch_at);
            }
            Operation::Continue => {
                code.push(0xe9);
                let patch_at = code.len();
                code.extend_from_slice(&0i32.to_le_bytes());
                continue_patches
                    .as_deref_mut()
                    .expect("continue must be lowered inside a loop")
                    .push(patch_at);
            }
            Operation::Exit(exit_code) => {
                emit_exit_code_expr_to_eax(exit_code, code, program);
                code.extend_from_slice(&[0x89, 0xc7]);
                code.extend_from_slice(&[0xb8, 0x3c, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
            }
        }
    }
}

fn emit_condition_false_jump(
    condition: &ConditionExpr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    match condition {
        ConditionExpr::I32 { kind, lhs, rhs } => {
            emit_i32_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_i32_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_false(*kind, false, code);
        }
        ConditionExpr::U8 { kind, lhs, rhs } => {
            emit_u8_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_u8_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_false(*kind, true, code);
        }
    }
}

fn emit_jcc_false(kind: ComparisonKind, unsigned: bool, code: &mut Vec<u8>) {
    code.extend_from_slice(&[0x0f, false_jump_opcode(kind, unsigned)]);
}

fn false_jump_opcode(kind: ComparisonKind, unsigned: bool) -> u8 {
    match (kind, unsigned) {
        (ComparisonKind::Eq, _) => 0x85,
        (ComparisonKind::Lte, false) => 0x8f,
        (ComparisonKind::Lt, false) => 0x8d,
        (ComparisonKind::Gte, false) => 0x8c,
        (ComparisonKind::Gt, false) => 0x8e,
        (ComparisonKind::Lte, true) => 0x87,
        (ComparisonKind::Lt, true) => 0x83,
        (ComparisonKind::Gte, true) => 0x82,
        (ComparisonKind::Gt, true) => 0x86,
    }
}

fn emit_exit_code_expr_to_eax(expr: &ExitCodeExpr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        ExitCodeExpr::I32(expr) => emit_i32_expr_to_eax(expr, code, program),
        ExitCodeExpr::U8(expr) => emit_u8_expr_to_eax(expr, code, program),
    }
}

fn emit_i32_expr_to_eax(expr: &I32Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        I32Expr::Literal(value) => {
            code.push(0xb8);
            code.extend_from_slice(&value.to_le_bytes());
        }
        I32Expr::Local(slot) => {
            let slot_offset = i32_slot_offset(program, *slot);
            code.extend_from_slice(&[0x8b, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
        }
        I32Expr::Add(lhs, rhs) => emit_i32_binary_expr(lhs, rhs, code, program, &[0x01, 0xc8]),
        I32Expr::Sub(lhs, rhs) => emit_i32_binary_expr(lhs, rhs, code, program, &[0x29, 0xc8]),
        I32Expr::Mul(lhs, rhs) => {
            emit_i32_binary_expr(lhs, rhs, code, program, &[0x0f, 0xaf, 0xc1])
        }
        I32Expr::Div(lhs, rhs) => emit_i32_div_mod_expr(lhs, rhs, code, program, false),
        I32Expr::Mod(lhs, rhs) => emit_i32_div_mod_expr(lhs, rhs, code, program, true),
        I32Expr::ArrayGet { array_slot, index } => {
            emit_array_get(*array_slot, index, code, program)
        }
    }
}

fn emit_i32_binary_expr(
    lhs: &I32Expr,
    rhs: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    opcode: &[u8],
) {
    emit_i32_expr_to_eax(lhs, code, program);
    code.push(0x50);
    emit_i32_expr_to_eax(rhs, code, program);
    code.extend_from_slice(&[0x89, 0xc1]);
    code.push(0x58);
    code.extend_from_slice(opcode);
}

fn emit_i32_div_mod_expr(
    lhs: &I32Expr,
    rhs: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    modulo: bool,
) {
    emit_i32_expr_to_eax(lhs, code, program);
    code.push(0x50);
    emit_i32_expr_to_eax(rhs, code, program);
    code.extend_from_slice(&[0x89, 0xc1]);
    code.push(0x58);
    code.push(0x99);
    code.extend_from_slice(&[0xf7, 0xf9]);
    if modulo {
        code.extend_from_slice(&[0x89, 0xd0]);
    }
}

fn emit_u8_expr_to_eax(expr: &U8Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        U8Expr::Literal(value) => {
            code.push(0xb8);
            code.extend_from_slice(&u32::from(*value).to_le_bytes());
        }
        U8Expr::Add(lhs, rhs) => emit_u8_binary_expr(lhs, rhs, code, program, &[0x00, 0xc8]),
        U8Expr::Sub(lhs, rhs) => emit_u8_binary_expr(lhs, rhs, code, program, &[0x28, 0xc8]),
        U8Expr::Mul(lhs, rhs) => emit_u8_mul_expr(lhs, rhs, code, program),
        U8Expr::Div(lhs, rhs) => emit_u8_div_mod_expr(lhs, rhs, code, program, false),
        U8Expr::Mod(lhs, rhs) => emit_u8_div_mod_expr(lhs, rhs, code, program, true),
        U8Expr::ArrayGet { array_slot, index } => {
            emit_u8_array_get(*array_slot, index, code, program)
        }
    }
}

fn emit_u8_binary_expr(
    lhs: &U8Expr,
    rhs: &U8Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    opcode: &[u8],
) {
    emit_u8_expr_to_eax(lhs, code, program);
    code.push(0x50);
    emit_u8_expr_to_eax(rhs, code, program);
    code.extend_from_slice(&[0x89, 0xc1]);
    code.push(0x58);
    code.extend_from_slice(opcode);
    code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
}

fn emit_u8_mul_expr(lhs: &U8Expr, rhs: &U8Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    emit_u8_expr_to_eax(lhs, code, program);
    code.push(0x50);
    emit_u8_expr_to_eax(rhs, code, program);
    code.extend_from_slice(&[0x89, 0xc1]);
    code.push(0x58);
    code.extend_from_slice(&[0x0f, 0xaf, 0xc1]);
    code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
}

fn emit_u8_div_mod_expr(
    lhs: &U8Expr,
    rhs: &U8Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    modulo: bool,
) {
    emit_u8_expr_to_eax(lhs, code, program);
    code.push(0x50);
    emit_u8_expr_to_eax(rhs, code, program);
    code.extend_from_slice(&[0x89, 0xc1]);
    code.push(0x58);
    code.extend_from_slice(&[0x31, 0xd2]);
    code.extend_from_slice(&[0xf7, 0xf1]);
    if modulo {
        code.extend_from_slice(&[0x89, 0xd0]);
    }
    code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
}

fn stack_frame_size(program: &LoweredProgram) -> usize {
    let pointer_bytes = (program.arrays.len() + program.heap_slots) * 8;
    let i32_slot_bytes = program.i32_slots * 4;
    let array_bytes: usize = program.arrays.iter().map(array_storage_size).sum();
    pointer_bytes + i32_slot_bytes + array_bytes
}

fn heap_slot_offset(slot: usize) -> i32 {
    -8 * (slot as i32 + 1)
}

fn array_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    -8 * (program.heap_slots as i32 + slot as i32 + 1)
}

fn array_data_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let pointer_bytes = ((program.arrays.len() + program.heap_slots) * 8) as i32;
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let mut offset = pointer_bytes + i32_slot_bytes;
    for array in &program.arrays {
        offset += array_storage_size(array) as i32;
        if array.slot == slot {
            return -offset;
        }
    }
    panic!("unknown array slot {slot}");
}

fn i32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let pointer_bytes = ((program.arrays.len() + program.heap_slots) * 8) as i32;
    -(pointer_bytes + 4 * (slot as i32 + 1))
}

fn emit_array_initializers(program: &LoweredProgram, code: &mut Vec<u8>) {
    for array in &program.arrays {
        let slot_offset = array_slot_offset(program, array.slot);
        let data_offset = array_data_offset(program, array.slot);
        code.extend_from_slice(&[0x48, 0x8d, 0x85]);
        code.extend_from_slice(&data_offset.to_le_bytes());
        code.extend_from_slice(&[0x48, 0x89, 0x85]);
        code.extend_from_slice(&slot_offset.to_le_bytes());
    }
}

fn array_storage_size(array: &ArrayAllocation) -> usize {
    let element_size = match array.element_type {
        ArrayElementType::I32 => 4,
        ArrayElementType::U8 => 1,
    };
    array.len * element_size
}

fn array_allocation(program: &LoweredProgram, slot: usize) -> &ArrayAllocation {
    program
        .arrays
        .iter()
        .find(|array| array.slot == slot)
        .unwrap_or_else(|| panic!("unknown array slot {slot}"))
}

fn emit_i32_array_set(
    array_slot: usize,
    index: &I32Expr,
    value: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
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

fn emit_u8_array_set(
    array_slot: usize,
    index: &I32Expr,
    value: &U8Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
    code.push(0x51);
    emit_u8_expr_to_eax(value, code, program);
    code.extend_from_slice(&[0x89, 0xc2]);
    code.push(0x59);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x88, 0x14, 0x0b]);
}

fn emit_array_get(
    array_slot: usize,
    index: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
    code.extend_from_slice(&[0x48, 0xc1, 0xe1, 0x02]);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x8b, 0x04, 0x0b]);
}

fn emit_u8_array_get(
    array_slot: usize,
    index: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    emit_i32_expr_to_eax(index, code, program);
    code.extend_from_slice(&[0x48, 0x63, 0xc8]);
    let slot_offset = array_slot_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
    code.extend_from_slice(&slot_offset.to_le_bytes());
    code.extend_from_slice(&[0x0f, 0xb6, 0x04, 0x0b]);
}
