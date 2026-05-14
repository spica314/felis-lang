use neco_rs_elf::{Elf64Executable, LoadSegment, SegmentFlags};

use crate::ir::{
    ArrayAllocation, ArrayElementType, ArrayKind, ComparisonKind, ConditionExpr, ExitCodeExpr,
    F32Expr, I32Expr, I64Expr, LoweredProgram, OpenPath, Operation, PathBufSource, U8Expr,
};

const DATA_VIRTUAL_ADDRESS: u64 = 0x410000;
const ARGV_GLOBAL_ADDRESS: u64 = 0x420000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryAbi {
    KernelStart,
    LibcMain,
}

pub(crate) struct ProgramImage {
    pub(crate) code: Vec<u8>,
    pub(crate) data: Vec<u8>,
    pub(crate) requires_argv: bool,
}

pub(crate) fn build_linux_x86_64_program_executable(program: &LoweredProgram) -> Elf64Executable {
    let code_virtual_address = 0x401000;
    let data_virtual_address = DATA_VIRTUAL_ADDRESS;
    let mut elf = Elf64Executable::new(code_virtual_address);
    elf.add_load_segment(LoadSegment::new(
        code_virtual_address,
        0,
        SegmentFlags::READ_EXECUTE,
        build_linux_x86_64_program_image(program, EntryAbi::KernelStart).code,
    ));
    if !program.data.is_empty() {
        elf.add_load_segment(LoadSegment::new(
            data_virtual_address,
            0x1000,
            SegmentFlags::READ_ONLY,
            flatten_data(program),
        ));
    }
    if program.requires_argv {
        elf.add_load_segment(LoadSegment::new(
            ARGV_GLOBAL_ADDRESS,
            0x1000,
            SegmentFlags::READ_WRITE,
            vec![0; 8],
        ));
    }
    elf
}

pub(crate) fn build_linux_x86_64_program_image(
    program: &LoweredProgram,
    entry_abi: EntryAbi,
) -> ProgramImage {
    ProgramImage {
        code: program_syscall_code(
            program,
            DATA_VIRTUAL_ADDRESS,
            program.requires_argv.then_some(ARGV_GLOBAL_ADDRESS),
            entry_abi,
        ),
        data: flatten_data(program),
        requires_argv: program.requires_argv,
    }
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

fn program_syscall_code(
    program: &LoweredProgram,
    data_virtual_address: u64,
    argv_global_address: Option<u64>,
    entry_abi: EntryAbi,
) -> Vec<u8> {
    let addresses = data_addresses(program, data_virtual_address);
    let mut code = Vec::new();
    let stack_frame_size = stack_frame_size(program);

    if let Some(argv_global_address) = argv_global_address {
        emit_argv_global_init(&mut code, argv_global_address, entry_abi);
    }

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
            Operation::StoreI64 { slot, value } => {
                emit_i64_expr_to_rax(value, code, program);
                let slot_offset = i64_slot_offset(program, *slot);
                code.extend_from_slice(&[0x48, 0x89, 0x85]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
            }
            Operation::StoreF32 { slot, value } => {
                emit_f32_expr_to_xmm0(value, code, program);
                let slot_offset = f32_slot_offset(program, *slot);
                code.extend_from_slice(&[0xf3, 0x0f, 0x11, 0x85]);
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
            Operation::ArrayAllocDynamic { array_slot, len } => {
                let element_size = array_element_size(array_allocation(program, *array_slot));
                code.extend_from_slice(&[0x31, 0xff]);
                emit_i32_expr_to_eax(len, code, program);
                if element_size != 1 {
                    code.extend_from_slice(&[0x6b, 0xc0, element_size as u8]);
                }
                code.extend_from_slice(&[0x89, 0xc6]);
                code.extend_from_slice(&[0xba, 0x03, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x41, 0xba, 0x22, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x41, 0xb8, 0xff, 0xff, 0xff, 0xff]);
                code.extend_from_slice(&[0x45, 0x31, 0xc9]);
                code.extend_from_slice(&[0xb8, 0x09, 0x00, 0x00, 0x00]);
                code.extend_from_slice(&[0x0f, 0x05]);
                let slot_offset = array_slot_offset(program, *array_slot);
                code.extend_from_slice(&[0x48, 0x89, 0x85]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                emit_i32_expr_to_eax(len, code, program);
                let len_offset = array_len_offset(program, *array_slot);
                code.extend_from_slice(&[0x89, 0x85]);
                code.extend_from_slice(&len_offset.to_le_bytes());
            }
            Operation::ArrayReplace {
                dest_slot,
                source_slot,
            } => {
                let source_offset = array_slot_offset(program, *source_slot);
                let dest_offset = array_slot_offset(program, *dest_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0x85]);
                code.extend_from_slice(&source_offset.to_le_bytes());
                code.extend_from_slice(&[0x48, 0x89, 0x85]);
                code.extend_from_slice(&dest_offset.to_le_bytes());

                let source_len_offset = array_len_offset(program, *source_slot);
                let dest_len_offset = array_len_offset(program, *dest_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0x85]);
                code.extend_from_slice(&source_len_offset.to_le_bytes());
                code.extend_from_slice(&[0x48, 0x89, 0x85]);
                code.extend_from_slice(&dest_len_offset.to_le_bytes());
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
            Operation::HeapStoreI64 {
                heap_slot,
                byte_offset,
                value,
            } => {
                emit_i64_expr_to_rax(value, code, program);
                code.extend_from_slice(&[0x48, 0x89, 0xc2]);
                let slot_offset = heap_slot_offset(*heap_slot);
                code.extend_from_slice(&[0x48, 0x8b, 0x9d]);
                code.extend_from_slice(&slot_offset.to_le_bytes());
                code.extend_from_slice(&[0x48, 0x89, 0x93]);
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
                path,
                flags,
                mode,
                result_slot,
            } => {
                match path {
                    OpenPath::PathBuf(array_slot) => {
                        let slot_offset = array_slot_offset(program, *array_slot);
                        code.extend_from_slice(&[0x48, 0x8b, 0xbd]);
                        code.extend_from_slice(&slot_offset.to_le_bytes());
                    }
                }
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
            Operation::PathBufPush { path_slot, source } => {
                emit_pathbuf_push(*path_slot, source, code, program, addresses)
            }
            Operation::PathBufPop { path_slot } => emit_pathbuf_pop(*path_slot, code, program),
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
                let false_patch_ats = emit_condition_false_jumps(condition, code, program);
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
                    patch_jumps_to(&false_patch_ats, end, code);
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
                    patch_jumps_to(&false_patch_ats, else_start, code);
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
            Operation::ArraySetI64 {
                array_slot,
                index,
                value,
            } => emit_i64_array_set(*array_slot, index, value, code, program),
            Operation::ArraySetF32 {
                array_slot,
                index,
                value,
            } => emit_f32_array_set(*array_slot, index, value, code, program),
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

fn emit_condition_false_jumps(
    condition: &ConditionExpr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) -> Vec<usize> {
    match condition {
        ConditionExpr::Literal(true) => Vec::new(),
        ConditionExpr::Literal(false) => {
            code.push(0xe9);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::And(lhs, rhs) => {
            let mut patches = emit_condition_false_jumps(lhs, code, program);
            patches.extend(emit_condition_false_jumps(rhs, code, program));
            patches
        }
        ConditionExpr::Or(lhs, rhs) => {
            let true_patches = emit_condition_true_jumps(lhs, code, program);
            let false_patches = emit_condition_false_jumps(rhs, code, program);
            patch_jumps_to_current(&true_patches, code);
            false_patches
        }
        ConditionExpr::Not(inner) => emit_condition_true_jumps(inner, code, program),
        ConditionExpr::I32 { kind, lhs, rhs } => {
            emit_i32_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_i32_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_false(*kind, false, code);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::I64 { kind, lhs, rhs } => {
            emit_i64_expr_to_rax(lhs, code, program);
            code.push(0x50);
            emit_i64_expr_to_rax(rhs, code, program);
            code.extend_from_slice(&[0x48, 0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x48, 0x39, 0xc8]);
            emit_jcc_false(*kind, false, code);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::F32 { kind, lhs, rhs } => {
            emit_f32_comparison(lhs, rhs, code, program);
            emit_f32_jcc_false(*kind, code);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::U8 { kind, lhs, rhs } => {
            emit_u8_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_u8_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_false(*kind, true, code);
            vec![emit_jump_placeholder(code)]
        }
    }
}

fn emit_condition_true_jumps(
    condition: &ConditionExpr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) -> Vec<usize> {
    match condition {
        ConditionExpr::Literal(true) => {
            code.push(0xe9);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::Literal(false) => Vec::new(),
        ConditionExpr::And(lhs, rhs) => {
            let false_patches = emit_condition_false_jumps(lhs, code, program);
            let true_patches = emit_condition_true_jumps(rhs, code, program);
            patch_jumps_to_current(&false_patches, code);
            true_patches
        }
        ConditionExpr::Or(lhs, rhs) => {
            let mut patches = emit_condition_true_jumps(lhs, code, program);
            patches.extend(emit_condition_true_jumps(rhs, code, program));
            patches
        }
        ConditionExpr::Not(inner) => emit_condition_false_jumps(inner, code, program),
        ConditionExpr::I32 { kind, lhs, rhs } => {
            emit_i32_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_i32_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_true(*kind, false, code);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::I64 { kind, lhs, rhs } => {
            emit_i64_expr_to_rax(lhs, code, program);
            code.push(0x50);
            emit_i64_expr_to_rax(rhs, code, program);
            code.extend_from_slice(&[0x48, 0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x48, 0x39, 0xc8]);
            emit_jcc_true(*kind, false, code);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::F32 { kind, lhs, rhs } => {
            emit_f32_comparison(lhs, rhs, code, program);
            emit_f32_jcc_true(*kind, code);
            vec![emit_jump_placeholder(code)]
        }
        ConditionExpr::U8 { kind, lhs, rhs } => {
            emit_u8_expr_to_eax(lhs, code, program);
            code.push(0x50);
            emit_u8_expr_to_eax(rhs, code, program);
            code.extend_from_slice(&[0x89, 0xc1]);
            code.push(0x58);
            code.extend_from_slice(&[0x39, 0xc8]);
            emit_jcc_true(*kind, true, code);
            vec![emit_jump_placeholder(code)]
        }
    }
}

fn emit_jump_placeholder(code: &mut Vec<u8>) -> usize {
    let patch_at = code.len();
    code.extend_from_slice(&0i32.to_le_bytes());
    patch_at
}

fn patch_jumps_to_current(patch_ats: &[usize], code: &mut [u8]) {
    patch_jumps_to(patch_ats, code.len(), code);
}

fn patch_jumps_to(patch_ats: &[usize], target: usize, code: &mut [u8]) {
    for patch_at in patch_ats {
        let jump_len = target as i32 - (*patch_at as i32 + 4);
        code[*patch_at..*patch_at + 4].copy_from_slice(&jump_len.to_le_bytes());
    }
}

fn emit_jcc_false(kind: ComparisonKind, unsigned: bool, code: &mut Vec<u8>) {
    code.extend_from_slice(&[0x0f, false_jump_opcode(kind, unsigned)]);
}

fn emit_jcc_true(kind: ComparisonKind, unsigned: bool, code: &mut Vec<u8>) {
    code.extend_from_slice(&[0x0f, true_jump_opcode(kind, unsigned)]);
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

fn true_jump_opcode(kind: ComparisonKind, unsigned: bool) -> u8 {
    match (kind, unsigned) {
        (ComparisonKind::Eq, _) => 0x84,
        (ComparisonKind::Lte, false) => 0x8e,
        (ComparisonKind::Lt, false) => 0x8c,
        (ComparisonKind::Gte, false) => 0x8d,
        (ComparisonKind::Gt, false) => 0x8f,
        (ComparisonKind::Lte, true) => 0x86,
        (ComparisonKind::Lt, true) => 0x82,
        (ComparisonKind::Gte, true) => 0x83,
        (ComparisonKind::Gt, true) => 0x87,
    }
}

fn emit_exit_code_expr_to_eax(expr: &ExitCodeExpr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        ExitCodeExpr::I32(expr) => emit_i32_expr_to_eax(expr, code, program),
        ExitCodeExpr::I64(expr) => emit_i64_expr_to_rax(expr, code, program),
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
        I32Expr::FromU8(value) => emit_u8_expr_to_eax(value, code, program),
        I32Expr::FromI64(value) => emit_i64_expr_to_rax(value, code, program),
        I32Expr::FromF32(value) => {
            emit_f32_expr_to_xmm0(value, code, program);
            code.extend_from_slice(&[0xf3, 0x0f, 0x2c, 0xc0]);
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
        I32Expr::ArrayLen { array_slot } => emit_array_len(*array_slot, code, program),
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

fn emit_i64_expr_to_rax(expr: &I64Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        I64Expr::Literal(value) => {
            code.extend_from_slice(&[0x48, 0xb8]);
            code.extend_from_slice(&value.to_le_bytes());
        }
        I64Expr::Local(slot) => {
            let slot_offset = i64_slot_offset(program, *slot);
            code.extend_from_slice(&[0x48, 0x8b, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
        }
        I64Expr::FromI32(value) => {
            emit_i32_expr_to_eax(value, code, program);
            code.extend_from_slice(&[0x48, 0x98]);
        }
        I64Expr::FromU8(value) => emit_u8_expr_to_eax(value, code, program),
        I64Expr::FromF32(value) => {
            emit_f32_expr_to_xmm0(value, code, program);
            code.extend_from_slice(&[0xf3, 0x48, 0x0f, 0x2c, 0xc0]);
        }
        I64Expr::Add(lhs, rhs) => {
            emit_i64_binary_expr(lhs, rhs, code, program, &[0x48, 0x01, 0xc8])
        }
        I64Expr::Sub(lhs, rhs) => {
            emit_i64_binary_expr(lhs, rhs, code, program, &[0x48, 0x29, 0xc8])
        }
        I64Expr::Mul(lhs, rhs) => {
            emit_i64_binary_expr(lhs, rhs, code, program, &[0x48, 0x0f, 0xaf, 0xc1])
        }
        I64Expr::Div(lhs, rhs) => emit_i64_div_mod_expr(lhs, rhs, code, program, false),
        I64Expr::Mod(lhs, rhs) => emit_i64_div_mod_expr(lhs, rhs, code, program, true),
        I64Expr::ArrayGet { array_slot, index } => {
            emit_i64_array_get(*array_slot, index, code, program)
        }
        I64Expr::ArrayLen { array_slot } => emit_i64_array_len(*array_slot, code, program),
    }
}

fn emit_i64_binary_expr(
    lhs: &I64Expr,
    rhs: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    opcode: &[u8],
) {
    emit_i64_expr_to_rax(lhs, code, program);
    code.push(0x50);
    emit_i64_expr_to_rax(rhs, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    code.push(0x58);
    code.extend_from_slice(opcode);
}

fn emit_i64_div_mod_expr(
    lhs: &I64Expr,
    rhs: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    modulo: bool,
) {
    emit_i64_expr_to_rax(lhs, code, program);
    code.push(0x50);
    emit_i64_expr_to_rax(rhs, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    code.push(0x58);
    code.extend_from_slice(&[0x48, 0x99]);
    code.extend_from_slice(&[0x48, 0xf7, 0xf9]);
    if modulo {
        code.extend_from_slice(&[0x48, 0x89, 0xd0]);
    }
}

fn emit_f32_expr_to_xmm0(expr: &F32Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        F32Expr::LiteralBits(bits) => {
            code.push(0xb8);
            code.extend_from_slice(&bits.to_le_bytes());
            code.extend_from_slice(&[0x66, 0x0f, 0x6e, 0xc0]);
        }
        F32Expr::Local(slot) => {
            let slot_offset = f32_slot_offset(program, *slot);
            code.extend_from_slice(&[0xf3, 0x0f, 0x10, 0x85]);
            code.extend_from_slice(&slot_offset.to_le_bytes());
        }
        F32Expr::FromI32(value) => {
            emit_i32_expr_to_eax(value, code, program);
            code.extend_from_slice(&[0xf3, 0x0f, 0x2a, 0xc0]);
        }
        F32Expr::FromI64(value) => {
            emit_i64_expr_to_rax(value, code, program);
            code.extend_from_slice(&[0xf3, 0x48, 0x0f, 0x2a, 0xc0]);
        }
        F32Expr::FromU8(value) => {
            emit_u8_expr_to_eax(value, code, program);
            code.extend_from_slice(&[0xf3, 0x0f, 0x2a, 0xc0]);
        }
        F32Expr::Add(lhs, rhs) => {
            emit_f32_binary_expr(lhs, rhs, code, program, &[0xf3, 0x0f, 0x58, 0xc1])
        }
        F32Expr::Sub(lhs, rhs) => {
            emit_f32_binary_expr(lhs, rhs, code, program, &[0xf3, 0x0f, 0x5c, 0xc1])
        }
        F32Expr::Mul(lhs, rhs) => {
            emit_f32_binary_expr(lhs, rhs, code, program, &[0xf3, 0x0f, 0x59, 0xc1])
        }
        F32Expr::Div(lhs, rhs) => {
            emit_f32_binary_expr(lhs, rhs, code, program, &[0xf3, 0x0f, 0x5e, 0xc1])
        }
        F32Expr::ArrayGet { array_slot, index } => {
            emit_f32_array_get(*array_slot, index, code, program)
        }
    }
}

fn emit_f32_binary_expr(
    lhs: &F32Expr,
    rhs: &F32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    opcode: &[u8],
) {
    emit_f32_expr_to_xmm0(lhs, code, program);
    code.extend_from_slice(&[0x48, 0x83, 0xec, 0x08]);
    code.extend_from_slice(&[0xf3, 0x0f, 0x11, 0x04, 0x24]);
    emit_f32_expr_to_xmm0(rhs, code, program);
    code.extend_from_slice(&[0xf3, 0x0f, 0x10, 0xc8]);
    code.extend_from_slice(&[0xf3, 0x0f, 0x10, 0x04, 0x24]);
    code.extend_from_slice(&[0x48, 0x83, 0xc4, 0x08]);
    code.extend_from_slice(opcode);
}

fn emit_f32_comparison(lhs: &F32Expr, rhs: &F32Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    emit_f32_expr_to_xmm0(lhs, code, program);
    code.extend_from_slice(&[0x48, 0x83, 0xec, 0x08]);
    code.extend_from_slice(&[0xf3, 0x0f, 0x11, 0x04, 0x24]);
    emit_f32_expr_to_xmm0(rhs, code, program);
    code.extend_from_slice(&[0xf3, 0x0f, 0x10, 0xc8]);
    code.extend_from_slice(&[0xf3, 0x0f, 0x10, 0x04, 0x24]);
    code.extend_from_slice(&[0x48, 0x83, 0xc4, 0x08]);
    code.extend_from_slice(&[0x0f, 0x2e, 0xc1]);
}

fn emit_f32_jcc_false(kind: ComparisonKind, code: &mut Vec<u8>) {
    code.extend_from_slice(&[0x0f, f32_false_jump_opcode(kind)]);
}

fn emit_f32_jcc_true(kind: ComparisonKind, code: &mut Vec<u8>) {
    code.extend_from_slice(&[0x0f, f32_true_jump_opcode(kind)]);
}

fn f32_false_jump_opcode(kind: ComparisonKind) -> u8 {
    match kind {
        ComparisonKind::Eq => 0x85,
        ComparisonKind::Lte => 0x87,
        ComparisonKind::Lt => 0x83,
        ComparisonKind::Gte => 0x82,
        ComparisonKind::Gt => 0x86,
    }
}

fn f32_true_jump_opcode(kind: ComparisonKind) -> u8 {
    match kind {
        ComparisonKind::Eq => 0x84,
        ComparisonKind::Lte => 0x86,
        ComparisonKind::Lt => 0x82,
        ComparisonKind::Gte => 0x83,
        ComparisonKind::Gt => 0x87,
    }
}

fn emit_u8_expr_to_eax(expr: &U8Expr, code: &mut Vec<u8>, program: &LoweredProgram) {
    match expr {
        U8Expr::Literal(value) => {
            code.push(0xb8);
            code.extend_from_slice(&u32::from(*value).to_le_bytes());
        }
        U8Expr::FromI32(value) => {
            emit_i32_expr_to_eax(value, code, program);
            code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
        }
        U8Expr::FromI64(value) => {
            emit_i64_expr_to_rax(value, code, program);
            code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
        }
        U8Expr::FromF32(value) => {
            emit_f32_expr_to_xmm0(value, code, program);
            code.extend_from_slice(&[0xf3, 0x0f, 0x2c, 0xc0]);
            code.extend_from_slice(&[0x0f, 0xb6, 0xc0]);
        }
        U8Expr::Add(lhs, rhs) => emit_u8_binary_expr(lhs, rhs, code, program, &[0x00, 0xc8]),
        U8Expr::Sub(lhs, rhs) => emit_u8_binary_expr(lhs, rhs, code, program, &[0x28, 0xc8]),
        U8Expr::Mul(lhs, rhs) => emit_u8_mul_expr(lhs, rhs, code, program),
        U8Expr::Div(lhs, rhs) => emit_u8_div_mod_expr(lhs, rhs, code, program, false),
        U8Expr::Mod(lhs, rhs) => emit_u8_div_mod_expr(lhs, rhs, code, program, true),
        U8Expr::RuntimeArgGet { arg_index, index } => {
            emit_runtime_arg_u8_get(arg_index, index, code, program, ARGV_GLOBAL_ADDRESS)
        }
        U8Expr::StaticDataGet { data_index, index } => {
            emit_static_data_u8_get(*data_index, index, code, program)
        }
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

fn emit_static_data_u8_get(
    data_index: usize,
    index: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
) {
    let data_address = static_data_address(program, data_index);
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0xbb]);
    code.extend_from_slice(&data_address.to_le_bytes());
    code.extend_from_slice(&[0x0f, 0xb6, 0x04, 0x03]);
}

fn emit_argv_global_init(code: &mut Vec<u8>, argv_global_address: u64, entry_abi: EntryAbi) {
    match entry_abi {
        EntryAbi::KernelStart => {
            code.extend_from_slice(&[0x48, 0x8d, 0x44, 0x24, 0x08]);
            code.extend_from_slice(&[0x48, 0xa3]);
            code.extend_from_slice(&argv_global_address.to_le_bytes());
        }
        EntryAbi::LibcMain => {
            code.extend_from_slice(&[0x48, 0x89, 0xf0]);
            code.extend_from_slice(&[0x48, 0xa3]);
            code.extend_from_slice(&argv_global_address.to_le_bytes());
        }
    }
}

fn emit_runtime_arg_ptr_to_rax(
    arg_index: &I32Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    argv_global_address: u64,
) {
    emit_i32_expr_to_eax(arg_index, code, program);
    code.extend_from_slice(&[0x48, 0x98]);
    code.extend_from_slice(&[0x48, 0xc1, 0xe0, 0x03]);
    code.extend_from_slice(&[0x49, 0x89, 0xc3]);
    code.extend_from_slice(&[0x48, 0xa1]);
    code.extend_from_slice(&argv_global_address.to_le_bytes());
    code.extend_from_slice(&[0x4c, 0x01, 0xd8]);
    code.extend_from_slice(&[0x48, 0x8b, 0x00]);
}

fn emit_runtime_arg_u8_get(
    arg_index: &I32Expr,
    index: &I64Expr,
    code: &mut Vec<u8>,
    program: &LoweredProgram,
    argv_global_address: u64,
) {
    emit_runtime_arg_ptr_to_rax(arg_index, code, program, argv_global_address);
    code.push(0x50);
    emit_i64_expr_to_rax(index, code, program);
    code.extend_from_slice(&[0x48, 0x89, 0xc1]);
    code.push(0x58);
    code.extend_from_slice(&[0x0f, 0xb6, 0x04, 0x08]);
}

fn stack_frame_size(program: &LoweredProgram) -> usize {
    let array_descriptor_bytes = array_descriptor_bytes(program);
    let pointer_bytes = array_descriptor_bytes + program.heap_slots * 8;
    let i32_slot_bytes = program.i32_slots * 4;
    let i64_slot_bytes = program.i64_slots * 8;
    let f32_slot_bytes = program.f32_slots * 4;
    let array_bytes: usize = program.arrays.iter().map(array_storage_size).sum();
    pointer_bytes + i32_slot_bytes + i64_slot_bytes + f32_slot_bytes + array_bytes
}

fn heap_slot_offset(slot: usize) -> i32 {
    -8 * (slot as i32 + 1)
}

fn array_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let mut offset = (program.heap_slots * 8) as i32;
    for array in &program.arrays {
        offset += array_descriptor_size(array) as i32;
        if array.slot == slot {
            return -offset;
        }
    }
    panic!("unknown array slot {slot}");
}

fn array_len_offset(program: &LoweredProgram, slot: usize) -> i32 {
    array_slot_offset(program, slot) + 8
}

fn array_logical_len_offset(program: &LoweredProgram, slot: usize) -> i32 {
    array_len_offset(program, slot)
}

fn array_data_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let pointer_bytes = pointer_bytes(program);
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let i64_slot_bytes = (program.i64_slots * 8) as i32;
    let f32_slot_bytes = (program.f32_slots * 4) as i32;
    let mut offset = pointer_bytes + i32_slot_bytes + i64_slot_bytes + f32_slot_bytes;
    for array in &program.arrays {
        offset += array_storage_size(array) as i32;
        if array.slot == slot {
            return -offset;
        }
    }
    panic!("unknown array slot {slot}");
}

fn static_data_address(program: &LoweredProgram, data_index: usize) -> u64 {
    data_addresses(program, DATA_VIRTUAL_ADDRESS)[data_index]
}

fn i32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    -(pointer_bytes(program) + 4 * (slot as i32 + 1))
}

fn i64_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    -(pointer_bytes(program) + i32_slot_bytes + 8 * (slot as i32 + 1))
}

fn f32_slot_offset(program: &LoweredProgram, slot: usize) -> i32 {
    let i32_slot_bytes = (program.i32_slots * 4) as i32;
    let i64_slot_bytes = (program.i64_slots * 8) as i32;
    -(pointer_bytes(program) + i32_slot_bytes + i64_slot_bytes + 4 * (slot as i32 + 1))
}

fn emit_array_initializers(program: &LoweredProgram, code: &mut Vec<u8>) {
    for array in &program.arrays {
        let slot_offset = array_slot_offset(program, array.slot);
        let data_offset = array_data_offset(program, array.slot);
        code.extend_from_slice(&[0x48, 0x8d, 0x85]);
        code.extend_from_slice(&data_offset.to_le_bytes());
        code.extend_from_slice(&[0x48, 0x89, 0x85]);
        code.extend_from_slice(&slot_offset.to_le_bytes());
        let len_offset = array_len_offset(program, array.slot);
        code.extend_from_slice(&[0x48, 0xc7, 0x85]);
        code.extend_from_slice(&len_offset.to_le_bytes());
        code.extend_from_slice(&(array.len as u32).to_le_bytes());
    }
}

fn array_descriptor_size(array: &ArrayAllocation) -> usize {
    match array.kind {
        ArrayKind::Fixed => 16,
        ArrayKind::Dynamic => 16,
    }
}

fn array_descriptor_bytes(program: &LoweredProgram) -> usize {
    program.arrays.iter().map(array_descriptor_size).sum()
}

fn pointer_bytes(program: &LoweredProgram) -> i32 {
    (program.heap_slots * 8 + array_descriptor_bytes(program)) as i32
}

fn array_storage_size(array: &ArrayAllocation) -> usize {
    let element_size = array_element_size(array);
    array.len * element_size
}

fn array_element_size(array: &ArrayAllocation) -> usize {
    match array.element_type {
        ArrayElementType::I32 => 4,
        ArrayElementType::I64 => 8,
        ArrayElementType::F32 => 4,
        ArrayElementType::U8 => 1,
    }
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

fn emit_i64_array_set(
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

fn emit_f32_array_set(
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

fn emit_array_len(array_slot: usize, code: &mut Vec<u8>, program: &LoweredProgram) {
    let len_offset = array_logical_len_offset(program, array_slot);
    code.extend_from_slice(&[0x8b, 0x85]);
    code.extend_from_slice(&len_offset.to_le_bytes());
}

fn emit_i64_array_len(array_slot: usize, code: &mut Vec<u8>, program: &LoweredProgram) {
    let len_offset = array_logical_len_offset(program, array_slot);
    code.extend_from_slice(&[0x48, 0x8b, 0x85]);
    code.extend_from_slice(&len_offset.to_le_bytes());
}

fn emit_u8_array_set(
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

fn emit_pathbuf_push(
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

fn emit_pathbuf_pop(path_slot: usize, code: &mut Vec<u8>, program: &LoweredProgram) {
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

fn emit_array_get(
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

fn emit_i64_array_get(
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

fn emit_f32_array_get(
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

fn emit_u8_array_get(
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
