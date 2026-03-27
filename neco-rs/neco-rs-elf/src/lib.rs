#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElfClass {
    Elf64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElfDataEncoding {
    LittleEndian,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElfOsAbi {
    SystemV,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElfType {
    Executable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElfMachine {
    X86_64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SegmentFlags {
    readable: bool,
    writable: bool,
    executable: bool,
}

impl SegmentFlags {
    pub const READ_ONLY: Self = Self {
        readable: true,
        writable: false,
        executable: false,
    };

    pub const READ_EXECUTE: Self = Self {
        readable: true,
        writable: false,
        executable: true,
    };

    fn to_elf_bits(self) -> u32 {
        let mut bits = 0;
        if self.executable {
            bits |= 0x1;
        }
        if self.writable {
            bits |= 0x2;
        }
        if self.readable {
            bits |= 0x4;
        }
        bits
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadSegment {
    virtual_address: u64,
    alignment: u64,
    flags: SegmentFlags,
    data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Elf64Executable {
    data_encoding: ElfDataEncoding,
    os_abi: ElfOsAbi,
    machine: ElfMachine,
    entry_virtual_address: u64,
    load_segments: Vec<LoadSegment>,
}

impl LoadSegment {
    pub fn new(virtual_address: u64, alignment: u64, flags: SegmentFlags, data: Vec<u8>) -> Self {
        Self {
            virtual_address,
            alignment,
            flags,
            data,
        }
    }
}

impl Elf64Executable {
    pub fn new(machine: ElfMachine, entry_virtual_address: u64) -> Self {
        Self {
            data_encoding: ElfDataEncoding::LittleEndian,
            os_abi: ElfOsAbi::SystemV,
            machine,
            entry_virtual_address,
            load_segments: Vec::new(),
        }
    }

    pub fn add_load_segment(&mut self, segment: LoadSegment) {
        self.load_segments.push(segment);
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        const ELF_HEADER_SIZE: usize = 64;
        const PROGRAM_HEADER_SIZE: usize = 56;

        let phoff = ELF_HEADER_SIZE;
        let header_region_size = ELF_HEADER_SIZE + PROGRAM_HEADER_SIZE * self.load_segments.len();

        let mut segment_offsets = Vec::with_capacity(self.load_segments.len());
        let mut file_size = header_region_size;

        for segment in &self.load_segments {
            file_size = align_usize(file_size, segment.alignment as usize);
            segment_offsets.push(file_size);
            file_size += segment.data.len();
        }

        let mut elf = vec![0_u8; file_size];

        elf[0..4].copy_from_slice(b"\x7FELF");
        elf[4] = elf_class_to_byte(ElfClass::Elf64);
        elf[5] = data_encoding_to_byte(self.data_encoding);
        elf[6] = 1;
        elf[7] = os_abi_to_byte(self.os_abi);
        elf[8..16].fill(0);

        write_u16(&mut elf, 16, elf_type_to_u16(ElfType::Executable));
        write_u16(&mut elf, 18, machine_to_u16(self.machine));
        write_u32(&mut elf, 20, 1);
        write_u64(&mut elf, 24, self.entry_virtual_address);
        write_u64(&mut elf, 32, phoff as u64);
        write_u64(&mut elf, 40, 0);
        write_u32(&mut elf, 48, 0);
        write_u16(&mut elf, 52, ELF_HEADER_SIZE as u16);
        write_u16(&mut elf, 54, PROGRAM_HEADER_SIZE as u16);
        write_u16(&mut elf, 56, self.load_segments.len() as u16);
        write_u16(&mut elf, 58, 0);
        write_u16(&mut elf, 60, 0);
        write_u16(&mut elf, 62, 0);

        for (index, segment) in self.load_segments.iter().enumerate() {
            let offset = segment_offsets[index];
            let phdr = phoff + index * PROGRAM_HEADER_SIZE;

            write_u32(&mut elf, phdr, 1);
            write_u32(&mut elf, phdr + 4, segment.flags.to_elf_bits());
            write_u64(&mut elf, phdr + 8, offset as u64);
            write_u64(&mut elf, phdr + 16, segment.virtual_address);
            write_u64(&mut elf, phdr + 24, segment.virtual_address);
            write_u64(&mut elf, phdr + 32, segment.data.len() as u64);
            write_u64(&mut elf, phdr + 40, segment.data.len() as u64);
            write_u64(&mut elf, phdr + 48, segment.alignment);

            elf[offset..offset + segment.data.len()].copy_from_slice(&segment.data);
        }

        elf
    }
}

fn align_usize(value: usize, alignment: usize) -> usize {
    if alignment <= 1 {
        return value;
    }
    let remainder = value % alignment;
    if remainder == 0 {
        value
    } else {
        value + (alignment - remainder)
    }
}

fn elf_class_to_byte(class: ElfClass) -> u8 {
    match class {
        ElfClass::Elf64 => 2,
    }
}

fn data_encoding_to_byte(encoding: ElfDataEncoding) -> u8 {
    match encoding {
        ElfDataEncoding::LittleEndian => 1,
    }
}

fn os_abi_to_byte(os_abi: ElfOsAbi) -> u8 {
    match os_abi {
        ElfOsAbi::SystemV => 0,
    }
}

fn elf_type_to_u16(elf_type: ElfType) -> u16 {
    match elf_type {
        ElfType::Executable => 2,
    }
}

fn machine_to_u16(machine: ElfMachine) -> u16 {
    match machine {
        ElfMachine::X86_64 => 62,
    }
}

fn write_u16(buffer: &mut [u8], offset: usize, value: u16) {
    buffer[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
}

fn write_u32(buffer: &mut [u8], offset: usize, value: u32) {
    buffer[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn write_u64(buffer: &mut [u8], offset: usize, value: u64) {
    buffer[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::{Elf64Executable, ElfMachine, LoadSegment, SegmentFlags};

    #[test]
    fn serializes_single_load_segment_executable() {
        let mut elf = Elf64Executable::new(ElfMachine::X86_64, 0x401000);
        elf.add_load_segment(LoadSegment::new(
            0x401000,
            0x1000,
            SegmentFlags::READ_EXECUTE,
            vec![
                0xb8, 0x3c, 0x00, 0x00, 0x00, 0xbf, 42, 0x00, 0x00, 0x00, 0x0f, 0x05,
            ],
        ));

        let bytes = elf.to_bytes();

        assert_eq!(&bytes[0..4], b"\x7FELF");
        assert_eq!(&bytes[0x1000..0x1005], &[0xb8, 0x3c, 0x00, 0x00, 0x00]);
        assert_eq!(&bytes[0x1005..0x100a], &[0xbf, 42, 0x00, 0x00, 0x00]);
        assert_eq!(&bytes[0x100a..0x100c], &[0x0f, 0x05]);
    }
}
