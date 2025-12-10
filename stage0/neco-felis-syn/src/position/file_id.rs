#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(usize);

pub struct FileIdGenerator {
    next_id: usize,
}

impl Default for FileIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl FileIdGenerator {
    pub fn new() -> FileIdGenerator {
        FileIdGenerator { next_id: 0 }
    }

    pub fn generate_file_id(&mut self) -> FileId {
        let res = FileId(self.next_id);
        self.next_id += 1;
        res
    }
}
