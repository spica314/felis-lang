use crate::FileId;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    file_id: FileId,
    line: usize,
    column: usize,
}

impl Pos {
    pub fn new(file_id: FileId, line: usize, column: usize) -> Pos {
        Pos {
            file_id,
            line,
            column,
        }
    }
}
