#[derive(Debug, Clone, Copy)]
pub struct SrcPos {
    pub index: usize,
    pub line: usize,
    pub col: usize
}

impl Default for SrcPos {
    fn default() -> SrcPos {
        SrcPos { index: 0, line: 1, col: 1 }
    }
}
