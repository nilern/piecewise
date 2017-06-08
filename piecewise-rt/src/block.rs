
/// A number such that SIZE = 1^SHIFT
pub const SHIFT: usize = 12;

/// A block is SIZE bytes of memory
pub const SIZE: usize = 1 << SHIFT;

pub enum Block {}

impl Block {
    pub fn sweep(&mut self) {
        unimplemented!()
    }
}
