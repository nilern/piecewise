use arena;
use block;

/// A number such that SIZE = 1^SHIFT
pub const SHIFT: usize = 12;

/// A block is SIZE bytes of memory
pub const SIZE: usize = 1 << SHIFT;

pub enum Descriptor {}

impl Descriptor {
    #[cfg(target_pointer_width = "64")]
    const SHIFT: usize = 6;

    pub const SIZE: usize = 1 << Descriptor::SHIFT;

    pub const MASK: usize = Descriptor::SIZE - 1;

    fn from_ptr(ptr: *const ()) -> *mut Descriptor {
        let index = (ptr as usize & arena::MASK) >> block::SHIFT;
        let byte_index = index >> block::SHIFT << Descriptor::SHIFT;
        (byte_index as usize & !arena::MASK | byte_index) as _
    }

    pub fn sweep(&mut self) {
        unimplemented!()
    }
}
