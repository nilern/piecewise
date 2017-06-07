use block;
use block::Block;
use block_arr::BlockArr;
use arena;
use arena_arr::ArenaArr;

pub enum Descriptor {
    Block(Block),
    BlockArr(BlockArr),
    ArenaArr(ArenaArr)
}

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
}

#[cfg(test)]
mod tests {
    use super::Descriptor;
    use std::mem;

    #[test]
    fn descriptor_size() {
        assert!(mem::size_of::<Descriptor>() <= Descriptor::SIZE);
    }
}
