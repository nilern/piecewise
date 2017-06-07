use std::mem::transmute;
use intrusive_collections::{UnsafeRef, LinkedListLink};

use arena;
use block;

pub enum Descriptor {
    BlockArr(BlockArr),
    Block(Block),
    ArenaArr(ArenaArr)
}

impl Descriptor {
    #[cfg(target_pointer_width = "64")]
    const SHIFT: usize = 6;

    const SIZE: usize = 1 << Descriptor::SHIFT;

    const MASK: usize = Descriptor::SIZE - 1;

    fn from_ptr(ptr: *const ()) -> *mut Descriptor {
        let index = (ptr as usize & arena::MASK) >> block::SHIFT;
        let byte_index = index >> block::SHIFT << Descriptor::SHIFT;
        (byte_index as usize & !arena::MASK | byte_index) as _
    }

    pub fn len(&self) -> usize {
        if let &Descriptor::BlockArr(BlockArr::FreeListNode(ref node)) = self {
            node.len()
        } else {
            unimplemented!()
        }
    }

    pub fn split_off(&mut self, n: usize) -> *mut Descriptor {
        unimplemented!()
    }
}

pub enum BlockArr {
    FreeListNode(FreeListNode)
}

pub struct FreeListNode {
    link: LinkedListLink,
    len: usize
}

intrusive_adapter!(pub FreeAdapter = UnsafeRef<FreeListNode>: FreeListNode { link: LinkedListLink });

impl FreeListNode {
    pub fn upcast(&self) -> *mut Descriptor {
        (unsafe { transmute::<_, usize>(self) } & !Descriptor::MASK) as _
    }

    pub fn len(&self) -> usize { self.len }
}

pub enum Block {

}

pub enum ArenaArr {
    FreeListNode(FreeListNode)
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
