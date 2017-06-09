use std::mem;
use std::mem::transmute;
use std::ptr;
use intrusive_collections::{UnsafeRef, LinkedList, LinkedListLink};

use block;
use arena;
use arena_arr;
use util::IntLog2;

pub enum Descriptor {
    FreeListNode(FreeListNode)
}

impl Descriptor {
    #[cfg(target_pointer_width = "64")]
    const SHIFT: usize = 6;

    pub const SIZE: usize = 1 << Descriptor::SHIFT;

    pub const MASK: usize = Descriptor::SIZE - 1;
}

pub struct FreeListNode {
    link: LinkedListLink,
    len: usize
}

intrusive_adapter!(pub FreeAdapter = UnsafeRef<FreeListNode>:
                   FreeListNode { link: LinkedListLink });

impl FreeListNode {
    fn upcast(&self) -> *mut Descriptor {
        (unsafe { transmute::<_, usize>(self) } & !Descriptor::MASK) as _
    }

    fn len(&self) -> usize { self.len }

    fn split_off(&mut self, n: usize) -> *mut FreeListNode {
        let ptr = self.offset(self.len() - n);
        unsafe { FreeListNode::init(ptr, n) }
    }

    fn offset(&self, n: usize) -> *mut () {
        (unsafe { transmute::<_, usize>(self) } + n*Descriptor::SIZE) as _
    }

    unsafe fn init(descr: *mut (), len: usize) -> *mut FreeListNode {
        let descr = descr as _;
        ptr::write(descr, Descriptor::FreeListNode(FreeListNode {
            link: LinkedListLink::default(),
            len: len
        }));

        let Descriptor::FreeListNode(ref node) = *descr;
        transmute(node)
    }
}

pub struct Active {
    link: LinkedListLink,
    len: usize
}

intrusive_adapter!(pub ActiveAdapter = UnsafeRef<Active>: Active { link: LinkedListLink });

impl Active {
    pub fn len(&self) -> usize { self.len }

    pub fn blocks(&self) -> Blocks {
        Blocks {
            start: self.upcast(),
            max: self.len()
        }
    }

    fn upcast(&self) -> *mut Descriptor {
        unimplemented!()
    }
}

pub struct Blocks {
    start: *mut Descriptor,
    max: usize
}

impl Iterator for Blocks {
    type Item = *mut block::Descriptor;

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

/// Block allocator (allocates Descriptor:s)
pub struct Allocator {
    arena_allocator: arena_arr::Allocator,
    blockarr_lists: [LinkedList<FreeAdapter>; Allocator::NLISTS]
}

impl Allocator {
    const NLISTS: usize = arena::SHIFT - block::SHIFT;

    /// Create a new block allocator.
    pub fn new() -> Allocator {
        // The array initialization is nasty. Hopefully there will be a better way in future Rust.

        let mut res = Allocator {
            arena_allocator: arena_arr::Allocator::new(),
            blockarr_lists: unsafe { mem::uninitialized::<[_; Allocator::NLISTS]>() }
        };
        for list in res.blockarr_lists.iter_mut() {
            unsafe { ptr::write(list, LinkedList::new(FreeAdapter::new())); }
        }
        res
    }

    /// Allocate a Descriptor of length `n`.
    pub fn allocate(&mut self, n: usize) -> *mut FreeListNode {
        if n <= arena::CAPACITY {
            let mut odescr = None;

            for list in self.blockarr_lists[Self::alloc_index(n)..].iter_mut() {
                odescr = list.pop_front();
                if odescr.is_some() { break; }
            }

            if let Some(descr) = odescr {
                match descr.len() {
                    len if len == n => UnsafeRef::into_raw(descr),
                    len if len > n => {
                        let descr = UnsafeRef::into_raw(descr);
                        unsafe {
                            let res = (*descr).split_off(n);
                            self.push_front(UnsafeRef::from_raw(descr));
                            res
                        }
                    },
                    _ => panic!("blockarr_lists corrupted")
                }
            } else {
                let arena_arr = self.arena_allocator.allocate(1);
                unsafe {
                    let descr = arena_arr;
                    let res = FreeListNode::init(descr as _, n);
                    let excess_descr = (*res).offset(n);
                    self.free(FreeListNode::init(excess_descr, arena::CAPACITY - n));
                    res
                }
            }
        } else {
            unimplemented!()
        }
    }

    /// Free `descr`.
    pub fn free(&mut self, descr: *mut FreeListNode) {
        unsafe { self.push_front(UnsafeRef::from_raw(descr)) };
        unimplemented!(); // TODO: coalescing
    }

    /// Push `descr` on top of the appropriate free list.
    fn push_front(&mut self, descr: UnsafeRef<FreeListNode>) {
        let i = Self::free_index(descr.len());
        self.blockarr_lists[i].push_front(descr);
    }

    fn alloc_index(n: usize) -> usize { n.log2_ceil() }

    fn free_index(n: usize) -> usize { n.log2_floor() }
}
