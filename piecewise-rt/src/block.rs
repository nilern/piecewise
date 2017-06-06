use std::mem;
use std::ptr;

use arena;
use freelist::FreeList;
use descriptor::{Descriptor, FreeListNode};
use util::IntLog2;

pub const SHIFT: usize = 12;
pub const SIZE: usize = 1 << SHIFT;

/// Block allocator (allocates BlockArr:s)
pub struct Allocator {
    arena_allocator: arena::Allocator,
    blockarr_lists: [FreeList<FreeListNode>; Allocator::NLISTS]
}

impl Allocator {
    const NLISTS: usize = arena::SHIFT - SHIFT;

    /// Create a new block allocator.
    pub fn new() -> Allocator {
        // The array initialization is nasty. Hopefully there will be a better way in future Rust.

        let mut res = Allocator {
            arena_allocator: arena::Allocator::new(),
            blockarr_lists: unsafe { mem::uninitialized::<[_; Allocator::NLISTS]>() }
        };
        for list in res.blockarr_lists.iter_mut() {
            unsafe { ptr::write(list, FreeList::new()); }
        }
        res
    }

    /// Allocate a BlockArr of length `n`.
    pub fn allocate(&mut self, n: usize) -> *mut Descriptor {
        if n <= arena::CAPACITY {
            unimplemented!()
        } else {
            let mut descr = ptr::null_mut();

            for list in self.blockarr_lists[n.log2_ceil()..].iter_mut() {
                descr = list.pop_front();
                if descr.is_null() { break; }
            }

            if !descr.is_null() {
                match unsafe { (*descr).len() } {
                    len if len == n => {
                        unsafe { (*descr).upcast() }
                    },
                    len if len > n => {
                        let res = unsafe { // descr is still non-null...
                            (*(*descr).upcast()).split_off(n)
                        };
                        self.push_front(descr);
                        res
                    },
                    _ => panic!("blockarr_lists corrupted")
                }
            } else {
                unimplemented!()
            }
        }
    }

    /// Push `descr` on top of the appropriate free list.
    fn push_front(&mut self, descr: *mut FreeListNode) {
        unsafe {
            self.blockarr_lists[(*descr).len().log2_floor()]
                .push_front(&mut *descr);
        }
    }
}
