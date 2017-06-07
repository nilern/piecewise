use std::mem;
use std::ptr;
use intrusive_collections::{UnsafeRef, LinkedList};

use arena;
use descriptor::{Descriptor, FreeListNode, FreeAdapter};
use util::IntLog2;

pub const SHIFT: usize = 12;
pub const SIZE: usize = 1 << SHIFT;

/// Block allocator (allocates BlockArr:s)
pub struct Allocator {
    arena_allocator: arena::Allocator,
    blockarr_lists: [LinkedList<FreeAdapter>; Allocator::NLISTS]
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
            unsafe { ptr::write(list, LinkedList::new(FreeAdapter::new())); }
        }
        res
    }

    /// Allocate a BlockArr of length `n`.
    pub fn allocate(&mut self, n: usize) -> *mut Descriptor {
        if n <= arena::CAPACITY {
            unimplemented!()
        } else {
            let mut odescr = None;

            for list in self.blockarr_lists[n.log2_ceil()..].iter_mut() {
                odescr = list.pop_front();
                if odescr.is_some() { break; }
            }

            if let Some(descr) = odescr {
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
    fn push_front(&mut self, descr: UnsafeRef<FreeListNode>) {
        let i = descr.len().log2_floor();
        self.blockarr_lists[i].push_front(descr);
    }
}
