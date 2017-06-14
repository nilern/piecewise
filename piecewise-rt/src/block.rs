use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::Unique;
use std::cmp::Ordering;
use intrusive_collections::{RBTree, Bound, IntrusivePointer, UnsafeRef};

use util::{Init, Lengthy, Uninitialized, SplitOff};
use arena::ArenaAllocator;
use object_model::ValueRef;
use freerope::{FreeRope, AddrFreeRope, SizeFreeRope};

/// A number such that SIZE = 1^SHIFT
pub const SHIFT: usize = 12;

/// A block is SIZE bytes of memory
pub const SIZE: usize = 1 << SHIFT;

pub const WSIZE: usize = SIZE >> ValueRef::SHIFT;

pub type Block = [ValueRef; WSIZE];

// ================================================================================================

pub struct BlockAllocator {
    free_ropes: Freelist,
    arenas: ArenaAllocator
}

impl BlockAllocator {
    pub fn new(max_heap: usize) -> Self {
        BlockAllocator {
            free_ropes: Freelist::new(),
            arenas: ArenaAllocator::new(max_heap)
        }
    }

    pub fn allocate(&mut self, n: NonZero<usize>) -> Option<Unique<Uninitialized<FreeRope>>> {
        self.free_ropes.allocate(n)
            .or_else(|| {
                self.arenas.allocate_at_least(n)
                    .map(|fresh_rope| unsafe {
                        let rem = (**fresh_rope).len() - *n;
                        if rem > 0 {
                            let excess = (**fresh_rope).split_off(rem);
                            self.release(excess, NonZero::new(rem));
                        }
                        Unique::new(*fresh_rope as _)
                    })
            })
    }

    pub fn release(&mut self, uptr: Unique<Uninitialized<FreeRope>>, n: NonZero<usize>) {
        self.free_ropes.release(uptr, n)
    }
}

// ================================================================================================

struct Freelist {
    by_addr: RBTree<AddrFreeRope>,
    by_size: RBTree<SizeFreeRope>
}

impl Freelist {
    fn new() -> Self {
        Freelist {
            by_addr: RBTree::new(AddrFreeRope::new()),
            by_size: RBTree::new(SizeFreeRope::new())
        }
    }

    fn allocate(&mut self, n: NonZero<usize>) -> Option<Unique<Uninitialized<FreeRope>>> {
        let (rope, erm) = {
            let mut cursor = self.by_size.lower_bound_mut(Bound::Included(&*n));
            match cursor.get().map(|node| node.len().cmp(&*n)) {
                Some(Ordering::Equal) => {
                    let rope = cursor.remove().unwrap().into_raw();
                    (Some(rope), None)
                },
                Some(Ordering::Greater) => {
                    let rope = cursor.remove().unwrap().into_raw();
                    unsafe {
                        let rem = (*rope).len() - *n;
                        (Some(rope), Some(((*rope).split_off(rem), NonZero::new(rem))))
                    }
                },
                _ => (None, None)
            }
        };
        if let Some((excess, rem)) = erm {
            self.release(excess, rem);
        }
        rope.map(|rope| unsafe {
            self.by_addr.cursor_mut_from_ptr(rope).remove();
            Unique::new(rope as *mut Uninitialized<FreeRope>)
        })
    }

    fn release(&mut self, uptr: Unique<Uninitialized<FreeRope>>, n: NonZero<usize>) {
        let lr = {
            let mut lcursor = self.by_addr.upper_bound_mut(Bound::Excluded(&(*uptr as _)));
            match lcursor.get() {
                Some(lr) if FreeRope::are_adjacent(lr, unsafe { transmute(*uptr) }) => {
                    lcursor.remove()
                },
                _ => None
            }
        };
        let rr = {
            let mut rcursor = self.by_addr.lower_bound_mut(Bound::Excluded(&(*uptr as _)));
            match rcursor.get() {
                Some(rr) if FreeRope::are_adjacent(unsafe { transmute(*uptr) }, rr) => {
                    rcursor.remove()
                },
                _ => None
            }
        };
        let rope = if let Some(lrope) = lr {
            let lptr: *mut FreeRope = lrope.into_raw() as _;
            unsafe { (*lptr).extend(n); }
            lptr
        } else {
            unsafe { *FreeRope::init(uptr, n) }
        };
        if let Some(rrope) = rr {
            unsafe { (*rope).extend(NonZero::new(rrope.len())); }
        }

        unsafe {
            self.by_addr.insert(UnsafeRef::from_raw(rope));
            self.by_size.insert(UnsafeRef::from_raw(rope));
        }
    }
}
