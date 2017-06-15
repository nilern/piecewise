use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::Unique;
use std::cmp::Ordering;
use intrusive_collections::{IntrusivePointer, RBTree, UnsafeRef, Bound};

use util::{Init, Lengthy, SplitOff, Uninitialized, Span};
use layout::Markmap;
use arena::ArenaAllocator;
use descriptor::{Descriptor, MSBlock, LargeObjRope, FreeRope,
                 AddrFreeRope, SizeFreeRope};

// ================================================================================================

pub struct BlockAllocator {
    free_ropes: Freelist,
    markmaps: Option<Span<Uninitialized<Markmap>>>,
    arenas: ArenaAllocator
}

impl BlockAllocator {
    pub fn new(max_heap: usize) -> Self {
        BlockAllocator {
            free_ropes: Freelist::new(),
            markmaps: None,
            arenas: ArenaAllocator::new(max_heap)
        }
    }

    pub fn alloc_ms_block(&mut self) -> Option<(Unique<MSBlock>, Span<Uninitialized<usize>>)> {
        self.allocate(unsafe { NonZero::new(1) })
            .and_then(|uptr|
                self.alloc_markmap()
                    .or_else(|| if self.refill_markmaps() {
                        self.alloc_markmap()
                    } else {
                        None
                    })
                    .map(|marks| unsafe { MSBlock::init(uptr, marks) }))
    }

    pub fn alloc_large_obj_rope(&mut self, n: NonZero<usize>) -> Option<Unique<LargeObjRope>> {
        self.allocate(n).map(|uptr| unsafe { LargeObjRope::init(uptr, n) })
    }

    fn alloc_markmap(&mut self) -> Option<Unique<Markmap>> {
        self.markmaps.as_mut().and_then(|mmaps| mmaps.split_off(1))
            .map(|ummap| unsafe {
                let mmap: *mut Markmap = *ummap as _;
                for byte in (*mmap).iter_mut() {
                    *byte = 0;
                }
                Unique::new(mmap)
            })
    }

    fn allocate(&mut self, n: NonZero<usize>) -> Option<Unique<Uninitialized<Descriptor>>> {
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

    fn refill_markmaps(&mut self) -> bool {
        if let Some(uptr) = self.allocate(unsafe { NonZero::new(1) }) {
            let descr: *mut Descriptor = *uptr as _;
            self.markmaps = Some(unsafe { (*descr).markmaps_mem() });
            true
        } else {
            false
        }
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

    fn allocate(&mut self, n: NonZero<usize>) -> Option<Unique<Uninitialized<Descriptor>>> {
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
            Unique::new(rope as _)
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
