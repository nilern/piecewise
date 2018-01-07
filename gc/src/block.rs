use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::Unique;
use intrusive_collections::{IntrusivePointer, RBTree, UnsafeRef, Bound};

use util::{Uninitialized, Initializable, Foam, Span, AllocSat};
use layout::Markmap;
use arena::ArenaAllocator;
use descriptor::{Descriptor, MSBlock, LargeObjRope, FreeRope, AddrFreeRope, SizeFreeRope};

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
        self.allocate(unsafe { NonZero::new_unchecked(1) })
            .and_then(|uptr|
                self.alloc_markmap()
                    .or_else(|| if self.refill_markmaps() {
                        self.alloc_markmap()
                    } else {
                        unsafe { self.release(Unique::new_unchecked(uptr.as_ptr() as _),
                                              NonZero::new_unchecked(1)); }
                        None
                    })
                    .map(|marks| unsafe { MSBlock::init(uptr, marks) }))
    }

    pub fn alloc_large_obj_rope(&mut self, n: NonZero<usize>) -> Option<Unique<LargeObjRope>> {
        self.allocate(n).map(|uptr| unsafe { LargeObjRope::init(uptr, n) })
    }

    fn alloc_markmap(&mut self) -> Option<Unique<Markmap>> {
        use self::AllocSat::*;

        self.markmaps.as_ref()
            .and_then(|mmaps| mmaps.weigh_against(1))
            .and_then(|sat| match sat {
                Split => self.markmaps.as_mut().map(|b| unsafe { b.split_off(1) }),
                Consume => self.markmaps.take().map(Span::take)
            })
            .map(|ummap| unsafe {
                let mmap: *mut Markmap = ummap.as_ptr() as _;
                for byte in (*mmap).iter_mut() {
                    *byte = 0;
                }
                Unique::new_unchecked(mmap)
            })
    }

    fn allocate(&mut self, n: NonZero<usize>) -> Option<Initializable<Descriptor>> {
        self.free_ropes.allocate(n)
            .or_else(|| {
                self.arenas.allocate_at_least(n)
                    .map(|fresh_rope| unsafe {
                        let rem = (*fresh_rope.as_ptr()).len() - n.get();
                        if rem > 0 {
                            let excess = (*fresh_rope.as_ptr()).split_off(rem);
                            self.release(excess, NonZero::new_unchecked(rem));
                        }
                        Unique::new_unchecked(fresh_rope.as_ptr() as _)
                    })
            })
    }

    pub fn release(&mut self, uptr: Initializable<FreeRope>, n: NonZero<usize>) {
        self.free_ropes.release(uptr, n)
    }

    fn refill_markmaps(&mut self) -> bool {
        if let Some(uptr) = self.allocate(unsafe { NonZero::new_unchecked(1) }) {
            let descr: *mut Descriptor = uptr.as_ptr() as _;
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

    fn insert(&mut self, rope: UnsafeRef<FreeRope>) {
        self.by_addr.insert(rope.clone());
        self.by_size.insert(rope);
    }

    fn allocate(&mut self, n: NonZero<usize>) -> Option<Initializable<Descriptor>> {
        use self::AllocSat::*;

        let (rope, erm) = {
            let mut cursor = self.by_size.lower_bound_mut(Bound::Included(&n.get()));
            if let Some(sat) = cursor.get().and_then(|node| node.weigh_against(n.get())) {
                let rope: *mut FreeRope = cursor.remove().unwrap().into_raw() as _;
                unsafe { self.by_addr.cursor_mut_from_ptr(rope).remove(); }
                match sat {
                    Split => unsafe {
                        let rem = (*rope).len() - n.get();
                        let erm = Some(((*rope).split_off(rem), NonZero::new_unchecked(rem)));
                        (Some(Unique::new_unchecked(rope as _)), erm)
                    },
                    Consume => (Some(unsafe { Unique::new_unchecked(rope as _) }), None)
                }
            } else {
                (None, None)
            }
        };
        if let Some((excess, rem)) = erm {
            self.release(excess, rem);
        }
        rope
    }

    fn release(&mut self, uptr: Initializable<FreeRope>, n: NonZero<usize>) {
        if let Some(lr) = self.lower_adj(uptr.as_ptr()) {
            unsafe {
                (*lr).extend(n); // free space goes to `lr`
                if let Some(rr) = self.upper_adj(uptr.as_ptr()) {
                    // `rr` is removed and its free space goes to `lr`:
                    (*lr).extend(NonZero::new_unchecked((*rr).len()));
                    self.by_addr.cursor_mut_from_ptr(rr).remove();
                    self.by_size.cursor_mut_from_ptr(rr).remove();
                }
                // `lr` is now bigger so its position in `by_size` needs to change:
                self.by_size.cursor_mut_from_ptr(lr)
                    .remove()
                    .map(|l| self.by_size.insert(l));
            }
        } else if let Some(rr) = self.upper_adj(uptr.as_ptr()) {
            unsafe {
                // `rr` is removed and a new node created:
                let m = NonZero::new_unchecked(n.get() + (*rr).len());
                self.insert(UnsafeRef::from_raw(FreeRope::init(uptr, m).as_ptr()));
                self.by_addr.cursor_mut_from_ptr(rr).remove();
                self.by_size.cursor_mut_from_ptr(rr).remove();
            }
        } else {
            unsafe {
                // a new node is created (with no change to existing ones):
                self.insert(UnsafeRef::from_raw(FreeRope::init(uptr, n).as_ptr()));
            }
        }
    }

    fn lower_adj(&self, uptr: *mut Uninitialized<FreeRope>) -> Option<*mut FreeRope> {
        let lcursor = self.by_addr.upper_bound(Bound::Excluded(&(uptr as _)));
        match lcursor.get() {
            Some(lr) if FreeRope::are_adjacent(lr, unsafe { &*(uptr as *mut FreeRope) }) =>
                Some(unsafe { transmute(lr) }),
            _ => None
        }
    }

    fn upper_adj(&self, uptr: *mut Uninitialized<FreeRope>) -> Option<*mut FreeRope> {
        let rcursor = self.by_addr.lower_bound(Bound::Excluded(&(uptr as _)));
        match rcursor.get() {
            Some(rr) if FreeRope::are_adjacent(unsafe { &*(uptr as *mut FreeRope) }, rr) =>
                Some(unsafe { transmute(rr) }),
            _ => None
        }
    }
}
