use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::{self, Unique};
use std::cmp::Ordering;
use intrusive_collections::{RBTree, Bound, IntrusivePointer, UnsafeRef,
                            LinkedList, LinkedListLink};

use util::{Init, Lengthy, Uninitialized, SplitOff, OwnedSlice, Span};
use arena::{Arena, ArenaAllocator};
use object_model::ValueRef;
use freerope::{FreeRope, AddrFreeRope, SizeFreeRope};
use mark_n_sweep::SizedFreeAdapter;

/// A number such that SIZE = 1^SHIFT
pub const SHIFT: usize = 12;

/// A block is SIZE bytes of memory
pub const SIZE: usize = 1 << SHIFT;

pub const WSIZE: usize = SIZE >> ValueRef::SHIFT;

pub type Block = [usize; WSIZE];

pub type Markmap = [u8; WSIZE];

// ================================================================================================

pub enum Descriptor {
    MSBlock(MSBlock),
    LargeObjRope(LargeObjRope)
}

impl Descriptor {
    #[cfg(target_pointer_width = "64")]
    pub const SHIFT: usize = 6;

    pub const SIZE: usize = 1 << Self::SHIFT;

    pub const MASK: usize = Self::SIZE - 1;

    unsafe fn mem(&self) -> Span<Uninitialized<usize>> {
        Span::from_raw_parts(Unique::new(self.start() as _), self.end() as _)
    }

    unsafe fn markmaps_mem(&self) -> Span<Uninitialized<Markmap>> {
        Span::from_raw_parts(Unique::new(self.start() as _), self.end() as _)
    }

    fn start(&self) -> *const usize {
        unsafe {
            let arena = Arena::containing(self as *const Self);
            let index: usize = (*arena).descriptor_index(self);
            transmute(&(*arena).blocks[index])
        }
    }

    fn end(&self) -> *const usize {
        unsafe { self.start().offset(self::WSIZE as isize) }
    }
}

trait SubDescr: Sized {
    fn upcast(&self) -> &Descriptor {
        unsafe { transmute(transmute::<_, usize>(self) & !Descriptor::MASK) }
    }
}

// ================================================================================================

pub struct MSBlock {
    link: LinkedListLink,
    marks: Unique<Markmap>,
    _padding: [usize; 4]
}

impl SubDescr for MSBlock {}

intrusive_adapter!(pub MSBlockAdapter = UnsafeRef<MSBlock>: MSBlock { link: LinkedListLink });

impl MSBlock {
    pub fn sweep(&self, block_allocator: &mut BlockAllocator,
                        freelist: &mut LinkedList<SizedFreeAdapter>)
    {
        unimplemented!()
    }
}

impl SplitOff<OwnedSlice<usize>> for MSBlock {
    unsafe fn split_off(&self, n: usize) -> OwnedSlice<usize> {
        unimplemented!()
    }
}

// ================================================================================================

pub struct LargeObjRope {
    link: LinkedListLink,
    _padding: [usize; 5]
}

impl SubDescr for LargeObjRope {}

intrusive_adapter!(pub LargeObjRopeAdapter = UnsafeRef<LargeObjRope>:
                   LargeObjRope { link: LinkedListLink });

impl LargeObjRope {
    pub fn start_mut(&mut self) -> *mut usize {
        unimplemented!()
    }
}

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
                    .map(|marks| {
                        let ptr: *mut Descriptor = *uptr as _;
                        unsafe {
                            ptr::write(ptr, Descriptor::MSBlock(MSBlock {
                                link: LinkedListLink::default(),
                                marks: marks,
                                _padding: Default::default()
                            }));
                            if let &Descriptor::MSBlock(ref block) = &*ptr {
                                (Unique::new(transmute(block)), block.upcast().mem())
                            } else {
                                unreachable!()
                            }
                        }
                    }))
    }

    pub fn alloc_large_obj_rope(&mut self, n: NonZero<usize>) -> Option<Unique<LargeObjRope>> {
        unimplemented!()
    }

    fn alloc_markmap(&mut self) -> Option<Unique<Markmap>> {
        unimplemented!()
    }

    fn allocate(&mut self, n: NonZero<usize>) -> Option<Unique<Uninitialized<FreeRope>>> {
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

// ================================================================================================

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::Descriptor;

    #[test]
    fn descriptor_size() {
        assert_eq!(size_of::<Descriptor>(), Descriptor::SIZE);
    }
}
