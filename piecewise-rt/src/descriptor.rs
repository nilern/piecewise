use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::{self, Unique};
use std::cell::Cell;
use std::ops::Index;
use intrusive_collections::{LinkedList, LinkedListLink, RBTreeLink, KeyAdapter, UnsafeRef};

use util::{Init, Lengthy, Uninitialized, SplitOff, Span};
use layout::{Arena, Block, DESCR_MASK, Markmap};
use block::BlockAllocator;
use mark_n_sweep::SizedFreeAdapter;

// ================================================================================================

pub enum Descriptor {
    MSBlock(MSBlock),
    LargeObjRope(LargeObjRope)
}

impl Descriptor {
    pub unsafe fn mem(&self) -> Span<Uninitialized<usize>> {
        Span::from_raw_parts(Unique::new(self.start() as _), self.end() as _)
    }

    pub unsafe fn markmaps_mem(&self) -> Span<Uninitialized<Markmap>> {
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
        unsafe { self.start().offset(Block::WSIZE as isize) }
    }
}

pub trait SubDescr: Sized {
    fn upcast(&self) -> &Descriptor {
        unsafe { transmute(transmute::<_, usize>(self) & !DESCR_MASK) }
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
    pub unsafe fn init(uptr: Unique<Uninitialized<Descriptor>>, marks: Unique<Markmap>)
        -> (Unique<MSBlock>, Span<Uninitialized<usize>>)
    {
        let ptr: *mut Descriptor = *uptr as _;
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

    pub fn sweep(&self, block_allocator: &mut BlockAllocator,
                        freelist: &mut LinkedList<SizedFreeAdapter>)
    {
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
    pub unsafe fn init(uptr: Unique<Uninitialized<Descriptor>>, n: NonZero<usize>)
        -> Unique<LargeObjRope>
    {
        unimplemented!()
    }

    pub fn start_mut(&mut self) -> *mut usize {
        unimplemented!()
    }

    pub fn get_mark(&self) -> u8 {
        unimplemented!()
    }
}

// ================================================================================================

#[derive(Debug)]
pub struct FreeRope {
    addr_link: RBTreeLink,
    size_link: RBTreeLink,
    len: Cell<usize>,
    #[cfg(target_pointer_width = "64")]
    _padding: usize
}

intrusive_adapter!(pub AddrFreeRope = UnsafeRef<FreeRope>: FreeRope { addr_link: RBTreeLink });
impl<'a> KeyAdapter<'a> for AddrFreeRope {
    type Key = *const FreeRope;

    fn get_key(&self, value: &'a FreeRope) -> *const FreeRope {
        unsafe { transmute(value) }
    }
}

intrusive_adapter!(pub SizeFreeRope = UnsafeRef<FreeRope>: FreeRope { size_link: RBTreeLink });
impl<'a> KeyAdapter<'a> for SizeFreeRope {
    type Key = usize;

    fn get_key(&self, value: &'a FreeRope) -> usize {
        value.len.get()
    }
}

impl FreeRope {
    fn offset(&self, n: usize) -> *const FreeRope {
        unsafe {
            let arena = Arena::containing(self as *const Self);
            let index: usize = (*arena).descriptor_index(self);
            let tot_n = index + n;
            let i = tot_n / Arena::CAPACITY;
            let j = tot_n % Arena::CAPACITY;
            transmute(&(*arena.offset(i as isize)).descriptors[j])
        }
    }

    pub fn are_adjacent(l: &Self, r: &Self) -> bool {
        l.offset(l.len()) == unsafe { transmute::<_, *const Self>(r) }
    }

    pub unsafe fn extend(&mut self, n: NonZero<usize>) {
        self.set_len(self.len() + *n);
    }
}

impl Init for FreeRope {
    unsafe fn init(uptr: Unique<Uninitialized<FreeRope>>, len: NonZero<usize>)
        -> Unique<FreeRope>
    {
        let ptr = *uptr as *mut FreeRope;
        ptr::write(ptr, FreeRope {
            addr_link: RBTreeLink::default(),
            size_link: RBTreeLink::default(),
            len: Cell::new(*len),
            _padding: Default::default()
        });
        Unique::new(ptr)
    }
}

impl Lengthy for FreeRope {
    fn len(&self) -> usize { self.len.get() }

    fn set_len(&self, new_len: usize) { self.len.set(new_len) }
}

impl Index<usize> for FreeRope  {
    type Output = Uninitialized<FreeRope>;

    fn index(&self, index: usize) -> &Uninitialized<FreeRope> {
        unsafe { transmute(self.offset(index)) }
    }
}

impl SplitOff<Unique<Uninitialized<FreeRope>>> for FreeRope {
    unsafe fn split_off(&self, n: usize) -> Unique<Uninitialized<FreeRope>> {
        debug_assert!(n < self.len());

        let rem = self.len() - n;
        let ptr: *mut () = transmute(&self[rem]);
        self.set_len(rem);
        Unique::new(ptr as _)
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::{Descriptor, FreeRope};
    use layout::DESCR_SIZE;

    #[test]
    fn descriptor_size() {
        assert_eq!(size_of::<Descriptor>(), DESCR_SIZE);
    }

    #[test]
    fn freerope_size() {
        assert_eq!(size_of::<FreeRope>(), DESCR_SIZE);
    }
}
