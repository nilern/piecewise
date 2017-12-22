use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::{self, Unique};
use std::cell::Cell;
use std::ops::Index;
use intrusive_collections::{LinkedListLink, RBTreeLink, KeyAdapter, UnsafeRef};

use gce::util::{Uninitialized, Initializable, Foam, Span, AllocSat};
use gce::layout::{Arena, Block, DESCR_SHIFT, DESCR_MASK, Granule, GSize, Markmap};
use gce::object_model::Object;

// ================================================================================================

pub enum Descriptor {
    MSBlock(MSBlock),
    LargeObjRope(LargeObjRope)
}

impl Descriptor {
    pub fn of(obj: &Object) -> *const Descriptor {
        unsafe {
            let addr: usize = transmute(obj);
            let block_index = (addr & Arena::MASK) >> Block::SHIFT;
            let arena_addr = addr & !Arena::MASK;
            (arena_addr | block_index << DESCR_SHIFT) as _
        }
    }

    pub fn mark_of(&self, obj: &Object) -> u8 {
        match self {
            &Descriptor::MSBlock(ref sd) => unsafe {
                let obj_addr: usize = transmute(obj);
                let index = (obj_addr & Block::MASK) >> Granule::SHIFT;
                sd.get_mark(index)
            },
            &Descriptor::LargeObjRope(ref sd) => sd.get_mark()
        }
    }

    pub fn set_mark_of(&mut self, obj: &Object, mark: u8, len: GSize) {
        match self {
            &mut Descriptor::MSBlock(ref mut sd) => unsafe {
                let obj_addr: usize = transmute(obj);
                let index = (obj_addr & Block::MASK) >> Granule::SHIFT;
                sd.set_mark(index, mark, len)
            },
            &mut Descriptor::LargeObjRope(ref mut sd) => sd.set_mark(mark)
        }
    }

    pub unsafe fn mem(&self) -> Span<Uninitialized<usize>> {
        Span::from_raw_parts(Unique::new_unchecked(self.start() as _), self.end() as _)
    }

    pub unsafe fn markmaps_mem(&self) -> Span<Uninitialized<Markmap>> {
        Span::from_raw_parts(Unique::new_unchecked(self.start() as _), self.end() as _)
    }

    pub fn start(&self) -> *const usize {
        unsafe {
            let arena = Arena::containing(self as *const Self);
            let index: usize = (*arena).descriptor_index(self);
            transmute(&(*arena).blocks[index])
        }
    }

    pub fn end(&self) -> *const usize {
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
    pub unsafe fn init(uptr: Initializable<Descriptor>, marks: Unique<Markmap>)
        -> (Unique<MSBlock>, Span<Uninitialized<usize>>)
    {
        let ptr: *mut Descriptor = uptr.as_ptr() as _;
        ptr::write(ptr, Descriptor::MSBlock(MSBlock {
            link: LinkedListLink::default(),
            marks: marks,
            _padding: Default::default()
        }));
        if let &Descriptor::MSBlock(ref block) = &*ptr {
            (Unique::new_unchecked(transmute(block)), block.upcast().mem())
        } else {
            unreachable!()
        }
    }

    pub fn get_obj(&self, index: usize) -> *const Object {
        unsafe { self.upcast().start().offset(index as isize) as _ }
    }

    pub fn get_mark(&self, index: usize) -> u8 { unsafe { (*self.marks.as_ptr())[index] } }

    pub fn set_mark(&mut self, index: usize, mark: u8, len: GSize) {
        unsafe {
            (*self.marks.as_ptr())[index] = mark;
            (*self.marks.as_ptr())[index + usize::from(len)] = mark;
        }
    }

    pub fn marks(&self) -> &Markmap { unsafe { &(*self.marks.as_ptr()) } }
}

// ================================================================================================

pub struct LargeObjRope {
    link: LinkedListLink,
    len: NonZero<usize>,
    mark: u8,
    _bpadding: [u8; 3],
    _padding: [usize; 3]
}

impl SubDescr for LargeObjRope {}

intrusive_adapter!(pub LargeObjRopeAdapter = UnsafeRef<LargeObjRope>:
                   LargeObjRope { link: LinkedListLink });

impl LargeObjRope {
    pub unsafe fn init(uptr: Initializable<Descriptor>, n: NonZero<usize>)
        -> Unique<LargeObjRope>
    {
        let ptr: *mut Descriptor = uptr.as_ptr() as _;
        ptr::write(ptr, Descriptor::LargeObjRope(LargeObjRope {
            link: LinkedListLink::default(),
            len: n,
            mark: 0,
            _bpadding: Default::default(),
            _padding: Default::default()
        }));
        if let &Descriptor::LargeObjRope(ref block) = &*ptr {
            Unique::new_unchecked(transmute(block))
        } else {
            unreachable!()
        }
    }

    pub fn len(&self) -> NonZero<usize> { self.len }

    pub fn get_mark(&self) -> u8 { self.mark }

    pub fn set_mark(&mut self, mark: u8) { self.mark = mark; }
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
    pub unsafe fn init(uptr: Initializable<FreeRope>, len: NonZero<usize>) -> Unique<FreeRope> {
        let ptr = uptr.as_ptr() as *mut FreeRope;
        ptr::write(ptr, FreeRope {
            addr_link: RBTreeLink::default(),
            size_link: RBTreeLink::default(),
            len: Cell::new(len.get()),
            _padding: Default::default()
        });
        Unique::new_unchecked(ptr)
    }

    pub fn len(&self) -> usize { self.len.get() }

    fn set_len(&self, new_len: usize) { self.len.set(new_len) }

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
        self.set_len(self.len() + n.get());
    }
}

impl Index<usize> for FreeRope  {
    type Output = Uninitialized<FreeRope>;

    fn index(&self, index: usize) -> &Uninitialized<FreeRope> {
        unsafe { transmute(self.offset(index)) }
    }
}

impl Foam for FreeRope {
    type Bubble = Uninitialized<FreeRope>;
    type Owner = Unique<FreeRope>;

    fn weigh_against(&self, request: usize) -> Option<AllocSat> {
        use std::cmp::Ordering::*;
        use self::AllocSat::*;

        match self.len().cmp(&request) {
            Greater => Some(Split),
            Equal => Some(Consume),
            Less => None
        }
    }

    unsafe fn split_off(&self, request: usize) -> Initializable<FreeRope> {
        debug_assert!(request < self.len());

        let rem = self.len() - request;
        let ptr: *mut () = transmute(&self[rem]);
        self.set_len(rem);
        Unique::new_unchecked(ptr as _)
    }

    fn take(rope: Unique<FreeRope>) -> Initializable<FreeRope> {
        unsafe { Unique::new_unchecked(rope.as_ptr() as _) }
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::{Descriptor, FreeRope};
    use gce::layout::DESCR_SIZE;

    #[test]
    fn descriptor_size() {
        assert_eq!(size_of::<Descriptor>(), DESCR_SIZE);
    }

    #[test]
    fn freerope_size() {
        assert_eq!(size_of::<FreeRope>(), DESCR_SIZE);
    }
}
