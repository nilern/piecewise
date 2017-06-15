use core::nonzero::NonZero;
use std::ptr;
use std::ops::Index;
use std::mem::transmute;
use std::ptr::Unique;
use std::cell::Cell;
use intrusive_collections::{UnsafeRef, RBTreeLink, KeyAdapter};

use util::{Lengthy, Uninitialized, Init, SplitOff};
use arena::{self, Arena};

#[cfg(target_pointer_width = "64")]
pub const SIZE: usize = 64;

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
            let i = tot_n / arena::CAPACITY;
            let j = tot_n % arena::CAPACITY;
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

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::{SIZE, FreeRope};

    #[test]
    fn size() {
        assert_eq!(size_of::<FreeRope>(), SIZE);
    }
}
