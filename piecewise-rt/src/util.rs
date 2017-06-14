use core::nonzero::NonZero;
use std::mem::size_of;
use std::ptr::Unique;
use std::marker::PhantomData;
use std::cell::Cell;

pub struct Uninitialized<T>(PhantomData<T>);

pub trait Init: Sized {
    unsafe fn init(uptr: Unique<Uninitialized<Self>>, len: NonZero<usize>) -> Unique<Self>;
}

pub trait Lengthy {
    fn len(&self) -> usize;
    fn set_len(&self, new_len: usize);
}

pub trait SplitOff<R> {
    unsafe fn split_off(&self, n: usize) -> R;
}

pub trait CeilDiv {
    fn ceil_div(self, other: Self) -> Self;
}

impl CeilDiv for usize {
    fn ceil_div(self, other: usize) -> usize {
        if self % other > 0 {
            self / other + 1
        } else {
            self / other
        }
    }
}

pub fn wsize_of<T>() -> usize { size_of::<T>().ceil_div(size_of::<usize>()) }

pub struct OwnedSlice<T> {
    ptr: Unique<T>,
    len: Cell<usize>
}

impl<T> OwnedSlice<T> {
    pub fn from_raw_parts(ptr: Unique<T>, len: usize) -> Self {
        OwnedSlice {
            ptr: ptr,
            len: Cell::new(len)
        }
    }

    pub fn into_unique(self) -> Unique<T> { self.ptr }
}

impl<T> Lengthy for OwnedSlice<T> {
    fn len(&self) -> usize { self.len.get() }

    fn set_len(&self, new_len: usize) { self.len.set(new_len) }
}

impl<T> SplitOff<OwnedSlice<T>> for OwnedSlice<T> {
    unsafe fn split_off(&self, n: usize) -> OwnedSlice<T> {
        debug_assert!(n <= self.len());

        let rem = self.len() - n;
        self.set_len(rem);
        OwnedSlice::from_raw_parts(Unique::new(self.ptr.offset(rem as isize)), n)
    }
}
