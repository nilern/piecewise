use std::ptr::Unique;
use std::cell::Cell;

pub struct Uninitialized<T>(T);

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

pub struct OwnedSlice<T> {
    ptr: Unique<T>,
    len: Cell<usize>
}

impl<T> OwnedSlice<T> {
    pub unsafe fn from_raw_parts(ptr: Unique<T>, len: usize) -> Self {
        OwnedSlice {
            ptr: ptr,
            len: Cell::new(len)
        }
    }

    pub fn into_unique(self) -> Unique<T> { self.ptr }

    pub fn len(&self) -> usize { self.len.get() }
}

pub struct Span<T> {
    start: Unique<T>,
    end: *const T
}

impl<T> Span<T> {
    pub unsafe fn from_raw_parts(start: Unique<T>, end: *const T) -> Self {
        Span {
            start: start,
            end: end
        }
    }

    pub fn split_off(&mut self, n: usize) -> Option<Unique<T>> {
        unsafe {
            let new_start = self.start.offset(n as isize);
            if new_start as *const T <= self.end {
                let res = Some(Unique::new(*self.start));
                self.start = Unique::new(new_start);
                res
            } else {
                None
            }
        }
    }

    pub fn into_owned_slice(self) -> OwnedSlice<T> {
        let len = self.end as usize - *self.start as usize;
        unsafe { OwnedSlice::from_raw_parts(self.start, len) }
    }
}
