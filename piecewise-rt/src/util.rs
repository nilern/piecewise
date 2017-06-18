use std::ptr::Unique;
use std::cell::Cell;
use std::fmt;

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

// TODO: get rid of this
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

    pub fn satisfiability(&self, n: usize) -> Option<AllocSat> {
        use std::cmp::Ordering::*;
        use self::AllocSat::*;

        let new_start: *const T = unsafe { self.start.offset(n as isize) as _ };
        match self.end.cmp(&new_start) {
            Greater => Some(Split),
            Equal => Some(Consume),
            Less => None
        }
    }

    pub unsafe fn split_off(&mut self, n: usize) -> Unique<T> {
        let res = Unique::new(*self.start);
        self.start = Unique::new(self.start.offset(n as isize));
        res
    }

    pub fn into_unique(self) -> Unique<T> { self.start }

    pub fn into_owned_slice(self) -> OwnedSlice<T> {
        let len = self.end as usize - *self.start as usize;
        unsafe { OwnedSlice::from_raw_parts(self.start, len) }
    }
}

impl<T> fmt::Debug for Span<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span {{ start: {:p}, end: {:p} }}", self.start, self.end)
    }
}

#[derive(Debug)]
pub enum AllocSat {
    Split,
    Consume
}
