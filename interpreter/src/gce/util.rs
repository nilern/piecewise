use std::mem::size_of;
use std::ptr::Unique;
use std::cell::RefCell;
use std::fmt;

pub struct Uninitialized<T>(T);

pub type Initializable<T> = Unique<Uninitialized<T>>;

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

#[derive(Debug)]
pub enum AllocSat {
    Split,
    Consume
}

pub trait Foam {
    type Bubble;
    type Owner;

    fn weigh_against(&self, request: usize) -> Option<AllocSat>;

    unsafe fn split_off(&self, request: usize) -> Unique<Self::Bubble>;

    fn take(foam: Self::Owner) -> Unique<Self::Bubble>;
}

pub struct Span<T> {
    start: RefCell<Unique<T>>,
    end: *const T
}

impl<T> Span<T> {
    pub unsafe fn from_raw_parts(start: Unique<T>, end: *const T) -> Self {
        Span {
            start: RefCell::new(start),
            end: end
        }
    }

    pub fn len(&self) -> usize {
        // TODO: use .offset_to()
        (self.end as usize - self.start.borrow().as_ptr() as usize) / size_of::<T>()
    }
}

impl<T> fmt::Debug for Span<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span {{ start: {:p}, end: {:p} }}", self.start.borrow().as_ptr(), self.end)
    }
}

impl<T> Foam for Span<T> {
    type Bubble = T;
    type Owner = Span<T>;

    fn weigh_against(&self, request: usize) -> Option<AllocSat> {
        use std::cmp::Ordering::*;
        use self::AllocSat::*;

        let new_start: *const T = unsafe { self.start.borrow().as_ptr()
                                                              .offset(request as _) as _ };
        match self.end.cmp(&new_start) {
            Greater => Some(Split),
            Equal => Some(Consume),
            Less => None
        }
    }

    unsafe fn split_off(&self, request: usize) -> Unique<T> {
        debug_assert!(request <= self.len());

        let old_start = self.start.borrow().as_ptr();
        *self.start.borrow_mut() = Unique::new_unchecked(old_start.offset(request as _));
        Unique::new_unchecked(old_start)
    }

    fn take(span: Span<T>) -> Unique<T> { span.start.into_inner() }
}
