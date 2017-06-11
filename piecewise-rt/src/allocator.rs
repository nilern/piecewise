use core::nonzero::NonZero;
use std::ptr::Unique;

use util::OwnedSlice;

/// Memory allocator
pub trait Allocator {
    /// Try to allocate `wsize` words of memory with an alignment (in words) of `walign`.
    fn allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>) -> Option<Unique<usize>>;
}

/// Generous memory allocator
pub trait OverAllocator {
    /// Try to allocate at least `wsize` words of memory with an alignment (in words) of `walign`.
    /// You might get a lot more so take care to split off what you need and re-release the rest
    /// into some MemoryPool.
    fn allocate_at_least(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<OwnedSlice<usize>>;
}

/// Memory can be released into a `MemoryPool`.
pub trait MemoryPool {
    /// Try to release `n` words of memory behind `oref` into the pool.
    ///
    /// # Safety
    /// Overestimating `wsize` leads to undefined behaviour. (Underestimates just leak memory).
    /// `wsize` must be large enough to hold the link pointers used by Self.
    unsafe fn try_release(&mut self, oref: Unique<usize>, wsize: NonZero<usize>)
        -> Option<Unique<usize>>;
}

/// Like `MemoryPool`, but `release` cannot fail.
pub trait AbsorbentMemoryPool {
    /// Release `wsize` words of memory behind `oref` into the pool.
    ///
    /// # Safety
    /// Overestimating `wsize` leads to undefined behaviour. (Underestimates just leak memory).
    /// `wsize` must be large enough to hold the link pointers used by Self.
    unsafe fn release(&mut self, oref: Unique<usize>, wsize: NonZero<usize>);
}

pub trait SplitOff {
    /// # Panics
    /// Panics if there is insufficient memory.
    fn split_off(&self, wsize: usize) -> Unique<usize>;
}
