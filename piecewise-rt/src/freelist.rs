use std::mem;
use std::mem::transmute;
use std::ptr;
use std::ptr::Unique;
use intrusive_collections::{LinkedList, LinkedListLink, Adapter, UnsafeRef, IntrusivePointer};

/// Freelist interface
pub trait FreeList {
    /// Try to allocate `n` words from this list.
    fn allocate_words(&mut self, n: usize) -> Option<Unique<()>>;

    /// Push `fobj` to the front of this list. `n` is the number of free words in `fobj`.
    unsafe fn push_front(&mut self, fobj: Unique<()>, n: usize) -> bool;
}

const NBUCKETS: usize = 16;

/// Bucketed freelist
pub struct Bucketed<N> where N: Adapter<Link = LinkedListLink> + Default, N::Value: Default {
    buckets: [LinkedList<N>; NBUCKETS]
}

impl<N> Bucketed<N> where N: Adapter<Link = LinkedListLink> + Default, N::Value: Default {
    /// Create a new bucketed freelist.
    pub fn new() -> Self {
        // The array initialization is nasty. Hopefully there will be a better way in future Rust.

        let mut res = Bucketed {
            buckets: unsafe { mem::uninitialized::<[_; NBUCKETS]>() }
        };
        for list in res.buckets.iter_mut() {
            unsafe { ptr::write(list, LinkedList::new(N::default())); }
        }
        res
    }

    fn alloc_index(n: usize) -> Option<usize> {
        if n <= 8 {
            Some(n - 1)         // 1 -> 0, .., 8 -> 7
        } else if n <= 16 {
            Some((n + 7) >> 1)  // 9 -> 8, 10 -> 8, .., 15 -> 11, 16 -> 11
        } else if n <= 32 {
            Some((n + 31) >> 2) // 17 -> 12, .., 20 -> 12, ..., 29 -> 15, .., 32 -> 15
        } else {
            None
        }
    }

    fn free_index(n: usize) -> Option<usize> {
        if n <= 8 {
            Some(n - 1)        // 1 -> 0, .., 8 -> 7
        } else if n <= 16 {
            Some((n >> 1) + 3) // 9 -> 7, 10 -> 8, 11 -> 8, .., 15 -> 10, 16 -> 11
        } else if n <= 35 {
            Some((n >> 2) + 7) // 17 -> 11, .., 19 -> 11, ..., 32 -> 15, .., 35 ->
        } else {
            None
        }
    }
}

impl<N> FreeList for Bucketed<N> where N: Adapter<Link = LinkedListLink> + Default,
                                       N::Value: Default
{
    fn allocate_words(&mut self, n: usize) -> Option<Unique<()>> {
        Self::alloc_index(n)
             .and_then(|i| self.buckets[i].pop_front())
             .map(|v| unsafe { Unique::new(v.into_raw() as *mut ()) })
    }

    unsafe fn push_front(&mut self, fobj: Unique<()>, n: usize) -> bool {
        if let Some(i) = Self::free_index(n) {
            let node = transmute::<*mut (), *mut N::Value>(*fobj);
            ptr::write(node, N::Value::default());
            self.buckets[i].push_front(N::Pointer::from_raw(node));
            true
        } else {
            false
        }
    }
}
