use std::mem;
use std::mem::transmute;
use core::nonzero::NonZero;
use std::ptr;
use std::ptr::Unique;
use std::marker::PhantomData;
use intrusive_collections::{LinkedList, LinkedListLink, Adapter, UnsafeRef, IntrusivePointer};

use util::Lengthy;
use allocator::{Allocator, OverAllocator, MemoryPool};

pub struct FirstFit<N> where N: Adapter<Link = LinkedListLink> + Default,
                             N::Value: Default + Lengthy {
    list: LinkedList<N>
}

impl<N> FirstFit<N> where N: Adapter<Link = LinkedListLink> + Default,
                          N::Value: Default + Lengthy
{
    pub fn new() -> Self {
        FirstFit {
            list: LinkedList::new(N::default())
        }
    }
}

impl<N> OverAllocator for FirstFit<N> where N: Adapter<Link = LinkedListLink> + Default,
                                            N::Value: Default + Lengthy
{
    fn allocate_at_least(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        // TODO: observe walign
        let mut cursor = self.list.cursor_mut();
        while let Some(node) = cursor.get() {
            if node.len() >= *wsize {
                return cursor.remove().map(|v| unsafe { Unique::new(v.into_raw() as *mut ()) });
            }
            cursor.move_next();
        }
        None
    }
}

impl<N> MemoryPool for FirstFit<N> where N: Adapter<Link = LinkedListLink> + Default,
                                         N::Value: Default + Lengthy
{
    unsafe fn try_release(&mut self, oref: Unique<()>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        let node = transmute::<*mut (), *mut N::Value>(*oref);
        ptr::write(node, N::Value::default());
        (*node).set_len(*wsize);
        self.list.push_front(N::Pointer::from_raw(node));
        None
    }
}

/// Compute free list indices from object word counts.
/// # Laws
/// `alloc_index(x) >= free_index(x) | x > 0`
pub trait IndexCalculation {
    /// Get the index of the first freelist that can support allocation of `n` words.
    fn alloc_index(n: NonZero<usize>) -> usize;

    /// Get the index of the freelist where an object of `n` words should be freed to.
    fn free_index(n: NonZero<usize>) -> usize;
}

// TODO: Use singly linked lists
// MAYBE: Use typelevel numbers to exchange the Vec for a size-generic array
/// Bucketed freelist
pub struct Bucketed<N, I> where N: Adapter<Link = LinkedListLink> + Default, N::Value: Default,
                                I: IndexCalculation
{
    buckets: Vec<LinkedList<N>>,
    indec_calc: PhantomData<I>
}

impl<N, I> Bucketed<N, I> where N: Adapter<Link = LinkedListLink> + Default, N::Value: Default,
                                I: IndexCalculation
{
    /// Create a new bucketed freelist with `n` buckets.
    pub fn new(n: usize) -> Self {
        Bucketed {
            buckets: (0..n).map(|_| LinkedList::new(N::default())).collect(),
            indec_calc: PhantomData::default()
        }
    }
}

impl<N, I> OverAllocator for Bucketed<N, I> where N: Adapter<Link = LinkedListLink> + Default,
                                                  N::Value: Default,
                                                  I: IndexCalculation
{
    fn allocate_at_least(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        // TODO: observe walign
        let start = I::alloc_index(wsize);
        self.buckets.get_mut(start)
            .and_then(LinkedList::pop_front)
            .or_else(|| self.buckets[start + 1..].iter_mut()
                            .find(|b| !b.is_empty())
                            .and_then(LinkedList::pop_front))
            .map(|v| unsafe { Unique::new(v.into_raw() as *mut ()) })
    }
}

impl<N, I> MemoryPool for Bucketed<N, I> where N: Adapter<Link = LinkedListLink> + Default,
                                               N::Value: Default,
                                               I: IndexCalculation
{
    unsafe fn try_release(&mut self, oref: Unique<()>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        let i = I::free_index(wsize);
        if i < self.buckets.len() {
            let node = transmute::<*mut (), *mut N::Value>(*oref);
            ptr::write(node, N::Value::default());
            self.buckets[i].push_front(N::Pointer::from_raw(node));
            None
        } else {
            Some(oref)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;
    use std::ptr::Unique;

    use gcref::GCRef;

    #[test]
    fn unique_is_null_ptr_optimized() {
        assert_eq!(size_of::<Option<Unique<()>>>(), size_of::<GCRef>());
    }
}