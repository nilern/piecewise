use std::mem;
use std::mem::transmute;
use core::nonzero::NonZero;
use std::ptr;
use std::ptr::Unique;
use std::marker::PhantomData;
use intrusive_collections::{LinkedList, LinkedListLink, Adapter, UnsafeRef, IntrusivePointer};

use allocator::{Allocator, MemoryPool};

/// Compute free list indices from object word counts.
/// # Laws
/// `alloc_index(x) >= free_index(x) | x > 0`
pub trait IndexCalculation {
    /// Get the index of the first freelist that can support allocation of `n` words.
    fn alloc_index(n: NonZero<usize>) -> usize;

    /// Get the index of the freelist where an object of `n` words should be freed to.
    fn free_index(n: NonZero<usize>) -> usize;
}

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

impl<N, I> Allocator for Bucketed<N, I> where N: Adapter<Link = LinkedListLink> + Default,
                                             N::Value: Default,
                                             I: IndexCalculation
{
    fn allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>) -> Option<Unique<()>> {
        // TODO: observe walign
        self.buckets.get_mut(I::alloc_index(wsize))
            .and_then(LinkedList::pop_front)
            .map(|v| unsafe { Unique::new(v.into_raw() as *mut ()) })
    }
}

impl<N, I> MemoryPool for Bucketed<N, I> where N: Adapter<Link = LinkedListLink> + Default,
                                               N::Value: Default,
                                               I: IndexCalculation
{
    unsafe fn release(&mut self, oref: Unique<()>, wsize: NonZero<usize>) -> Option<Unique<()>> {
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
