use std::mem;
use std::mem::transmute;
use std::ptr;
use std::ptr::Unique;
use std::marker::PhantomData;
use intrusive_collections::{LinkedList, LinkedListLink, Adapter, UnsafeRef, IntrusivePointer};

/// Freelist interface
pub trait FreeList {
    /// Try to allocate `n` words from this freelist.
    /// # Safety
    /// `n` must not be zero.
    unsafe fn allocate(&mut self, n: usize) -> Option<Unique<()>>;

    /// Release `n` words of memory behind `fobj` into this list.
    /// # Safety
    /// Overestimating `n` leads to undefined behaviour. (Underestimates just leak memory).
    /// `n` must be large enough to hold the link pointers used by Self.
    unsafe fn release(&mut self, fobj: Unique<()>, n: usize) -> bool;
}

/// Compute free list indices from object word counts.
/// # Laws
/// `alloc_index(x) >= free_index(x) | x > 0`
pub trait IndexCalculation {
    /// Get the index of the first freelist that can support allocation of `n` words.
    unsafe fn alloc_index(n: usize) -> usize;

    /// Get the index of the freelist where an object of `n` words should be freed to.
    unsafe fn free_index(n: usize) -> usize;
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

impl<N, I> FreeList for Bucketed<N, I> where N: Adapter<Link = LinkedListLink> + Default,
                                             N::Value: Default,
                                             I: IndexCalculation
{
    unsafe fn allocate(&mut self, n: usize) -> Option<Unique<()>> {
        self.buckets.get_mut(I::alloc_index(n))
            .and_then(LinkedList::pop_front)
            .map(|v| Unique::new(v.into_raw() as *mut ()))
    }

    unsafe fn release(&mut self, fobj: Unique<()>, n: usize) -> bool {
        let i = I::free_index(n);
        if i < self.buckets.len() {
            let node = transmute::<*mut (), *mut N::Value>(*fobj);
            ptr::write(node, N::Value::default());
            self.buckets[i].push_front(N::Pointer::from_raw(node));
            true
        } else {
            false
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
