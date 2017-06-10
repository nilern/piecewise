use std::mem::transmute;
use core::nonzero::NonZero;
use std::ptr;
use std::ptr::Unique;
use std::marker::PhantomData;
use intrusive_collections::{LinkedList, LinkedListLink, Adapter,
                            SinglyLinkedList, SinglyLinkedListLink,
                            IntrusivePointer};

use util::Lengthy;
use allocator::{OverAllocator, MemoryPool};

// ================================================================================================

/// Compute free list indices from object word counts.
/// # Laws
/// `alloc_index(x) >= free_index(x) | x > 0`
pub trait IndexCalculation {
    /// Get the index of the first freelist that can support allocation of `n` words.
    fn alloc_index(n: NonZero<usize>) -> usize;

    /// Get the index of the freelist where an object of `n` words should be freed to.
    fn free_index(n: NonZero<usize>) -> usize;
}

// ================================================================================================

struct SizeClass<N, I> where N: Adapter<Link = SinglyLinkedListLink>
{
    index: usize,
    list: SinglyLinkedList<N>,
    indec_calc: PhantomData<I>
}

impl<N, I> SizeClass<N, I> where N: Adapter<Link = SinglyLinkedListLink> + Default
{
    fn new(index: usize) -> Self {
        SizeClass {
            index: index,
            list: SinglyLinkedList::new(N::default()),
            indec_calc: PhantomData::default()
        }
    }

    fn is_empty(&self) -> bool { self.list.is_empty() }
}

impl<N, I> OverAllocator for SizeClass<N, I> where N: Adapter<Link = SinglyLinkedListLink>,
                                                   I: IndexCalculation
{
    fn allocate_at_least(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        if I::alloc_index(wsize) <= self.index {
            self.list.pop_front().map(|v| unsafe { Unique::new(v.into_raw() as *mut ()) })
        } else {
            None
        }
    }
}

impl<N, I> MemoryPool for SizeClass<N, I> where N: Adapter<Link = SinglyLinkedListLink>,
                                                N::Value: Default,
                                                I: IndexCalculation
{
    unsafe fn try_release(&mut self, oref: Unique<()>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        if I::free_index(wsize) == self.index {
            let node = transmute::<*mut (), *mut N::Value>(*oref);
            ptr::write(node, N::Value::default());
            self.list.push_front(N::Pointer::from_raw(node));
            None
        } else {
            Some(oref)
        }
    }
}

// ================================================================================================

/// First-fit freelist
pub struct FirstFit<N> where N: Adapter<Link = LinkedListLink> {
    list: LinkedList<N>
}

impl<N> FirstFit<N> where N: Adapter<Link = LinkedListLink> + Default
{
    /// Create a new first-fit freelist
    pub fn new() -> Self {
        FirstFit {
            list: LinkedList::new(N::default())
        }
    }
}

impl<N> OverAllocator for FirstFit<N> where N: Adapter<Link = LinkedListLink>,
                                            N::Value: Lengthy
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

impl<N> MemoryPool for FirstFit<N> where N: Adapter<Link = LinkedListLink>,
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

// ================================================================================================

// MAYBE: Use typelevel numbers to exchange the Vec for a size-generic array
/// Bucketed freelist
pub struct Bucketed<N, I> where N: Adapter<Link = SinglyLinkedListLink>
{
    buckets: Vec<SizeClass<N, I>>
}

impl<N, I> Bucketed<N, I> where N: Adapter<Link = SinglyLinkedListLink> + Default
{
    /// Create a new bucketed freelist with `n` buckets.
    pub fn new(n: usize) -> Self {
        Bucketed {
            buckets: (0..n).map(SizeClass::new).collect()
        }
    }
}

impl<N, I> OverAllocator for Bucketed<N, I> where N: Adapter<Link = SinglyLinkedListLink>
                                                     + Default,
                                                  I: IndexCalculation
{
    fn allocate_at_least(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        // TODO: observe walign
        let start = I::alloc_index(wsize);
        self.buckets.get_mut(start)
            .and_then(|b| b.allocate_at_least(walign, wsize))
            .or_else(|| self.buckets[start + 1..].iter_mut()
                            .find(|b| !b.is_empty())
                            .and_then(|b| b.allocate_at_least(walign, wsize)))
    }
}

impl<N, I> MemoryPool for Bucketed<N, I> where N: Adapter<Link = SinglyLinkedListLink>,
                                               N::Value: Default,
                                               I: IndexCalculation
{
    unsafe fn try_release(&mut self, oref: Unique<()>, wsize: NonZero<usize>)
        -> Option<Unique<()>>
    {
        self.buckets.get_mut(I::free_index(wsize))
            .and_then(|b| b.try_release(oref, wsize))
    }
}

// ================================================================================================

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
