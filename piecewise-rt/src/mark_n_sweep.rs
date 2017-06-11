use core::nonzero::NonZero;
use std::ptr::{Unique, Shared};
use std::mem::{size_of, transmute};
use std::cell::Cell;
use intrusive_collections::{SinglyLinkedListLink, LinkedList, LinkedListLink, UnsafeRef};

use util::Lengthy;
use allocator::{Allocator, OverAllocator, MemoryPool, AbsorbentMemoryPool, SplitOff};
use freelist;
use freelist::IndexCalculation;
use block_arr;
use object_model::{GCRef, Object};

pub struct MSHeap {
    block_allocator: block_arr::Allocator,
    free_buckets: freelist::Bucketed<FreeAdapter, ObjIndexCalc>,
    free_fallback: freelist::FirstFit<SizedFreeAdapter>,
    active_blocks: LinkedList<block_arr::ActiveAdapter>, // FIXME: LinkedList<BlockAdapter>
    mark_stack: Vec<Shared<Object>>
}

impl MSHeap {
    const NLISTS: usize = 16;

    pub fn new() -> MSHeap {
        MSHeap {
            block_allocator: block_arr::Allocator::new(),
            free_buckets: freelist::Bucketed::new(Self::NLISTS),
            free_fallback: freelist::FirstFit::new(ObjIndexCalc::max(Self::NLISTS)),
            active_blocks: LinkedList::new(block_arr::ActiveAdapter::new()),
            mark_stack: Vec::new()
        }
    }

    pub fn collect(&mut self) {
        self.mark();
        self.sweep();
    }

    fn mark(&mut self) {
        while let Some(oref) = self.mark_stack.pop() {
            unsafe {
                for fref in (**oref).fields() {
                    if let Some(ptr) = fref.ptr() {
                        if (**ptr).is_marked() {
                            (**ptr).mark();
                            self.mark_stack.push(ptr);
                        }
                    }
                }
            }
        }
    }

    fn sweep(&mut self) {
        for block_arr in self.active_blocks.iter() {
            for block in block_arr.blocks() {
                unsafe { (*block).sweep(/* free_buckets */); }
            }
        }
    }
}

impl Allocator for MSHeap {
    fn allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<usize>>
    {
        // TODO: what if wsize >= Block::SIZE?
        self.free_buckets.allocate_at_least(walign, wsize)
            .or_else(|| self.free_fallback.allocate_at_least(walign, wsize))
            .map(|mut words| {
                let excess_len = words.len() - *wsize;
                if excess_len > 0 {
                    let excess = words.split_off(excess_len);
                    unsafe { self.release(excess.into_unique(), NonZero::new(excess_len)); }
                }
                words.into_unique()
            })
            // TODO: allocate blocks if free_fallback fails
    }
}

impl AbsorbentMemoryPool for MSHeap {
    unsafe fn release(&mut self, oref: Unique<usize>, wsize: NonZero<usize>) {
        // TODO: what if wsize >= Block::SIZE?
        self.free_buckets.try_release(oref, wsize)
            .and_then(|oref| self.free_fallback.try_release(oref, wsize))
            .map_or((), |oref| unimplemented!())
    }
}

#[derive(Default)]
struct FreeObj {
    link: SinglyLinkedListLink
}

intrusive_adapter!(FreeAdapter = UnsafeRef<FreeObj>: FreeObj { link: SinglyLinkedListLink });


#[derive(Default)]
struct SizedFreeObj {
    len: Cell<usize>,
    link: LinkedListLink
}

intrusive_adapter!(SizedFreeAdapter = UnsafeRef<SizedFreeObj>:
                   SizedFreeObj { link: LinkedListLink });

impl Lengthy for SizedFreeObj {
    fn len(&self) -> usize { self.len.get() }
    fn set_len(&mut self, new_len: usize) { self.len.set(new_len) }
}

impl SplitOff for SizedFreeObj {
    fn split_off(&self, wsize: usize) -> Unique<usize> {
        assert!(wsize <= self.len.get() + size_of::<SizedFreeObj>()/size_of::<GCRef>());
        let rem = self.len.get() - wsize;
        self.len.set(rem);
        unsafe { Unique::new(transmute::<_, *mut usize>(self).offset(rem as isize)) }
    }
}

struct ObjIndexCalc;

impl IndexCalculation for ObjIndexCalc {
    fn alloc_index(n: NonZero<usize>) -> usize {
        if *n <= 8 {
            *n - 1         // 1 -> 0, .., 8 -> 7, ...
        } else if *n <= 16 {
            (*n + 7) >> 1  // 9 -> 8, 10 -> 8, .., 15 -> 11, 16 -> 11, ...
        } else {
            (*n + 31) >> 2 // 17 -> 12, .., 20 -> 12, ..., 29 -> 15, .., 32 -> 15, ...
        }
    }

    fn free_index(n: NonZero<usize>) -> usize {
        if *n <= 8 {
            *n - 1        // 1 -> 0, .., 8 -> 7, ...
        } else if *n <= 16 {
            (*n >> 1) + 3 // 9 -> 7, 10 -> 8, 11 -> 8, .., 15 -> 10, 16 -> 11, ...
        } else {
            (*n >> 2) + 7 // 17 -> 11, .., 19 -> 11, ..., 32 -> 15, .., 35 -> 15, ...
        }
    }

    fn min(index: usize) -> usize {
        unimplemented!()
    }

    fn max(index: usize) -> usize {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use core::nonzero::NonZero;
    use std::mem::size_of;
    use quickcheck::TestResult;

    use super::{FreeObj, SizedFreeObj, ObjIndexCalc};
    use freelist::IndexCalculation;
    use object_model::GCRef;

    #[test]
    fn freeobj_size() {
        assert!(size_of::<FreeObj>() <= 2*size_of::<GCRef>());
        assert!(size_of::<SizedFreeObj>() <= 3*size_of::<GCRef>());
    }

    quickcheck! {
        fn alloc_free_indices(n: usize) -> TestResult {
            if n != 0 {
                let n_ = unsafe { NonZero::new(n) };
                TestResult::from_bool(ObjIndexCalc::alloc_index(n_)
                                      >= ObjIndexCalc::free_index(n_))
            } else {
                TestResult::discard()
            }
        }
    }
}
