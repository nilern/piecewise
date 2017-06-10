use core::nonzero::NonZero;
use std::ptr;
use std::ptr::Unique;
use std::mem;
use std::mem::transmute;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef};

use util::Lengthy;
use allocator::{Allocator, OverAllocator, MemoryPool, AbsorbentMemoryPool};
use freelist;
use freelist::IndexCalculation;
use block_arr;
use gcref::GCRef;

pub struct MSHeap {
    block_allocator: block_arr::Allocator,
    free_buckets: freelist::Bucketed<FreeAdapter, ObjIndexCalc>,
    free_fallback: freelist::FirstFit<SizedFreeAdapter>,
    active_blocks: LinkedList<block_arr::ActiveAdapter>,
    mark_stack: Vec<GCRef>
}

impl MSHeap {
    const NLISTS: usize = 16;

    pub fn new() -> MSHeap {
        let mut res = MSHeap {
            block_allocator: block_arr::Allocator::new(),
            free_buckets: freelist::Bucketed::new(Self::NLISTS),
            free_fallback: freelist::FirstFit::new(),
            active_blocks: LinkedList::new(block_arr::ActiveAdapter::new()),
            mark_stack: Vec::new()
        };
        res
    }

    pub fn collect(&mut self) {
        self.mark();
        self.sweep();
    }

    fn mark(&mut self) {
        while let Some(oref) = self.mark_stack.pop() {
            for fref in oref.fields() {
                if !fref.is_marked() {
                    fref.mark();
                    self.mark_stack.push(fref);
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
    fn allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>) -> Option<Unique<()>> {
        // TODO: what if wsize >= Block::SIZE?
        self.free_buckets.allocate_at_least(walign, wsize)
            .or_else(|| self.free_fallback.allocate_at_least(walign, wsize))
            // TODO: split excess off and release it, allocate blocks if free_fallback fails
    }
}

impl AbsorbentMemoryPool for MSHeap {
    unsafe fn release(&mut self, oref: Unique<()>, wsize: NonZero<usize>) {
        // TODO: what if wsize >= Block::SIZE?
        self.free_buckets.try_release(oref, wsize)
            .and_then(|oref| self.free_fallback.try_release(oref, wsize))
            .map_or((), |oref| unimplemented!())
    }
}

#[derive(Default)]
struct FreeObj {
    link: LinkedListLink
}

intrusive_adapter!(FreeAdapter = UnsafeRef<FreeObj>: FreeObj { link: LinkedListLink });


#[derive(Default)]
struct SizedFreeObj {
    len: usize,
    link: LinkedListLink
}

intrusive_adapter!(SizedFreeAdapter = UnsafeRef<SizedFreeObj>:
                   SizedFreeObj { link: LinkedListLink });

impl SizedFreeObj {
    fn split_off(&mut self, n: usize) -> *mut () {
        let offset = self.len() - n;
        self.len = offset;
        (unsafe { transmute::<_, usize>(self) } + offset) as _
    }
}

impl Lengthy for SizedFreeObj {
    fn len(&self) -> usize { self.len }
    fn set_len(&mut self, new_len: usize) { self.len = new_len }
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
}

#[cfg(test)]
mod tests {
    use core::nonzero::NonZero;
    use std::mem::size_of;
    use quickcheck::TestResult;

    use super::{FreeObj, SizedFreeObj, ObjIndexCalc};
    use freelist::IndexCalculation;
    use gcref::GCRef;

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