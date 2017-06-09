use std::ptr;
use std::ptr::Unique;
use std::mem;
use std::mem::transmute;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef};

use freelist;
use freelist::{FreeList, IndexCalculation};
use block_arr;
use gcref::GCRef;

pub struct MSHeap {
    block_allocator: block_arr::Allocator,
    free_buckets: freelist::Bucketed<FreeAdapter, ObjIndexCalc>,
    free_fallback: LinkedList<SizedFreeAdapter>,
    active_blocks: LinkedList<block_arr::ActiveAdapter>,
    mark_stack: Vec<GCRef>
}

impl MSHeap {
    const NLISTS: usize = 16;

    pub fn new() -> MSHeap {
        let mut res = MSHeap {
            block_allocator: block_arr::Allocator::new(),
            free_buckets: freelist::Bucketed::new(Self::NLISTS),
            free_fallback: LinkedList::new(SizedFreeAdapter::new()),
            active_blocks: LinkedList::new(block_arr::ActiveAdapter::new()),
            mark_stack: Vec::new()
        };
        res
    }

    pub fn allocate_words(&mut self, n: usize) -> Unique<()> {
        loop {
            if let Some(res) = unsafe { self.free_buckets.allocate(n) } {
                return res;
            } else {
                let mut cursor = self.free_fallback.cursor_mut();
                while let Some(fobj) = cursor.get() {
                    if fobj.len == n {
                        cursor.remove();
                        return unsafe { transmute(fobj) };
                    } else if fobj.len > n {
                        cursor.remove();
                        let fobj: *mut SizedFreeObj = unsafe { transmute(fobj) };
                        let res = unsafe { (*fobj).split_off(n) };
                        // self.push_front(fobj); FIXME
                        return unsafe { Unique::new(fobj as *mut ()) };
                    }
                }

                let block = self.block_allocator.allocate(1);
                // self.active_blocks.push_back(block); FIXME
                // (*block).sweep(/* free_buckets */);
            }
        }
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

    fn push_front(&mut self, sfref: *mut SizedFreeObj) {
        unimplemented!()
        // if let Some(i) = MSHeap::free_index(unsafe { (*sfref).len() }) {
        //     let fref = unsafe { transmute(sfref) };
        //     unsafe { ptr::write(fref, FreeObj::default()) };
        //     self.free_buckets[i].push_front(unsafe { UnsafeRef::from_raw(fref) });
        // } else {
        //     self.free_fallback.push_front(unsafe { UnsafeRef::from_raw(sfref) });
        // }
    }
}

#[derive(Default)]
struct FreeObj {
    link: LinkedListLink
}

intrusive_adapter!(FreeAdapter = UnsafeRef<FreeObj>: FreeObj { link: LinkedListLink });

struct SizedFreeObj {
    len: usize,
    link: LinkedListLink
}

intrusive_adapter!(SizedFreeAdapter = UnsafeRef<SizedFreeObj>:
                   SizedFreeObj { link: LinkedListLink });

impl SizedFreeObj {
    fn len(&self) -> usize { self.len }

    fn split_off(&mut self, n: usize) -> *mut () {
        let offset = self.len() - n;
        self.len = offset;
        (unsafe { transmute::<_, usize>(self) } + offset) as _
    }
}

struct ObjIndexCalc;

impl IndexCalculation for ObjIndexCalc {
    unsafe fn alloc_index(n: usize) -> usize {
        if n <= 8 {
            n - 1         // 1 -> 0, .., 8 -> 7, ...
        } else if n <= 16 {
            (n + 7) >> 1  // 9 -> 8, 10 -> 8, .., 15 -> 11, 16 -> 11, ...
        } else {
            (n + 31) >> 2 // 17 -> 12, .., 20 -> 12, ..., 29 -> 15, .., 32 -> 15, ...
        }
    }

    unsafe fn free_index(n: usize) -> usize {
        if n <= 8 {
            n - 1        // 1 -> 0, .., 8 -> 7, ...
        } else if n <= 16 {
            (n >> 1) + 3 // 9 -> 7, 10 -> 8, 11 -> 8, .., 15 -> 10, 16 -> 11, ...
        } else {
            (n >> 2) + 7 // 17 -> 11, .., 19 -> 11, ..., 32 -> 15, .., 35 -> 15, ...
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;
    use quickcheck::TestResult;

    use super::{FreeObj, SizedFreeObj, ObjIndexCalc};
    use freelist::IndexCalculation;
    use gcref::GCRef;

    #[test]
    fn freeobj_size() {
        assert!(size_of::<FreeObj>() <= 2*size_of::<GCRef>());
        assert!(size_of::<SizedFreeObj>() <= 36*size_of::<GCRef>());
    }

    quickcheck! {
        fn alloc_free_indices(n: usize) -> TestResult {
            if n != 0 {
                unsafe {
                    TestResult::from_bool(
                        ObjIndexCalc::alloc_index(n) >= ObjIndexCalc::free_index(n))
                }
            } else {
                TestResult::discard()
            }
        }
    }
}
