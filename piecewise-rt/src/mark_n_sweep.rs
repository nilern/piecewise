use std::ptr;
use std::ptr::Unique;
use std::mem;
use std::mem::transmute;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef};

use freelist;
use freelist::FreeList;
use block_arr;
use gcref::GCRef;

pub struct MSHeap {
    block_allocator: block_arr::Allocator,
    free_buckets: freelist::Bucketed<FreeAdapter>,
    free_fallback: LinkedList<SizedFreeAdapter>,
    active_blocks: LinkedList<block_arr::ActiveAdapter>,
    mark_stack: Vec<GCRef>
}

impl MSHeap {
    const NLISTS: usize = 16;

    pub fn new() -> MSHeap {
        let mut res = MSHeap {
            block_allocator: block_arr::Allocator::new(),
            free_buckets: freelist::Bucketed::new(),
            free_fallback: LinkedList::new(SizedFreeAdapter::new()),
            active_blocks: LinkedList::new(block_arr::ActiveAdapter::new()),
            mark_stack: Vec::new()
        };
        res
    }

    pub fn allocate_words(&mut self, n: usize) -> Unique<()> {
        loop {
            if let Some(res) = self.free_buckets.allocate_words(n) {
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

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::{FreeObj, SizedFreeObj};
    use gcref::GCRef;

    #[test]
    fn freeobj_size() {
        assert!(size_of::<FreeObj>() <= 2*size_of::<GCRef>());
        assert!(size_of::<SizedFreeObj>() <= 36*size_of::<GCRef>());
    }
}
