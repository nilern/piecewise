use intrusive_collections::LinkedList;

use block_arr;
use gcref::GCRef;

pub struct MSHeap {
    block_allocator: block_arr::Allocator,
    active_blocks: LinkedList<block_arr::ActiveAdapter>,
    mark_stack: Vec<GCRef>
}

impl MSHeap {
    pub fn new() -> MSHeap {
        MSHeap {
            block_allocator: block_arr::Allocator::new(),
            active_blocks: LinkedList::new(block_arr::ActiveAdapter::new()),
            mark_stack: Vec::new()
        }
    }

    pub fn allocate(n: usize) -> *mut () {
        unimplemented!()
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
                unsafe { (*block).sweep(/* freelists */); }
            }
        }
    }
}
