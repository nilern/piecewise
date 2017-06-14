use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr;
use std::ptr::{Unique, Shared};
use std::cell::Cell;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef};

use util::{Init, Lengthy, SplitOff, Uninitialized};
use block::BlockAllocator;
use freelist::FirstFitTail;
use object_model::{ValueRef, PointyObject};

pub struct Generation {
    freelist: FirstFitTail<SizedFreeAdapter>,
    bumper: Option<Shared<ActiveRope>>,
    block_allocator: BlockAllocator,

    active_blocks: LinkedList<Shared<ActiveRope>>,
    mark_stack: Vec<Shared<PointyObject>>
}

impl Generation {
    pub fn new(max_heap: usize) -> Self {
        Generation {
            freelist: FirstFitTail::new(),
            bumper: None,
            block_allocator: BlockAllocator::new(max_heap),

            active_blocks: LinkedList::new(ActiveRopeAdapter::new()),
            mark_stack: Vec::new()
        }
    }

    pub fn allocate_at_least(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        self.freelist.allocate_at_least(wsize)
            .map(|ptr| transmute(ptr))
            .or_else(|| {
                self.ensure_bumper()
                    .and_then(|b| unsafe { (*b.as_mut_ptr()).allocate(walign, wsize) })
            })
    }

    /// Mark a single object reference.
    pub fn mark_ref(&mut self, oref: ValueRef) {
        unsafe {
            if let Some(ptr) = oref.ptr() {
                if !(**ptr).is_marked() {
                    (**ptr).mark();
                    if oref.is_pointy() {
                        self.mark_stack.push(transmute(ptr));
                    }
                }
            }
        }
    }

    /// Collect garbage.
    /// # Safety
    /// Calling this without marking all the roots first leads to undefined behaviour.
    pub unsafe fn collect(&mut self) {
        self.mark_all();
        self.sweep_all();
    }

    unsafe fn mark_all(&mut self) {
        while let Some(oref) = self.mark_stack.pop() {
            for fref in (**oref).fields() {
                self.mark_ref(*fref);
            }
        }
    }

    unsafe fn sweep_all(&mut self) {
        for block in self.active_blocks.iter() {
            block.sweep(&mut self.freelist);
        }
    }

    unsafe fn sweep_block(&mut self, block: &block::Descriptor) {
        unimplemented!()
    }

    fn ensure_bumper(&mut self) -> Option<Shared<block::Descriptor>> {
        if self.bumper.is_none() {
            if let Some(uptr) = self.block_allocator.allocate(1) {
                let ptr = *uptr;
                unsafe {
                    ptr::write(ptr as _, block::Descriptor::new());
                    self.active_blocks.push_back(UnsafeRef::from_raw(ptr as _));
                    self.bumper = Some(Shared::new(ptr as _));
                }
            }
        }
        self.bumper
    }

    fn ensure_bumper(&mut self) -> Option<Shared<block::Descriptor>> {
        if self.bumper.is_none() {
            if let Some(uptr) = self.block_allocator.allocate(1) {
                let ptr = *uptr;
                unsafe {
                    ptr::write(ptr as _, block::Descriptor::new());
                    self.active_blocks.push_back(UnsafeRef::from_raw(ptr as _));
                    self.bumper = Some(Shared::new(ptr as _));
                }
            }
        }
        self.bumper
    }
}

#[derive(Debug)]
struct SizedFreeObj {
    link: LinkedListLink,
    len: Cell<usize>
}

intrusive_adapter!(SizedFreeAdapter = UnsafeRef<SizedFreeObj>:
                   SizedFreeObj { link: LinkedListLink });

impl Init for SizedFreeObj {
    unsafe fn init(uptr: Unique<Uninitialized<Self>>, len: NonZero<usize>) -> Unique<Self> {
        let ptr = *uptr as *mut SizedFreeObj;
        ptr::write(ptr, SizedFreeObj {
            link: LinkedListLink::default(),
            len: Cell::new(*len)
        });
        Unique::new(ptr)
    }
}

impl Lengthy for SizedFreeObj {
    fn len(&self) -> usize { self.len.get() }
    fn set_len(&self, new_len: usize) { self.len.set(new_len) }
}

impl SplitOff for SizedFreeObj {
    type R = usize;

    fn split_off(&self, n: usize) -> Unique<Uninitialized<usize>> {
        assert!(n <= self.len());
        let rem = self.len() - n;
        unsafe {
            let ptr = transmute::<_, *mut usize>(self).offset(rem as isize);
            self.set_len(rem);
            Unique::new(ptr as _)
        }
    }
}
