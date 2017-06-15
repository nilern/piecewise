use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr;
use std::ptr::{Unique, Shared};
use std::cell::Cell;
use std::cmp::Ordering;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef, IntrusivePointer};

use util::{Init, Lengthy, SplitOff, Uninitialized, Span, CeilDiv};
use layout::Block;
use block::BlockAllocator;
use descriptor::{MSBlockAdapter, LargeObjRopeAdapter};
use object_model::{LARGE_OBJ_THRESHOLD, ValueRef, PointyObject};

// TODO: use singly linked .freelist?

pub struct Generation {
    freelist: LinkedList<SizedFreeAdapter>,
    bumper: Option<Span<Uninitialized<usize>>>,
    block_allocator: BlockAllocator,

    active_blocks: LinkedList<MSBlockAdapter>,
    large_objs: LinkedList<LargeObjRopeAdapter>,
    mark_stack: Vec<Shared<PointyObject>>
}

impl Generation {
    pub fn new(max_heap: usize) -> Self {
        Generation {
            freelist: LinkedList::new(SizedFreeAdapter::new()),
            bumper: None,
            block_allocator: BlockAllocator::new(max_heap),

            active_blocks: LinkedList::new(MSBlockAdapter::new()),
            large_objs: LinkedList::new(LargeObjRopeAdapter::new()),
            mark_stack: Vec::new()
        }
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

    pub fn allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        if *wsize < LARGE_OBJ_THRESHOLD {
            self.freelist_allocate(walign, wsize)
                .or_else(|| self.sequential_allocate(walign, wsize))
                .or_else(|| {
                    if self.refill_bumper() {
                        self.sequential_allocate(walign, wsize)
                    } else {
                        None
                    }
                })
        } else {
            self.allocate_large(walign, wsize)
        }
    }

    /// Collect garbage.
    /// # Safety
    /// Calling this without marking all the roots first leads to undefined behaviour.
    pub unsafe fn collect(&mut self) {
        self.mark_all();
        self.sweep_all();
    }

    fn release(&mut self, uptr: Unique<Uninitialized<usize>>, wsize: NonZero<usize>) {
        unimplemented!()
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
            block.sweep(&mut self.block_allocator, &mut self.freelist);
        }
        // TODO: sweep large_objs
    }

    fn freelist_allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        let mut cursor = self.freelist.cursor_mut();
        while let Some(node) = cursor.get() {
            match node.len().cmp(&*wsize) {
                Ordering::Equal =>
                    return cursor.remove()
                                 .map(|ptr| unsafe { Unique::new(ptr.into_raw() as _) }),
                Ordering::Greater => return Some(unsafe { transmute(node.split_off(*wsize)) }),
                Ordering::Less => cursor.move_next()
            }
        }
        None
    }

    fn sequential_allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        self.bumper.as_mut().and_then(|bumper| bumper.split_off(*wsize))
    }

    fn allocate_large(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        unsafe {
            let bsize = NonZero::new((*wsize).ceil_div(Block::WSIZE));
            self.block_allocator.alloc_large_obj_rope(bsize)
                .map(|rope| {
                    self.large_objs.push_back(UnsafeRef::from_raw(*rope));
                    Unique::new((**rope).start_mut() as _)
                })
        }
    }

    fn refill_bumper(&mut self) -> bool {
        if let Some(bumper) = self.bumper.take() {
            let slice = bumper.into_owned_slice();
            let len = slice.len();
            if len > 0 {
                self.release(slice.into_unique(), unsafe { NonZero::new(len) });
            }
        }
        if let Some((block, bumper)) = self.block_allocator.alloc_ms_block() {
            self.bumper = Some(bumper);
            self.active_blocks.push_back(unsafe { UnsafeRef::from_raw(*block) });
            true
        } else {
            false
        }
    }
}

// ================================================================================================

#[derive(Debug)]
pub struct SizedFreeObj {
    link: LinkedListLink,
    len: Cell<usize>
}

intrusive_adapter!(pub SizedFreeAdapter = UnsafeRef<SizedFreeObj>:
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

impl SplitOff<Unique<Uninitialized<usize>>> for SizedFreeObj {
    unsafe fn split_off(&self, n: usize) -> Unique<Uninitialized<usize>> {
        assert!(n <= self.len());

        let rem = self.len() - n;
        let ptr = transmute::<_, *mut usize>(self).offset(rem as isize);
        self.set_len(rem);
        Unique::new(ptr as _)
    }
}
