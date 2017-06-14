use core::nonzero::NonZero;
use std::mem::{transmute, size_of};
use std::ptr;
use std::ptr::{Unique, Shared};
use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef, IntrusivePointer};

use util::{Init, Lengthy, SplitOff, Uninitialized, OwnedSlice};
use freerope::FreeRope;
use block::{self, BlockAllocator};
use object_model::{LARGE_OBJ_THRESHOLD, ValueRef, PointyObject};

// TODO: use singly linked .freelist?

pub struct Generation {
    freelist: LinkedList<SizedFreeAdapter>,
    bumper: Option<Shared<MSBlock>>,
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
        } else {
            self.allocate_large(walign, wsize)
        }
        // self.freelist.allocate_at_least(wsize)
        //     .map(|ptr| transmute(ptr))
        //     .or_else(|| {
        //         self.ensure_bumper()
        //             .and_then(|b| unsafe { (*b.as_mut_ptr()).allocate(walign, wsize) })
        //     })
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
        unsafe {
            self.ensure_bumper();
            (**self.bumper.unwrap()).allocate(walign, wsize)
                .or_else(|| {
                    let bumper = &**self.bumper.unwrap();
                    let rest = bumper.split_off(bumper.allocatable());
                    let rlen = rest.len();
                    if rlen > 0 {
                        self.release(transmute(rest.into_unique()), NonZero::new(rlen));
                    }
                    self.bumper = None;
                    self.ensure_bumper();
                    (**self.bumper.unwrap()).allocate(walign, wsize)
                })
        }
    }

    fn allocate_large(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        unimplemented!()
    }

    fn ensure_bumper(&mut self) {
        if self.bumper.is_none() {
            if let Some(uptr) = self.block_allocator.allocate(unsafe { NonZero::new(1) }) {
                let block = MSBlock::init(uptr);
                self.bumper = Some(unsafe { Shared::new(*block) });
                self.active_blocks.push_back(unsafe { UnsafeRef::from_raw(*block) });
            }
        }
    }
}

// ================================================================================================

enum Block {
    MSBlock(MSBlock),
    LargeObjRope(LargeObjRope)
}

impl Block {
    fn start(&self) -> *const usize {
        unimplemented!()
    }
}

// ================================================================================================

struct MSBlock {
    link: LinkedListLink,
    free: RefCell<Unique<usize>>
}

intrusive_adapter!(MSBlockAdapter = UnsafeRef<MSBlock>: MSBlock { link: LinkedListLink });

impl MSBlock {
    fn init(uptr: Unique<Uninitialized<FreeRope>>) -> Unique<MSBlock> {
        let ptr: *mut Block = *uptr as _;
        unsafe {
            ptr::write(ptr, Block::MSBlock(MSBlock {
                link: LinkedListLink::default(),
                free: RefCell::new(Unique::new((*ptr).start() as _))
            }));
            if let &Block::MSBlock(ref block) = &*ptr {
                Unique::new(transmute(block))
            } else {
                unreachable!()
            }
        }
    }

    fn upcast(&self) -> &Block {
        unimplemented!()
    }

    fn allocatable(&self) -> usize {
        ((self.upcast().start() as usize + block::SIZE) - **self.free.borrow() as usize)
            / size_of::<usize>()
    }

    fn allocate(&self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        if self.allocatable() >= *wsize {
            unsafe {
                let ptr: *mut usize = **self.free.borrow() as _;
                let uptr = Unique::new(ptr as _);
                *self.free.borrow_mut() =
                    Unique::new(ptr.offset((*wsize * size_of::<usize>()) as isize));
                Some(uptr)
            }
        } else {
            None
        }
    }

    fn sweep(&self, block_allocator: &mut BlockAllocator,
                    freelist: &mut LinkedList<SizedFreeAdapter>)
    {
        unimplemented!()
    }
}

impl SplitOff<OwnedSlice<usize>> for MSBlock {
    unsafe fn split_off(&self, n: usize) -> OwnedSlice<usize> {
        unimplemented!()
    }
}

// ================================================================================================

struct LargeObjRope {
    link: LinkedListLink
}

intrusive_adapter!(LargeObjRopeAdapter = UnsafeRef<LargeObjRope>:
                   LargeObjRope { link: LinkedListLink });

// ================================================================================================

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

impl SplitOff<Unique<Uninitialized<usize>>> for SizedFreeObj {
    unsafe fn split_off(&self, n: usize) -> Unique<Uninitialized<usize>> {
        assert!(n <= self.len());
        let rem = self.len() - n;
        unsafe {
            let ptr = transmute::<_, *mut usize>(self).offset(rem as isize);
            self.set_len(rem);
            Unique::new(ptr as _)
        }
    }
}
