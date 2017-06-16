use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr;
use std::ptr::{Unique, Shared};
use std::cell::Cell;
use std::cmp::Ordering;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef, IntrusivePointer};

use util::{Init, Lengthy, Uninitialized, Span, CeilDiv};
use layout::{Block, GSize};
use block::BlockAllocator;
use descriptor::{Descriptor, SubDescr, MSBlockAdapter, LargeObjRopeAdapter};
use object_model::{LARGE_OBJ_THRESHOLD, ValueRef, PointyObject, Object};

// FIXME: observe alignment
// TODO: use singly linked .freelist?

pub struct Generation {
    freelist: LinkedList<SizedFreeAdapter>,
    bumper: Option<Span<Uninitialized<usize>>>,
    block_allocator: BlockAllocator,

    active_blocks: LinkedList<MSBlockAdapter>,
    large_objs: LinkedList<LargeObjRopeAdapter>,

    current_mark: u8,
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

            current_mark: 1,
            mark_stack: Vec::new()
        }
    }

    // TODO: leave breathing room (don't wait until the very last moment before returning `None`)
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

    /// Mark a single object reference.
    pub fn mark_ref(&mut self, oref: ValueRef) -> ValueRef {
        if let Some(ptr) = oref.ptr() {
            unsafe {
                if (**ptr).get_mark() != self.current_mark {
                    let pointy = oref.is_pointy();
                    (**ptr).set_mark(pointy, self.current_mark);
                    if pointy {
                        self.mark_stack.push(transmute(ptr));
                    }
                }
            }
        }
        oref
    }

    /// Collect garbage.
    /// # Safety
    /// Calling this without marking all the roots first leads to undefined behaviour.
    pub unsafe fn collect(&mut self) {
        self.mark_all();

        self.freelist.fast_clear();
        self.bumper = None;
        self.sweep_all();
        
        self.current_mark.wrapping_add(2);
    }

    fn release(&mut self, uptr: Unique<Uninitialized<usize>>, gsize: NonZero<GSize>) {
        // TODO: is gsize is large enough, recycle to bumper list instead (a la Immix)
        if *gsize >= GSize::of::<SizedFreeObj>() { // can we link it into the list?
            unsafe {
                let node = SizedFreeObj::init(transmute(uptr), transmute(gsize));
                self.freelist.push_back(UnsafeRef::from_raw(*node));
            }
        }
        // otherwise we just leak it (at least until the next sweep)
    }

    unsafe fn mark_all(&mut self) {
        while let Some(oref) = self.mark_stack.pop() {
            for fref in (*(*oref as *mut PointyObject)).fields_mut() {
                *fref = self.mark_ref(*fref);
            }
        }
    }

    unsafe fn sweep_all(&mut self) {
        for block in self.active_blocks.iter() {
            use self::SweepState::*;
            let mut state = Det;
            for (i, mark) in block.marks().iter().enumerate() {
                match state {
                    Det => if *mark == self.current_mark {
                        state = Obj;
                    } else {
                        state = Free(block.get_obj(i), NonZero::new(GSize::from(1)));
                    },
                    Free(ptr, len) => if *mark == self.current_mark {
                        if *len == GSize::from(Block::WSIZE) {
                            let descr = Unique::new(Descriptor::of(&*ptr) as _);
                            self.block_allocator.release(descr, NonZero::new(1));
                        } else if *len >= GSize::of::<SizedFreeObj>() { // FIXME: DRY
                            // TODO: is len is large enough, recycle as a bumper instead
                            let node = SizedFreeObj::init(Unique::new(ptr as _), transmute(len));
                            self.freelist.push_back(UnsafeRef::from_raw(*node));
                        }
                        // otherwise we just leak it (at least until the next sweep)
                        state = Obj;
                    } else {
                        state = Free(ptr, NonZero::new(*len + GSize::from(1)));
                    },
                    Obj if *mark == self.current_mark => state = Det,
                    Obj => ()
                }
            }
        }

        let mut cursor = self.large_objs.front_mut();
        while let Some(descr) = cursor.get() {
            if descr.get_mark() != self.current_mark {
                let udescr = Unique::new(cursor.remove().unwrap().into_raw() as _);
                self.block_allocator.release(udescr, descr.len());
            } else {
                cursor.move_next();
            }
        }
    }

    fn freelist_allocate(&mut self, walign: NonZero<usize>, wsize: NonZero<usize>)
        -> Option<Unique<Uninitialized<usize>>>
    {
        let mut cursor = self.freelist.front_mut();
        while let Some(node) = cursor.get() {
            match node.len().cmp(&*wsize) {
                Ordering::Equal =>
                    return cursor.remove()
                                 .map(|ptr| unsafe { Unique::new(ptr.into_raw() as _) }),
                Ordering::Greater =>
                    return node.split_off(*wsize)
                               .or_else(||
                                   cursor.remove()
                                         .map(|ptr| unsafe { Unique::new(ptr.into_raw() as _) })),
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
                    Unique::new((**rope).upcast().start() as _)
                })
        }
    }

    fn refill_bumper(&mut self) -> bool {
        if let Some(bumper) = self.bumper.take() {
            let slice = bumper.into_owned_slice();
            let len = slice.len();
            if len > 0 {
                self.release(slice.into_unique(), unsafe { NonZero::new(transmute(len)) });
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

#[derive(Clone, Copy)]
enum SweepState {
    Det,
    Free(*const Object, NonZero<GSize>),
    Obj
}

// ================================================================================================

#[derive(Debug)]
pub struct SizedFreeObj {
    link: LinkedListLink,
    len: Cell<usize>
}

intrusive_adapter!(pub SizedFreeAdapter = UnsafeRef<SizedFreeObj>:
                   SizedFreeObj { link: LinkedListLink });

impl SizedFreeObj {
    fn split_off(&self, n: usize) -> Option<Unique<Uninitialized<usize>>> {
        if self.len() - n >= usize::from(GSize::of::<SizedFreeObj>()) {
            let rem = self.len() - n;
            unsafe {
                let ptr = transmute::<_, *mut usize>(self).offset(rem as isize);
                self.set_len(rem);
                Some(Unique::new(ptr as _))
            }
        } else {
            None
        }
    }
}

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
