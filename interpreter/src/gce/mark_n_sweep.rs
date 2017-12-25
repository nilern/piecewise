use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::{self, Unique};
use std::cell::Cell;
use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef, IntrusivePointer};

use gce::{Object, ObjectRef, PointyObjectRef};
use gce::util::{Uninitialized, Initializable, CeilDiv, Foam, Span, AllocSat};
use gce::layout::{Block, GSize};
use gce::block::BlockAllocator;
use gce::descriptor::{Descriptor, SubDescr, MSBlockAdapter, LargeObjRopeAdapter};

const LARGE_OBJ_THRESHOLD: usize = Block::WSIZE * 8 / 10;

// FIXME: observe alignment
// TODO: use singly linked .freelist?

pub struct Generation<ORef: ObjectRef> {
    freelist: LinkedList<SizedFreeAdapter>,
    bumper: Option<Span<Uninitialized<usize>>>,
    block_allocator: BlockAllocator,

    active_blocks: LinkedList<MSBlockAdapter>,
    large_objs: LinkedList<LargeObjRopeAdapter>,

    current_mark: u8,
    mark_stack: Vec<ORef::PORef>
}

impl<ORef, PORef> Generation<ORef>
    where ORef: ObjectRef<PORef=PORef>, PORef: PointyObjectRef<ORef=ORef>
{
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
    pub unsafe fn allocate<T>(&mut self, galign: NonZero<GSize>, gsize: NonZero<GSize>)
        -> Option<Initializable<T>>
    {
        let res = if usize::from(gsize.get()) < LARGE_OBJ_THRESHOLD {
            self.freelist_allocate(galign, gsize)
                .or_else(|| self.sequential_allocate(galign, gsize))
                .or_else(|| if self.refill_bumper() {
                    self.sequential_allocate(galign, gsize)
                } else {
                    None
                })
        } else {
            self.allocate_large(galign, gsize)
        };
        transmute(res)
    }

    /// Mark a single object reference.
    pub fn mark_ref(&mut self, oref: ORef) -> ORef {
        if let Some(ptr) = oref.ptr() {
            let mut descr = Descriptor::of(ptr.as_ptr());
            let descr = unsafe { descr.as_mut() };

            if descr.mark_of(ptr) != self.current_mark {
                unsafe { descr.set_mark_of(ptr, self.current_mark, ptr.as_ref().gsize()) };

                if let Some(pref) = oref.pointy_ref() {
                    self.mark_stack.push(pref);
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

    fn release(&mut self, uptr: Initializable<usize>, gsize: NonZero<GSize>) {
        // TODO: is gsize is large enough, recycle to bumper list instead (a la Immix)
        if gsize.get() >= GSize::of::<SizedFreeObj>() { // can we link it into the list?
            unsafe {
                let node = SizedFreeObj::init(transmute(uptr), transmute(gsize));
                self.freelist.push_back(UnsafeRef::from_raw(node.as_ptr()));
            }
        }
        // otherwise we just leak it (at least until the next sweep)
    }

    unsafe fn mark_all(&mut self) {
        while let Some(poref) = self.mark_stack.pop() {
            for fref in poref.obj_refs() {
                *fref = self.mark_ref(*fref);
            }
        }
    }

    unsafe fn sweep_all(&mut self) {
        for block in self.active_blocks.iter() {
            use self::SweepState::*;
            let mut state: SweepState<ORef::Obj> = Det;
            for (i, mark) in block.marks().iter().enumerate() {
                match state {
                    Det => if *mark == self.current_mark {
                        state = Obj;
                    } else {
                        state = Free(block.get_obj(i), NonZero::new_unchecked(GSize::from(1)));
                    },
                    Free(ptr, len) => if *mark == self.current_mark {
                        if len.get() == GSize::from(Block::WSIZE) {
                            let descr = Unique::new_unchecked(Descriptor::of(ptr).as_ptr() as _);
                            self.block_allocator.release(descr, NonZero::new_unchecked(1));
                        } else if len.get() >= GSize::of::<SizedFreeObj>() { // FIXME: DRY
                            // TODO: is len is large enough, recycle as a bumper instead
                            let node = SizedFreeObj::init(Unique::new_unchecked(ptr as _),
                                                          transmute(len));
                            self.freelist.push_back(UnsafeRef::from_raw(node.as_ptr()));
                        }
                        // otherwise we just leak it (at least until the next sweep)
                        state = Obj;
                    } else {
                        state = Free(ptr, NonZero::new_unchecked(len.get() + GSize::from(1)));
                    },
                    Obj if *mark == self.current_mark => state = Det,
                    Obj => ()
                }
            }
            // FIXME: if state is Free(..) at this point an additional release is needed
        }

        let mut cursor = self.large_objs.front_mut();
        while let Some(descr) = cursor.get() {
            if descr.get_mark() != self.current_mark {
                let udescr = Unique::new_unchecked(cursor.remove().unwrap().into_raw() as _);
                self.block_allocator.release(udescr, descr.len());
            } else {
                cursor.move_next();
            }
        }
    }

    fn freelist_allocate(&mut self, galign: NonZero<GSize>, gsize: NonZero<GSize>)
        -> Option<Initializable<usize>>
    {
        use self::AllocSat::*;

        let gsize = usize::from(gsize.get());
        let mut cursor = self.freelist.front_mut();
        while let Some(node) = cursor.get() {
            match node.weigh_against(gsize) {
                Some(Split) => return Some(unsafe { node.split_off(gsize) }),
                Some(Consume) => return cursor.remove().map(SizedFreeObj::take),
                None => cursor.move_next()
            }
        }
        None
    }

    fn sequential_allocate(&mut self, galign: NonZero<GSize>, gsize: NonZero<GSize>)
        -> Option<Initializable<usize>>
    {
        use self::AllocSat::*;

        let gsize = usize::from(gsize.get());
        self.bumper.as_ref()
            .and_then(|bumper| bumper.weigh_against(gsize))
            .and_then(|sat| match sat {
                Split => self.bumper.as_mut().map(|b| unsafe { b.split_off(gsize) }),
                Consume => self.bumper.take().map(Span::take)
            })
    }

    fn allocate_large(&mut self, galign: NonZero<GSize>, gsize: NonZero<GSize>)
        -> Option<Initializable<usize>>
    {
        unsafe {
            let bsize = NonZero::new_unchecked(usize::from(gsize.get()).ceil_div(Block::WSIZE));
            self.block_allocator.alloc_large_obj_rope(bsize)
                .map(|rope| {
                    self.large_objs.push_back(UnsafeRef::from_raw(rope.as_ptr()));
                    Unique::new_unchecked((*rope.as_ptr()).upcast().start() as _)
                })
        }
    }

    fn refill_bumper(&mut self) -> bool {
        if let Some(bumper) = self.bumper.take() {
            let len = bumper.len();
            if len > 0 {
                self.release(Span::take(bumper),
                             unsafe { NonZero::new_unchecked(transmute(len)) });
            }
        }
        if let Some((block, bumper)) = self.block_allocator.alloc_ms_block() {
            self.bumper = Some(bumper);
            self.active_blocks.push_back(unsafe { UnsafeRef::from_raw(block.as_ptr()) });
            true
        } else {
            false
        }
    }
}

// ================================================================================================

#[derive(Clone, Copy)]
enum SweepState<Obj> {
    Det,
    Free(*const Obj, NonZero<GSize>),
    Obj
}

// ================================================================================================

#[derive(Debug)]
struct SizedFreeObj {
    link: LinkedListLink,
    len: Cell<usize>
}

intrusive_adapter!(SizedFreeAdapter = UnsafeRef<SizedFreeObj>:
                   SizedFreeObj { link: LinkedListLink });

impl SizedFreeObj {
    unsafe fn init(uptr: Initializable<Self>, len: NonZero<usize>) -> Unique<Self> {
        let ptr = uptr.as_ptr() as *mut SizedFreeObj;
        ptr::write(ptr, SizedFreeObj {
            link: LinkedListLink::default(),
            len: Cell::new(len.get())
        });
        Unique::new_unchecked(ptr)
    }

    fn len(&self) -> usize { self.len.get() }

    fn set_len(&self, new_len: usize) { self.len.set(new_len) }
}

impl Foam for SizedFreeObj {
    type Bubble = Uninitialized<usize>;
    type Owner = UnsafeRef<SizedFreeObj>;

    fn weigh_against(&self, request: usize) -> Option<AllocSat> {
        use std::cmp::Ordering::*;
        use self::AllocSat::*;

        match self.len().cmp(&request) {
            Greater if self.len() - request >= usize::from(GSize::of::<Self>()) => Some(Split),
            Greater | Equal => Some(Consume),
            Less => None
        }
    }

    unsafe fn split_off(&self, request: usize) -> Initializable<usize> {
        debug_assert!(self.len() - request >= usize::from(GSize::of::<Self>()));

        let rem = self.len() - request;
        let ptr = transmute::<_, *mut usize>(&*self).offset(rem as isize);
        self.set_len(rem);
        Unique::new_unchecked(ptr as _)
    }

    fn take(obj: UnsafeRef<SizedFreeObj>) -> Initializable<usize> {
        unsafe { Unique::new_unchecked(obj.into_raw() as _) }
    }
}
