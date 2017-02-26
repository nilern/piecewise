use std::slice;
use std::mem;
use std::mem::transmute;
use std::mem::size_of;

/// Object references should implement this.
pub trait Reference: Copy + Default {
    type Header: Header;

    /// Create a new reference from an object pointer,
    fn from_mut_ptr<O>(ptr: *mut O) -> Self where O: Object<Slot=Self>;

    /// Try to convert the reference to a raw pointer. Should produce `None` when self is immediate
    /// or null.
    fn ptr(self) -> Option<*const Self::Header>;

    /// Try to convert the reference to a mutable raw pointer. Should produce `None` when self is
    /// immediate or null.
    fn ptr_mut(self) -> Option<*mut Self::Header>;
}

/// Object headers should implement this.
pub trait Header {
    /// Is the object marked?
    fn is_marked(&self) -> bool;

    /// Is the object a blob (consists of raw bytes instead of slots)?
    fn is_blob(&self) -> bool;

    /// Get the raw tag of the object. Currently the tag is in the range [0, 16).
    fn tag(&self) -> usize;

    /// Get the size of the object (in slots).
    fn len(&self) -> usize;

    /// Get the forwarding pointer.
    fn get_forward<O>(&self) -> *mut O where O: Object;

    /// Set the mark to true.
    fn set_mark(&mut self);

    /// Set the mark to false.
    fn remove_mark(&mut self);

    /// Turn into a forwarding pointer.
    fn set_forward_to<O>(&mut self, dest: *mut O) where O: Object;
}

/// Any data that should be allocated and managed by GC should implement this.
pub trait Object: Sized {
    /// The header representation.
    type Header: Header;

    /// The slot type that makes up the actual data.
    type Slot: Reference;

    /// Get the header of the object.
    fn header(&self) -> &Self::Header;

    /// Get the header of the object.
    fn header_mut(&mut self) -> &mut Self::Header;

    /// Set the header of the object.
    fn set_header(&mut self, header: Self::Header);

    /// Get a slice to the data of the object.
    fn data(&self) -> &[Self::Slot];
}

/// A memory manager.
pub trait Collector {
    type O: Object;

    /// Can we allocate `slot_count` worth of pointy objects (including headers)?
    fn pointy_poll(&self, slot_count: usize) -> bool;

    /// Can we allocate `byte_count` worth of flat objects (including headers)?
    fn flat_poll(&self, byte_count: usize) -> bool;

    /// # Safety
    /// 1. You should pointy_poll() first to make sure `slot_count` slots can be allocated.
    /// 2. `slot_count == header.len() + size_of::<O::Header>() / size_of::<O::Slot>()`
    unsafe fn alloc_pointy(&mut self, header: <Self::O as Object>::Header, slot_count: usize)
        -> <Self::O as Object>::Slot;

    /// # Safety
    /// 1. You should flat_poll() first to make sure `byte_count` bytes can be allocated.
    /// 2. `byte_count == header.len() + size_of::<O::Header>()`
    unsafe fn alloc_flat(&mut self, header: <Self::O as Object>::Header, byte_count: usize)
        -> <Self::O as Object>::Slot;

    /// Mark/move a reference. Return the new value of the reference.
    ///
    /// # Safety
    /// `oref` must actually point into the GC heap.
    unsafe fn mark(&mut self, oref: <Self::O as Object>::Slot) -> <Self::O as Object>::Slot;

    /// Collect garbage. Assumes that all roots have already been `mark()`:ed.
    ///
    /// # Safety
    /// You must mark all roots and let go of all raw O::Slot:s before calling
    /// this. Otherwise you will get segfaults or at the very least pointers to
    /// garbage data.
    unsafe fn collect(&mut self);
}

pub struct SimpleCollector<O> where O: Object {
    fromspace: Vec<O::Slot>,
    tospace: Vec<O::Slot>,
    space_interval: usize,

    blobspace: Vec<*mut O>,
    blob_bytes_allocated: usize,
    byte_interval: usize
}

impl<O> SimpleCollector<O> where O: Object {
    /// Create a new `Collector`.
    pub fn new(space_interval: usize, byte_interval: usize) -> SimpleCollector<O> {
        SimpleCollector {
            fromspace: Vec::with_capacity(space_interval),
            tospace: Vec::with_capacity(space_interval),
            space_interval: space_interval,

            blobspace: Vec::new(),
            blob_bytes_allocated: 0,
            byte_interval: byte_interval
        }
    }

    unsafe fn salvage(&mut self, obj: *mut O::Header) -> *mut O {
        let free = self.tospace.as_mut_ptr().offset(self.tospace.len() as isize) as *mut O;
        let oslice = slice::from_raw_parts(
            obj as *const O::Slot,
            (*obj).len() + size_of::<O::Header>() / size_of::<O::Slot>());
        self.tospace.extend_from_slice(oslice);
        free
    }

    fn sweep(&mut self) {
        self.blobspace.retain(|&ptr| unsafe {
            let header = (*ptr).header_mut();
            if header.is_marked() {
                header.remove_mark();
                true
            } else {
                let size = header.len() + size_of::<O::Header>();
                mem::drop(Vec::from_raw_parts(ptr, size, size)); // deallocate
                false
            }
        });
    }

    fn copy_compact(&mut self) {
        let mut scan = 0;
        while scan < self.tospace.len() {
            let len = unsafe {
                transmute::<&mut O::Slot, &mut O>(&mut self.tospace[scan]).header().len()
            };
            scan += 1;

            for _ in 0..len {
                let slot = self.tospace[scan];
                self.tospace[scan] = unsafe { self.mark(slot) };
                scan += 1;
            }
        }
    }
}

impl<O> Collector for SimpleCollector<O> where O: Object {
    type O = O;

    fn pointy_poll(&self, slot_count: usize) -> bool {
        self.fromspace.capacity() - self.fromspace.len() >= slot_count * size_of::<O::Slot>()
    }

    fn flat_poll(&self, byte_count: usize) -> bool {
        self.byte_interval - self.blob_bytes_allocated >= byte_count
    }

    unsafe fn alloc_pointy(&mut self, header: O::Header, slot_count: usize) -> O::Slot {
        let oldlen = self.fromspace.len();
        let ptr = self.fromspace.as_mut_ptr().offset(oldlen as isize) as *mut O;
        self.fromspace.resize(oldlen + slot_count, Default::default());
        (*ptr).set_header(header);

        O::Slot::from_mut_ptr(ptr)
    }

    unsafe fn alloc_flat(&mut self, header: O::Header, byte_count: usize) -> O::Slot {
        let mut bytes = vec![0; byte_count];
        let ptr = bytes.as_mut_ptr() as *mut O;
        mem::forget(bytes);
        (*ptr).set_header(header);

        self.blobspace.push(ptr);
        self.blob_bytes_allocated += byte_count;

        O::Slot::from_mut_ptr(ptr)
    }

    unsafe fn mark(&mut self, oref: O::Slot) -> O::Slot {
        if let Some(ptr) = oref.ptr_mut() {
            if (*ptr).is_marked() {
                if (*ptr).is_blob() {
                    oref
                } else {
                    O::Slot::from_mut_ptr((*ptr).get_forward::<O>())
                }
            } else {
                if (*ptr).is_blob() {
                    (*ptr).set_mark();
                    oref
                } else {
                    let newptr = self.salvage(ptr as *mut O::Header);
                    (*ptr).set_forward_to(newptr);
                    O::Slot::from_mut_ptr(newptr)
                }
            }
        } else {
            oref
        }
    }

    unsafe fn collect(&mut self) {
        // Ensure tospace is at least as big as fromspace:
        let fcap = self.fromspace.capacity();
        if self.tospace.capacity() < fcap {
            self.tospace.reserve_exact(fcap);
        }

        self.copy_compact(); // copy and compact pointies
        self.sweep(); // deallocate blobs

        mem::swap(&mut self.fromspace, &mut self.tospace); // fromspace <-> tospace
        self.tospace.clear(); // don't need the tospace contents anymore so empty it

        // Grow tospace if fromspace is at >80% despite having just collected:
        if self.fromspace.len() as f64 > 0.8*self.fromspace.capacity() as f64 {
            let tcap = self.tospace.capacity();
            self.tospace.reserve(tcap + self.space_interval);
        }
        self.blob_bytes_allocated = 0; // Haven't allocated any blobs after this collection:
    }
}
