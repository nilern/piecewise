use std::slice;
use std::mem;
use std::mem::transmute;
use std::mem::size_of;

// FIXME: Make it possible to allocate pointy objects while trying to reach safepoint

/// Object reference, usually a (possibly tagged) pointer.
pub unsafe trait Reference: Copy + Default {
    /// The object header type `Self` possibly refers to.
    type Header;

    /// Create a new reference from an object pointer.
    ///
    /// # Safety
    /// `ptr` should point to managed memory.
    unsafe fn from_mut_ptr(ptr: *mut Self::Header) -> Self;

    /// Try to convert the reference to a raw pointer. Should produce `None` when self is immediate
    /// or null.
    fn ptr(self) -> Option<*const Self::Header>;

    /// Try to convert the reference to a mutable raw pointer. Should produce `None` when self is
    /// immediate or null.
    fn ptr_mut(self) -> Option<*mut Self::Header>;
}

/// Object header
pub unsafe trait Header: Copy {
    /// Is the object marked?
    fn is_marked(&self) -> bool;

    /// Is the object a blob (consists of raw bytes instead of slots)?
    fn is_blob(&self) -> bool;

    /// Get the raw tag of the object. Currently the tag is in the range [0, 16).
    fn tag(&self) -> usize;

    /// Get the size of the object (in bytes if `self.is_blob()`, otherwise in slots).
    fn len(&self) -> usize;

    /// Get the forwarding pointer.
    fn get_forward(&self) -> Option<*mut Self>;

    /// Set the mark to true. Doing this at the wrong time will result in memory leaks but those
    /// are not considered unsafe so neither is this method.
    fn set_mark(&mut self);

    /// Set the mark to false.
    ///
    /// # Safety
    /// Removing the mark from the header of a live object at the wrong time will result in
    /// dangling pointers.
    unsafe fn remove_mark(&mut self);

    /// Turn into a forwarding pointer.
    ///
    /// # Safety
    /// `dest` should point to managed memory. Doing this at the wrong time will probably confuse
    /// the mutator and thus result in undefined behaviour.
    unsafe fn set_forward_to(&mut self, dest: *mut Self);
}

/// An `Object` can be allocated under `Self::Header`.
pub trait Object {
    type Header: Header;
}

/// An `Object` that has a static size and contains (only) pointers.
pub unsafe trait SizedPointyObject: Object + Sized {
    /// Create the appropriate `Self::Header`.
    fn header() -> Self::Header;
}

/// An `Object` that has a static size and doesn't contain any pointers.
pub unsafe trait SizedFlatObject: Object + Sized {
    /// Create the appropriate `Self::Header`.
    fn header() -> Self::Header;
}

/// An `Object` that has a dynamically determined size and contains (only) pointers.
pub unsafe trait UnSizedPointyObject: Object {
    /// Create the appropriate `Self::Header`. `len` is the amount of slots contained.
    fn header(len: usize) -> Self::Header;
}

/// An `Object` that has a dynamically determined size and doesn't contain any pointers.
pub unsafe trait UnSizedFlatObject: Object {
    /// Create the appropriate `Self::Header`. `len` is the size of the data in bytes.
    fn header(len: usize) -> Self::Header;
}

/// Allocation of header-tagged memory.
pub unsafe trait Allocator {
    /// The type to use for object headers.
    type Header: Header;

    /// The type to use for object slots.
    type Slot: Reference<Header=Self::Header>;

    /// Can we allocate `slot_count` worth of pointy objects (including headers)?
    fn pointy_poll(&self, slot_count: usize) -> bool;

    /// Can we allocate `byte_count` worth of flat objects (including headers)?
    fn flat_poll(&self, byte_count: usize) -> bool;

    /// # Safety
    /// `slot_count == header.len() + size_of::<O::Header>() / size_of::<O::Slot>()`
    unsafe fn alloc_pointy(&mut self, header: Self::Header, slot_count: usize) -> Self::Slot;

    /// # Safety
    /// `byte_count == header.len() + size_of::<O::Header>()`
    unsafe fn alloc_flat(&mut self, header: Self::Header, byte_count: usize) -> Self::Slot;

    fn alloc_sized_pointy<T>(&mut self, v: T) -> Self::Slot
        where T: SizedPointyObject<Header=Self::Header>
    {
        unsafe {
            let header = T::header();
            let oref = self.alloc_pointy(header,
                header.len() + size_of::<Self::Header>() / size_of::<Self::Slot>());
            let data_ptr = oref.ptr_mut().unwrap().offset(1) as *mut T;
            *data_ptr = v;
            oref
        }
    }

    fn alloc_sized_flat<T>(&mut self, v: T) -> Self::Slot
        where T: SizedFlatObject<Header=Self::Header>
    {
        unsafe {
            let header = T::header();
            let oref = self.alloc_flat(header, header.len() + size_of::<Self::Header>());
            let data_ptr = oref.ptr_mut().unwrap().offset(1) as *mut T;
            *data_ptr = v;
            oref
        }
    }
}

/// Garbage collection.
pub unsafe trait Collector {
    type Slot: Reference;

    /// Mark/move a reference. Return the new value of the reference.
    fn mark(&mut self, oref: Self::Slot) -> Self::Slot;

    /// Collect garbage. Assumes that all roots have already been `mark()`:ed.
    ///
    /// # Safety
    /// You must mark all roots and let go of all raw O::Slot:s before calling
    /// this. Otherwise you will get segfaults or at the very least pointers to
    /// garbage data.
    unsafe fn collect(&mut self);
}

// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct SimpleCollector<H, R> where H: Header, R: Reference<Header=H> {
    fromspace: Vec<R>,
    tospace: Vec<R>,
    space_interval: usize,

    blobspace: Vec<*mut H>,
    blob_bytes_allocated: usize,
    byte_interval: usize
}

impl<H, R> SimpleCollector<H, R> where H: Header, R: Reference<Header=H> {
    /// Create a new `SimpleCollector`.
    pub fn new(space_interval: usize, byte_interval: usize) -> SimpleCollector<H, R> {
        SimpleCollector {
            fromspace: Vec::with_capacity(space_interval),
            tospace: Vec::with_capacity(space_interval),
            space_interval: space_interval,

            blobspace: Vec::new(),
            blob_bytes_allocated: 0,
            byte_interval: byte_interval
        }
    }

    unsafe fn salvage(&mut self, obj: *mut H) -> *mut H {
        let free = self.tospace.as_mut_ptr().offset(self.tospace.len() as isize) as *mut H;
        let oslice = slice::from_raw_parts(
            obj as *const R,
            (*obj).len() + size_of::<H>() / size_of::<R>());
        self.tospace.extend_from_slice(oslice);
        free
    }

    fn sweep(&mut self) {
        self.blobspace.retain(|&header| unsafe {
            if (*header).is_marked() {
                (*header).remove_mark();
                true
            } else {
                let size = (*header).len() + size_of::<H>();
                mem::drop(Vec::from_raw_parts(header, size, size)); // deallocate
                false
            }
        });
    }

    fn copy_compact(&mut self) {
        let mut scan = 0;
        while scan < self.tospace.len() {
            let len = unsafe {
                transmute::<&mut R, &mut H>(&mut self.tospace[scan]).len()
            };
            scan += 1;

            for _ in 0..len {
                let slot = self.tospace[scan];
                self.tospace[scan] = self.mark(slot);
                scan += 1;
            }
        }
    }
}

unsafe impl<H, R> Allocator for SimpleCollector<H, R> where H: Header, R: Reference<Header=H> {
    type Header = H;
    type Slot = R;

    fn pointy_poll(&self, slot_count: usize) -> bool {
        self.fromspace.capacity() - self.fromspace.len() >= slot_count * size_of::<R>()
    }

    fn flat_poll(&self, byte_count: usize) -> bool {
        self.byte_interval - self.blob_bytes_allocated >= byte_count
    }

    unsafe fn alloc_pointy(&mut self, header: Self::Header, slot_count: usize) -> R {
        let oldlen = self.fromspace.len();
        let ptr = self.fromspace.as_mut_ptr().offset(oldlen as isize) as *mut H;
        self.fromspace.resize(oldlen + slot_count, Default::default());
        *ptr = header;

        Self::Slot::from_mut_ptr(ptr)
    }

    unsafe fn alloc_flat(&mut self, header: Self::Header, byte_count: usize) -> R {
        let mut bytes = vec![0; byte_count];
        let ptr = bytes.as_mut_ptr() as *mut H;
        mem::forget(bytes);
        *ptr = header;

        self.blobspace.push(ptr);
        self.blob_bytes_allocated += byte_count;

        Self::Slot::from_mut_ptr(ptr)
    }
}

unsafe impl<H, R> Collector for SimpleCollector<H, R> where H: Header, R: Reference<Header=H> {
    type Slot = R;

    fn mark(&mut self, oref: Self::Slot) -> Self::Slot {
        if let Some(ptr) = oref.ptr_mut() {
            unsafe {
                if let Some(fwd) = (*ptr).get_forward() {
                    Self::Slot::from_mut_ptr(fwd)
                } else if !(*ptr).is_blob() {
                    let newptr = self.salvage(ptr as *mut H);
                    (*ptr).set_forward_to(newptr);
                    Self::Slot::from_mut_ptr(newptr)
                } else {
                    if !(*ptr).is_marked() {
                        (*ptr).set_mark();
                    }
                    oref
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
