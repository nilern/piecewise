use std::ptr::Shared;
use std::slice;
use std::mem::transmute;

/// Object reference (tagged pointer)
#[derive(Clone, Copy)]
pub struct GCRef(usize);

impl GCRef {
    const TAG_MASK: usize = 0b111;
    const PTR_BIT: usize = 0b1;

    /// Get the pointer if the reference contains one.
    pub fn ptr(self) -> Option<Shared<Object>> {
        if self.0 & Self::PTR_BIT == 1 {
            Some(unsafe { Shared::new((self.0 & !Self::TAG_MASK) as *const Object) })
        } else {
            None
        }
    }
}

impl From<Shared<Object>> for GCRef {
    fn from(ptr: Shared<Object>) -> GCRef {
        GCRef(unsafe { ptr.as_mut_ptr() } as usize | Self::PTR_BIT)
    }
}

/// Object header
#[derive(Clone, Copy)]
struct Header(usize);

impl Header {
    const LEN_SHIFT: usize = 8;
    const POINTY_BIT: usize = 0b10;

    /// Length of the object (in words if `self.pointy()`, in bytes otherwise).
    fn obj_len(self) -> usize { self.0 >> Self::LEN_SHIFT }

    /// Does the object contain pointers?
    fn pointy(self) -> bool { self.0 & Self::POINTY_BIT == 1 }
}

/// Heap-allocated value
#[repr(C)]
pub struct Object {
    header: Header
}

impl Object {
    const DATA_OFFSET: isize = 1;

    /// Does the object contain pointers?
    pub fn is_pointy(&self) -> bool { self.header.pointy() }

    /// Has the object been marked during the current collection?
    pub fn is_marked(&self) -> bool { unimplemented!() }

    /// Set the mark (so that `self.is_marked() == true`).
    pub fn mark(&self) { unimplemented!() }

    /// Length of the object (in words if `self.pointy()`, in bytes otherwise).
    pub fn len(&self) -> usize { self.header.obj_len() }

    /// The fields of the object that may contain pointers.
    pub fn fields(&self) -> &[GCRef] {
        if self.is_pointy() {
            unsafe {
                let ptr = transmute::<_, *const GCRef>(self).offset(Self::DATA_OFFSET);
                slice::from_raw_parts(ptr, self.len())
            }
        } else {
            Default::default() // empty slice
        }
    }
}
