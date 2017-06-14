use std::ptr::Shared;
use std::slice;
use std::mem::transmute;

use block;

pub const LARGE_OBJ_THRESHOLD: usize = block::WSIZE * 8 / 10;

/// Object reference (tagged pointer)
#[derive(Clone, Copy)]
pub struct ValueRef(usize);

impl ValueRef {
    pub const SHIFT: usize  = 3;
    const TAG_MASK: usize   = 0b111;
    const PTR_BIT: usize    = 0b001;
    const POINTY_BIT: usize = 0b010;

    /// Get the pointer if the reference contains one.
    pub fn ptr(self) -> Option<Shared<Object>> {
        if self.0 & Self::PTR_BIT == 1 {
            Some(unsafe { Shared::new((self.0 & !Self::TAG_MASK) as *const Object) })
        } else {
            None
        }
    }

    pub fn is_pointy(self) -> bool { self.0 & Self::POINTY_BIT != 0 }
}

impl From<Shared<Object>> for ValueRef {
    fn from(ptr: Shared<Object>) -> ValueRef {
        ValueRef(unsafe { ptr.as_mut_ptr() } as usize | Self::PTR_BIT)
    }
}

impl From<isize> for ValueRef {
    fn from(n: isize) -> ValueRef {
        ValueRef((n as usize) << Self::SHIFT)
    }
}

/// Object header
#[derive(Clone, Copy)]
pub struct Header(usize);

impl Header {
    const LEN_SHIFT: usize = 8;

    pub fn new(len: usize) -> Self { Header(len << Self::LEN_SHIFT) }

    /// Length of the object (in words if `self.pointy()`, in bytes otherwise).
    fn obj_len(self) -> usize { self.0 >> Self::LEN_SHIFT }
}

/// Heap-allocated value
#[repr(C)]
pub struct Object {
    header: Header
}

impl Object {
    const DATA_OFFSET: isize = 1;

    /// Has the object been marked during the current collection?
    pub fn is_marked(&self) -> bool { unimplemented!() }

    /// Set the mark (so that `self.is_marked() == true`).
    pub fn mark(&self) { unimplemented!() }

    /// Length of the object (in words if `self.pointy()`, in bytes otherwise).
    pub fn len(&self) -> usize { self.header.obj_len() }
}

#[repr(C)]
pub struct PointyObject {
    base: Object
}

impl PointyObject {
    /// The fields of the object that may contain pointers.
    pub fn fields(&self) -> &[ValueRef] {
        unsafe {
            let ptr = transmute::<_, *const ValueRef>(self).offset(Object::DATA_OFFSET);
            slice::from_raw_parts(ptr, self.base.len())
        }
    }
}
