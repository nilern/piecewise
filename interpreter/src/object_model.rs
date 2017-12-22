use std::ptr::Shared;
use std::slice;
use std::mem::{size_of, transmute};
use std::ops::Deref;
use std::fmt::{self, Debug, Formatter};

use gce::util::CeilDiv;
use gce::{Object, ObjectRef, PointyObjectRef};
use gce::layout::{Granule, GSize};

// ================================================================================================

const TAG_MASK: usize = 0b111;
const PTR_BIT: usize = 0b001;

// ================================================================================================

#[derive(Debug)]
pub enum ValueView {
    Type(GcPtr<Type>),

    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
}

// ================================================================================================

#[derive(Debug)]
#[repr(C)]
pub struct HeapValue {
    link: ValueRef,
    typ: GcPtr<Type>
}

impl HeapValue {
    fn ref_len(&self) -> usize { self.typ.ref_len }

    fn view(&self) -> ValueView { unimplemented!() }
}

impl Object for HeapValue {
    fn gsize(&self) -> GSize { From::from(self.typ.size.ceil_div(size_of::<Granule>())) }
}

// ================================================================================================

#[derive(Debug)]
#[repr(C)]
pub struct Type {
    heap_value: HeapValue,
    size: usize,
    ref_len: usize
}

// ================================================================================================

#[derive(Clone, Copy, Debug)]
pub struct GcPtr<T>(*mut T);

impl<T> Deref for GcPtr<T> {
    type Target = T;

    fn deref(&self) -> &T { unsafe{ &*self.0 } }
}

// ================================================================================================

#[derive(Clone, Copy)]
pub struct ValueRef(usize);

impl ValueRef {
    fn is_ptr(self) -> bool { self.0 & PTR_BIT == 1 }

    fn view(self) -> ValueView {
        if let Some(sptr) = self.ptr() {
            unsafe { &*sptr.as_ptr() }.view()
        } else {
            match self.0 & TAG_MASK {
                0b000 => ValueView::Int((self.0 & !TAG_MASK) as isize),
                0b010 => ValueView::Float((self.0 & !TAG_MASK) as f64),
                0b100 => ValueView::Char(unsafe { transmute((self.0 & !TAG_MASK) as u32) }),
                0b110 => ValueView::Bool((self.0 & !TAG_MASK) == 1),
                _ => unreachable!()
            }
        }
    }
}

impl ObjectRef for ValueRef {
    type Obj = HeapValue;
    type PORef = PointyValueRef;

    fn ptr(self) -> Option<Shared<HeapValue>> {
        if self.is_ptr() {
            Shared::new((self.0 & !TAG_MASK) as _)
        } else {
            None
        }
    }

    fn pointy_ref(self) -> Option<PointyValueRef> {
        if (self.0 & TAG_MASK) == 0b011 {
            Some(PointyValueRef(self))
        } else {
            None
        }
    }
}

impl Debug for ValueRef {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> { self.view().fmt(f) }
}

// ================================================================================================

#[derive(Clone, Copy)]
pub struct PointyValueRef(ValueRef);

impl PointyObjectRef for PointyValueRef {
    type ORef = ValueRef;

    fn obj_refs<'a>(self) -> &'a mut[Self::ORef] {
        let ptr: *mut HeapValue = ((self.0).0 & !TAG_MASK) as _;
        unsafe {
            let data_ptr: *mut ValueRef = ptr.offset(1) as _;
            slice::from_raw_parts_mut(data_ptr, (*ptr).ref_len())
        }
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use gce::layout::GSize;
    use super::{HeapValue, Type};

    #[test]
    fn heap_value_size() {
        assert_eq!(GSize::of::<HeapValue>(), GSize::from(2));
    }

    #[test]
    fn type_size() {
        assert_eq!(GSize::of::<Type>(), GSize::from(4));
    }
}
