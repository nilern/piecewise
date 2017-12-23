use std::ptr::Shared;
use std::slice;
use std::mem::{size_of, transmute};
use std::ops::{Deref, DerefMut};
use std::fmt::{self, Debug, Formatter};
use std::marker::PhantomData;

use gce::util::CeilDiv;
use gce::{Object, ObjectRef, PointyObjectRef};
use gce::layout::{Granule, GSize};

// ================================================================================================

const TAG_MASK: usize = 0b111;
const PTR_BIT: usize = 0b001;

// ================================================================================================

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum TypeIndex {
    Type
}

trait TypeRegistry {
    fn index_of(&self, typ: TypedValueRef<Type>) -> TypeIndex;
}

// ================================================================================================

#[derive(Debug)]
pub enum ValueView {
    Type(TypedValueRef<Type>),

    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
}

// ================================================================================================

#[repr(C)]
pub struct HeapValue {
    link: ValueRef,
    typ: TypedValueRef<Type>
}

impl HeapValue {
    fn ref_len(&self) -> usize { self.typ.ref_len }
}

impl Object for HeapValue {
    fn gsize(&self) -> GSize {
        From::from(self.typ.size.ceil_div(size_of::<Granule>()))
    }
}

impl Debug for HeapValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        // TODO: fields (may point back to self!)
        f.debug_struct("HeapValue").finish()
    }
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

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ValueRef(usize);

impl ValueRef {
    fn is_ptr(self) -> bool { self.0 & PTR_BIT == 1 }

    fn typ(self) -> TypedValueRef<Type> {
        unimplemented!()
    }

    fn view<T: TypeRegistry>(self, type_reg: &T) -> ValueView {
        if let Some(sptr) = self.ptr() {
            match type_reg.index_of(self.typ()) {
                TypeIndex::Type => ValueView::Type(unsafe { TypedValueRef::new(self) })
            }
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

    fn fmt<T>(vref: ValueRef, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error>
        where T: TypeRegistry
    {
        vref.view(type_reg).fmt(f)
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

// ================================================================================================

pub struct TypedValueRef<T>(usize, PhantomData<T>);

impl<T> TypedValueRef<T> {
    unsafe fn new(vref: ValueRef) -> Self {
        TypedValueRef(vref.0, PhantomData::default())
    }
}

impl<T> Deref for TypedValueRef<T> {
    type Target = T;

    fn deref(&self) -> &T { unsafe { transmute(self.0 & !TAG_MASK) } }
}

impl<T> DerefMut for TypedValueRef<T> {
    fn deref_mut(&mut self) -> &mut T { unsafe { transmute(self.0 & !TAG_MASK) } }
}

impl<T: Debug> Debug for TypedValueRef<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.deref().fmt(f)
    }
}

// ================================================================================================

#[derive(Clone, Copy)]
pub struct PointyValueRef(ValueRef);

impl PointyObjectRef for PointyValueRef {
    type ORef = ValueRef;

    fn obj_refs<'a>(self) -> &'a mut[Self::ORef] {
        let ptr: *const HeapValue = ((self.0).0 & !TAG_MASK) as _;
        unsafe {
            let data_ptr = (ptr as *mut ValueRef).offset(1);
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
