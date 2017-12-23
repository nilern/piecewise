use std::ptr::Shared;
use std::slice;
use std::mem::transmute;
use std::ops::{Deref, DerefMut};
use std::fmt::{self, Debug, Formatter};
use std::marker::PhantomData;

use gce::{ObjectRef, PointyObjectRef};
use object::{HeapValue, ValueView, Type, TypeIndex, TypeRegistry};

// ================================================================================================

const TAG_MASK: usize = 0b111;
const PTR_BIT: usize = 0b001;

// ================================================================================================

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ValueRef(usize);

impl ValueRef {
    fn is_ptr(self) -> bool { self.0 & PTR_BIT == 1 }

    fn typ(self) -> TypedValueRef<Type> {
        unimplemented!()
    }

    fn view<T: TypeRegistry>(self, type_reg: &T) -> ValueView {
        if self.is_ptr() {
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
