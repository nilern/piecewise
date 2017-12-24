use std::ptr::{Unique, Shared};
use std::mem::transmute;
use std::ops::{Deref, DerefMut};
use std::fmt::{self, Formatter};
use std::marker::PhantomData;
use std::iter;

use gce::{ObjectRef, PointyObjectRef};
use object::{DynamicDebug, HeapValue, ValueView, TypeIndex, TypeRegistry, ObjRefs};

// ================================================================================================

const TAG_MASK: usize = 0b111;
const PTR_BIT: usize = 0b001;

// ================================================================================================

/// A value reference (tagged pointer).
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct ValueRef(usize);

impl ValueRef {
    /// Does `self` hold a pointer?
    fn is_ptr(self) -> bool { self.0 & PTR_BIT == 1 }

    /// Get the corresponding `ValueView`.
    fn view<T: TypeRegistry>(self, type_reg: &T) -> ValueView {
        if let Some(sptr) = self.ptr() {
            match type_reg.index_of(*unsafe { sptr.as_ref() }.typ()) {
                TypeIndex::Type =>
                    ValueView::Type(TypedValueRef::new(unsafe { transmute(sptr) })),
                TypeIndex::Const =>
                    ValueView::Const(TypedValueRef::new(unsafe { transmute(sptr) })),
                TypeIndex::Symbol =>
                    ValueView::Symbol(TypedValueRef::new(unsafe { transmute(sptr) })),
                TypeIndex::Call =>
                    ValueView::Call(TypedValueRef::new(unsafe { transmute(sptr) }))
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

impl DynamicDebug for ValueRef {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        self.view(type_reg).fmt(f, type_reg)
    }
}

// ================================================================================================

/// A statically typed `ValueRef`.
pub struct TypedValueRef<T>(usize, PhantomData<T>);

impl<T> Clone for TypedValueRef<T> {
    fn clone(&self) -> Self { TypedValueRef(self.0, self.1) }
}

impl<T> Copy for TypedValueRef<T> {}

impl<T> TypedValueRef<T> {
    /// Convert from a (freshly allocated) pointer.
    pub fn new(ptr: Unique<T>) -> TypedValueRef<T> {
        TypedValueRef(ptr.as_ptr() as usize | PTR_BIT, PhantomData::default())
    }

    /// Forget the static type information.
    pub fn upcast(self) -> ValueRef { ValueRef(self.0) }
}

impl<T> Deref for TypedValueRef<T> {
    type Target = T;

    fn deref(&self) -> &T { unsafe { transmute(self.0 & !TAG_MASK) } }
}

impl<T> DerefMut for TypedValueRef<T> {
    fn deref_mut(&mut self) -> &mut T { unsafe { transmute(self.0 & !TAG_MASK) } }
}

impl<T: DynamicDebug> DynamicDebug for TypedValueRef<T> {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, type_reg: &R) -> Result<(), fmt::Error> {
        self.deref().fmt(f, type_reg)
    }
}

// ================================================================================================

/// A `ValueRef` that has a non-zero number of potentially pointer-valued fields.
#[derive(Clone, Copy)]
pub struct PointyValueRef(ValueRef);

impl PointyObjectRef for PointyValueRef {
    type ORef = ValueRef;
    type RefIter = iter::Chain<iter::Once<*mut ValueRef>, ObjRefs>;

    fn obj_refs(&self) -> Self::RefIter {
        let base: &HeapValue = unsafe { transmute((self.0).0 & !TAG_MASK) };
        iter::once(unsafe { transmute(base.typ()) })
             .chain(base.ref_fields())
    }
}
