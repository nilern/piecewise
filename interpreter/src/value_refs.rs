use std::ptr::{Unique, Shared};
use std::mem::transmute;
use std::ops::{Deref, DerefMut};
use std::fmt::{self, Formatter};
use std::marker::PhantomData;
use std::iter;
use std::hash::{Hash, Hasher};

use gce::{ObjectRef, PointyObjectRef};
use object::{DynamicDebug, HeapValue, ValueView, TypeIndex, TypeRegistry, ObjRefs};

// ================================================================================================

const SHIFT: usize = 3;
const TAG_MASK: usize = (1 << SHIFT) - 1;
const PTR_BIT: usize = 0b001;

// ================================================================================================

/// A value reference (tagged pointer).
#[derive(Clone, Copy)]
pub struct ValueRef(usize);

impl ValueRef {
    /// Does `self` hold a pointer?
    fn is_ptr(self) -> bool { self.0 & PTR_BIT == 1 }

    /// Record the fact that `self` points to a `T`.
    unsafe fn downcast<T>(self) -> TypedValueRef<T> {
        TypedValueRef(self.0, PhantomData::default())
    }

    /// Get the corresponding `ValueView`.
    fn view<T: TypeRegistry>(self, type_reg: &T) -> ValueView {
        if let Some(sptr) = self.ptr() {
            match type_reg.index_of(*unsafe { sptr.as_ref() }.typ()) {
                TypeIndex::Type     => ValueView::Type(unsafe { self.downcast() }),
                TypeIndex::Symbol   => ValueView::Symbol(unsafe { self.downcast() }),

                TypeIndex::Function => ValueView::Function(unsafe { self.downcast() }),
                TypeIndex::Method   => ValueView::Method(unsafe { self.downcast() }),
                TypeIndex::Block    => ValueView::Block(unsafe { self.downcast() }),
                TypeIndex::Call     => ValueView::Call(unsafe { self.downcast() }),
                TypeIndex::Const    => ValueView::Const(unsafe { self.downcast() }),
                TypeIndex::Lex      => ValueView::Lex(unsafe { self.downcast() }),
            }
        } else {
            match self.0 & TAG_MASK {
                0b000 => ValueView::Int((self.0 >> SHIFT) as isize),
                0b010 => ValueView::Float((self.0 & !TAG_MASK) as f64),
                0b100 => ValueView::Char(unsafe { transmute((self.0 >> SHIFT) as u32) }),
                0b110 => ValueView::Bool(unsafe { transmute((self.0 >> SHIFT) as u8) }),
                _ => unreachable!()
            }
        }
    }
}

impl From<isize> for ValueRef {
    fn from(n: isize) -> ValueRef { ValueRef((n as usize) << SHIFT) }
}

impl From<f64> for ValueRef {
    fn from(n: f64) -> ValueRef { ValueRef((n as usize & !TAG_MASK) | 0b010) }
}

impl From<char> for ValueRef {
    fn from(c: char) -> ValueRef { ValueRef((c as usize) << SHIFT | 0b100) }
}

impl From<bool> for ValueRef {
    fn from(b: bool) -> ValueRef { ValueRef((b as usize) << SHIFT | 0b110) }
}

impl<T> From<TypedValueRef<T>> for ValueRef {
    fn from(tvref: TypedValueRef<T>) -> ValueRef { ValueRef(tvref.0) }
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

impl<T> PartialEq for TypedValueRef<T> {
    fn eq(&self, other: &TypedValueRef<T>) -> bool { self.0 == other.0 }
}

impl<T> Eq for TypedValueRef<T> {}

impl<T> Hash for TypedValueRef<T> {
    fn hash<H>(&self, state: &mut H) where H: Hasher { self.0.hash(state) }
}

impl<T> TypedValueRef<T> {
    /// Convert from a (freshly allocated) pointer.
    pub fn new(ptr: Unique<T>) -> TypedValueRef<T> {
        TypedValueRef(ptr.as_ptr() as usize | PTR_BIT, PhantomData::default())
    }
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
