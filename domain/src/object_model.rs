use std::mem::transmute;
use std::ptr::{Unique, Shared};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::hash::{Hash, Hasher};
use std::slice;
use std::fmt::{self, Debug, Formatter};

use pcws_gc::{GSize, Object, ObjectRef};

use super::{Allocator, Reifier, Viewer, DynamicDebug};
use values::Type;

// ================================================================================================

/// Unboxable scalar reference.
pub trait Unbox {
    type Target: Copy;

    fn unbox(self) -> Self::Target;
}

// ================================================================================================

/// A subtype of `HeapValue`.
pub trait HeapValueSub: Sized {
    /// The constant portion (or minimum number) of `ValueRef` fields on instances of `self`.
    const UNIFORM_REF_LEN: usize;
}

pub trait UniformHeapValue: HeapValueSub {
    fn new_typ<A: Allocator>(allocator: &mut A) -> Option<ValueRefT<Type>> {
        Type::new::<A, Self>(allocator, false, false)
    }
}

/// A subtype of `DynHeapValue`.
pub trait DynHeapValueSub: HeapValueSub {}

pub trait RefTailed: DynHeapValueSub {
    type TailItem;

    /// Get the tail slice.
    fn tail(&self) -> &[Self::TailItem] {
        unsafe {
            slice::from_raw_parts((self as *const Self).offset(1) as *const Self::TailItem,
                                  transmute::<_, &DynHeapValue>(self).dyn_len)
        }
    }

    fn new_typ<A: Allocator>(allocator: &mut A) -> Option<ValueRefT<Type>> {
        Type::new::<A, Self>(allocator, true, true)
    }
}

pub trait BlobTailed: DynHeapValueSub {
    type TailItem;

    /// Get the tail slice.
    fn tail(&self) -> &[Self::TailItem] {
        unsafe {
            slice::from_raw_parts((self as *const Self).offset(1) as *const Self::TailItem,
                                  transmute::<_, &DynHeapValue>(self).dyn_len)
        }
    }

    fn new_typ<A: Allocator>(allocator: &mut A) -> Option<ValueRefT<Type>> {
        Type::new::<A, Self>(allocator, true, false)
    }
}

// ================================================================================================

/// The 'supertype' of all heap values (non-scalars).
#[repr(C)]
pub struct HeapValue {
    /// Multipurpose redirection field
    pub link: ValueRef,
    /// Dynamic type
    pub typ: ValueRefT<Type>
}

impl HeapValue {
    /// See `ValueRef::force`.
    fn force(&self) -> Option<ValueRef> {
        let mut ptr = self as *const HeapValue;
        let mut vref = ValueRef::from(unsafe { Shared::new_unchecked(ptr as *mut _) });

        loop {
            let link = unsafe { (*ptr).link };
            if link == vref {
                return Some(vref);
            } else if link == ValueRef::NULL {
                return None;
            } else {
                vref = link;
                if let Some(sptr) = vref.ptr() {
                    ptr = sptr.as_ptr();
                } else {
                    return Some(vref);
                }
            }
        }
    }

    fn ref_len(&self) -> usize {
        if self.typ.has_dyn_ref_len() {
            self.typ.uniform_ref_len() + unsafe { transmute::<_, &DynHeapValue>(self) }.dyn_len
        } else {
            self.typ.uniform_ref_len()
        }
    }

    fn refs_ptr(&self) -> *mut ValueRef {
        if self.typ.has_dyn_ref_len() {
            (unsafe { transmute::<_, *const DynHeapValue>(self).offset(1) }) as _
        } else {
            (unsafe { transmute::<_, *const HeapValue>(self).offset(1) }) as _
        }
    }
}

impl Object for HeapValue {
    type ORef = ValueRef;
    type RefIter = ObjRefs;

    fn gsize(&self) -> GSize {
        let gsize = self.typ.uniform_gsize();
        if self.typ.has_dyn_ref_len() {
            return GSize::from(gsize + 1 + unsafe { transmute::<_, &DynHeapValue>(self) }.dyn_len);
        }
        if self.typ.has_dyn_gsize() {
            return GSize::from(gsize + 1)
                 + GSize::from_bytesize(unsafe { transmute::<_, &DynHeapValue>(self) }.dyn_len);
        }
        GSize::from(gsize)
    }

    fn obj_refs(&self) -> ObjRefs {
        let ptr = self.refs_ptr();
        let end = unsafe { ptr.offset(self.ref_len() as isize) };
        ObjRefs { ptr, end }
    }
}

impl DynamicDebug for HeapValue {
    fn fmt<T: Viewer>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        let self_ref =
            ValueRef::from(unsafe { Shared::new_unchecked((self as *const HeapValue) as _) });
        let mut dbg = f.debug_struct("HeapValue");

        if self.link == self_ref {
            dbg.field("link", &"#[cycle]");
        } else {
            dbg.field("link", &self.link.fmt_wrap(types));
        }

        if ValueRef::from(self.typ) == self_ref {
            dbg.field("typ", &"#[cycle]");
        } else {
            dbg.field("typ", &self.typ.fmt_wrap(types));
        }

        dbg.finish()
    }
}

/// A `HeapValue` whose instances are non-uniformly sized.
#[repr(C)]
pub struct DynHeapValue {
    /// The `HeapValue` part.
    pub base: HeapValue,
    /// The length of the dynamic tail on this instance.
    pub dyn_len: usize
}

impl DynamicDebug for DynHeapValue {
    fn fmt<R: Viewer>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_struct("DynHeapValue")
         .field("base", &self.base.fmt_wrap(types))
         .field("dyn_len", &self.dyn_len)
         .finish()
    }
}

/// An iterator over `Shared`:s to `ValueRef` fields of a `HeapValue`.
pub struct ObjRefs {
    ptr: *mut ValueRef,
    end: *mut ValueRef
}

impl Iterator for ObjRefs {
    type Item = Shared<ValueRef>;

    fn next(&mut self) -> Option<Shared<ValueRef>> {
        if self.ptr < self.end {
            let old_ptr = self.ptr;
            self.ptr = unsafe { self.ptr.offset(1) };
            Some(unsafe { Shared::new_unchecked(old_ptr) })
        } else {
            None
        }
    }
}

// ================================================================================================

/// A value reference (tagged pointer).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueRef(usize);

impl ValueRef {
    const SHIFT: usize = 3;
    const TAG_MASK: usize = (1 << Self::SHIFT) - 1;
    const PTR_BIT: usize = 0b001;
    const POINTY_BITS: usize = 0b011;

    /// The null reference.
    pub const NULL: ValueRef = ValueRef(0b001);

    /// Does `self` contain a pointer?
    pub fn is_ptr(self) -> bool { self.0 & Self::PTR_BIT == Self::PTR_BIT }

    unsafe fn unchecked_ptr(self) -> *mut HeapValue {
        (self.0 & !Self::TAG_MASK) as *mut HeapValue
    }

    /// Get the actual (non-`Promise`) reference to `self`. If the link chain ends at an
    /// uninitialized `HeapValue` return `None`.
    pub fn force(self) -> Option<ValueRef> {
        if let Some(sptr) = self.ptr() {
            unsafe { sptr.as_ref() }.force()
        } else {
            Some(self)
        }
    }

    /// Record the fact that `self` points to a `T`.
    pub unsafe fn downcast<T>(self) -> ValueRefT<T> {
        ValueRefT(self, PhantomData::default())
    }

    /// Is `self` an instance of `T`?
    pub fn is_instance<T, R: Reifier<T>>(self, types: &R) -> bool {
        if let Some(sptr) = self.ptr() {
            unsafe { sptr.as_ref() }.typ == types.reify()
        } else {
            false
        }
    }
}

impl From<Shared<HeapValue>> for ValueRef {
    fn from(sptr: Shared<HeapValue>) -> ValueRef {
        ValueRef(sptr.as_ptr() as usize | Self::PTR_BIT)
    }
}

impl<T> From<ValueRefT<T>> for ValueRef {
    fn from(svref: ValueRefT<T>) -> ValueRef { svref.0 }
}

impl ObjectRef for ValueRef {
    type Obj = HeapValue;

    fn ptr(self) -> Option<Shared<HeapValue>> {
        if self.is_ptr() {
            Shared::new((self.0 & !Self::TAG_MASK) as _)
        } else {
            None
        }
    }

    fn is_pointy(self) -> bool { self.0 & Self::POINTY_BITS == Self::POINTY_BITS }
}

impl DynamicDebug for ValueRef {
    fn fmt<R: Viewer>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        types.view(*self).fmt(f, types)
    }
}

// ================================================================================================

pub struct ValueRefT<T>(ValueRef, PhantomData<T>);

impl<T> Clone for ValueRefT<T> {
    fn clone(&self) -> Self { ValueRefT(self.0, self.1) }
}

impl<T> Copy for ValueRefT<T> {}

impl<T> PartialEq for ValueRefT<T> {
    fn eq(&self, other: &ValueRefT<T>) -> bool { self.0 == other.0 }
}

impl<T> Eq for ValueRefT<T> {}

impl<T> Hash for ValueRefT<T> {
    fn hash<H>(&self, state: &mut H) where H: Hasher { self.0.hash(state) }
}

// ================================================================================================

impl From<isize> for ValueRefT<isize> {
    fn from(n: isize) -> ValueRefT<isize> {
        ValueRefT(ValueRef((n as usize) << ValueRef::SHIFT), PhantomData::default())
    }
}

impl From<f64> for ValueRefT<f64> {
    fn from(n: f64) -> ValueRefT<f64> {
        ValueRefT(ValueRef((unsafe { transmute::<_, usize>(n) } & !ValueRef::TAG_MASK) | 0b010),
                  PhantomData::default())
    }
}

impl From<char> for ValueRefT<char> {
    fn from(c: char) -> ValueRefT<char> {
        ValueRefT(ValueRef((c as usize) << ValueRef::SHIFT | 0b100), PhantomData::default())
    }
}

impl From<bool> for ValueRefT<bool> {
    fn from(b: bool) -> ValueRefT<bool> {
        ValueRefT(ValueRef((b as usize) << ValueRef::SHIFT | 0b110), PhantomData::default())
    }
}

impl Unbox for ValueRefT<isize> {
    type Target = isize;

    fn unbox(self) -> isize { ((self.0).0 >> ValueRef::SHIFT) as isize }
}

impl Unbox for ValueRefT<f64> {
    type Target = f64;

    fn unbox(self) -> f64 { unsafe { transmute(((self.0).0 & !ValueRef::TAG_MASK) as f64) } }
}

impl Unbox for ValueRefT<char> {
    type Target = char;

    fn unbox(self) -> char { unsafe { transmute(((self.0).0 >> ValueRef::SHIFT) as u32) } }
}

impl Unbox for ValueRefT<bool> {
    type Target = bool;

    fn unbox(self) -> bool { unsafe { transmute(((self.0).0 >> ValueRef::SHIFT) as u8) } }
}

impl<T: Copy + Debug> Debug for ValueRefT<T> where Self: Unbox<Target=T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.unbox().fmt(f)
    }
}

// ================================================================================================

impl<T: HeapValueSub> From<Unique<T>> for ValueRefT<T> {
    fn from(ptr: Unique<T>) -> ValueRefT<T> {
        ValueRefT(ValueRef(ptr.as_ptr() as usize | ValueRef::PTR_BIT), PhantomData::default())
    }
}

impl<T: HeapValueSub> Deref for ValueRefT<T> {
    type Target = T;

    fn deref(&self) -> &T { unsafe { transmute(self.0.unchecked_ptr()) } }
}

impl<T: HeapValueSub> DerefMut for ValueRefT<T> {
    fn deref_mut(&mut self) -> &mut T { unsafe { transmute(self.0.unchecked_ptr()) } }
}

impl<T: HeapValueSub> AsRef<ValueRef> for ValueRefT<T> {
    fn as_ref(&self) -> &ValueRef { unsafe { transmute(self) } }
}

impl<T: HeapValueSub> AsMut<ValueRef> for ValueRefT<T> {
    fn as_mut(&mut self) -> &mut ValueRef { unsafe { transmute(self) } }
}

impl<T: HeapValueSub + DynamicDebug> DynamicDebug for ValueRefT<T> {
    fn fmt<R: Viewer>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        self.deref().fmt(f, types)
    }
}
