use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::{Unique, NonNull};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::hash::{Hash, Hasher};
use std::slice;
use std::fmt::{self, Debug, Display, Formatter};

use pcws_gc::{GSize, Object, ObjectRef};

use super::Allocator;
use values::Type;

// ================================================================================================

#[derive(Debug, Clone, Copy)]
pub enum Sizing {
    Static,
    DynamicRefs,
    DynamicBlob
}

// ================================================================================================

/// Unboxable scalar reference.
pub trait Unbox {
    type Target: Copy;

    fn unbox(self) -> Self::Target;
}

// ================================================================================================

/// A subtype of `HeapValue`.
pub trait HeapValueSub: Sized {
    const SIZING: Sizing;
    const MIN_REF_LEN: usize;
}

pub trait UniformHeapValue: HeapValueSub {}

/// A dynamically sized `HeapValue` whose tail contains `ValueRef`:s.
pub trait RefTailed: HeapValueSub {
    /// The type of tail elements.
    type TailItem: Into<Option<ValueRef>>;

    /// Get the tail slice.
    fn tail(&self) -> &[Self::TailItem] {
        unsafe {
            slice::from_raw_parts((self as *const Self).offset(1) as *const Self::TailItem,
                                  transmute::<_, &DynHeapValue>(self).dyn_len)
        }
    }
}

/// A dynamically sized `HeapValue` whose tail does not contain `ValueRef`:s.
pub trait BlobTailed: HeapValueSub {
    /// The type of tail elements.
    type TailItem;

    /// Get the tail slice.
    fn tail(&self) -> &[Self::TailItem] {
        unsafe {
            slice::from_raw_parts((self as *const Self).offset(1) as *const Self::TailItem,
                                  transmute::<_, &DynHeapValue>(self).dyn_len)
        }
    }
}

// ================================================================================================

/// The 'supertype' of all heap values (non-scalars).
#[repr(C)]
pub struct HeapValue {
    /// Multipurpose redirection field
    pub link: Option<ValueRef>,
    /// Dynamic type
    pub typ: ValueRefT<Type>
}

impl HeapValue {
    /// See `ValueRef::force`.
    fn force(&self) -> Option<ValueRef> {
        let mut ptr = self as *const HeapValue;
        let mut vref = ValueRef::from(unsafe { NonNull::new_unchecked(ptr as *mut _) });

        loop {
            match unsafe { (*ptr).link } {
                Some(link) if link == vref => return Some(vref),
                None => return None,
                Some(link) => {
                    vref = link;
                    if let Some(sptr) = vref.ptr() {
                        ptr = sptr.as_ptr();
                    } else {
                        return Some(vref);
                    }
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

    fn refs_ptr(&self) -> *mut Option<ValueRef> {
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
        ObjRefs::Link(unsafe { NonNull::new_unchecked(transmute(self)) })
    }
}

impl Debug for HeapValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let self_ref =
            ValueRef::from(unsafe { NonNull::new_unchecked((self as *const HeapValue) as _) });
        let mut dbg = f.debug_struct("HeapValue");

        match self.link {
            Some(link) if link == self_ref => dbg.field("link", &"#[cycle]"),
            Some(_) => dbg.field("link", &self.link),
            None => dbg.field("link", &"#[null]")
        };

        if ValueRef::from(self.typ) == self_ref {
            dbg.field("typ", &"#[cycle]");
        } else {
            dbg.field("typ", &self.typ);
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

impl Debug for DynHeapValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("DynHeapValue")
         .field("base", &self.base)
         .field("dyn_len", &self.dyn_len)
         .finish()
    }
}

/// An iterator over `NonNull`:s to `ValueRef` fields of a `HeapValue`.
pub enum ObjRefs {
    Link(NonNull<HeapValue>),
    Type(NonNull<HeapValue>),
    Fields(*mut Option<ValueRef>, usize)
}

impl Iterator for ObjRefs {
    type Item = NonNull<Option<ValueRef>>;

    fn next(&mut self) -> Option<Self::Item> {
        use self::ObjRefs::*;

        unsafe {
            match *self {
                Link(value) => {
                    *self = Type(value);
                    Some(NonNull::new_unchecked(transmute(&value.as_ref().link)))
                },
                Type(value) => {
                    let value = value.as_ref();
                    *self = Fields(value.refs_ptr(), value.ref_len());
                    Some(NonNull::new_unchecked(transmute(&value.typ)))
                },
                Fields(field_ptr, len) => if len > 0 {
                    *self = Fields(field_ptr.offset(1), len - 1);
                    Some(NonNull::new_unchecked(field_ptr))
                } else {
                    None
                }
            }
        }
    }
}

// ================================================================================================

/// A value reference (tagged pointer).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueRef(NonZero<usize>);

enum ValueView {
    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool),
    HeapValue(NonNull<HeapValue>)
}

impl ValueRef {
    const SHIFT: usize = 3;
    const TAG_MASK: usize = (1 << Self::SHIFT) - 1;
    const PTR_BIT: usize = 0b001;
    const POINTY_BITS: usize = 0b011;

    const INT_TAG  : usize = 0b001;
    const FLOAT_TAG: usize = 0b011;
    const CHAR_TAG : usize = 0b101;
    const BOOL_TAG : usize = 0b111;

    /// Does `self` contain a pointer?
    pub fn is_ptr(self) -> bool { self.0.get() & Self::PTR_BIT == 0 }

    unsafe fn unchecked_ptr(self) -> *mut HeapValue {
        (self.0.get() & !Self::TAG_MASK) as *mut HeapValue
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
    pub fn is_instance<T: HeapValueSub + 'static>(self) -> bool {
        if let Some(sptr) = self.ptr() {
            unsafe { sptr.as_ref() }.typ == Allocator::instance().reify::<T>()
        } else {
            false
        }
    }

    pub fn try_downcast<T: HeapValueSub + 'static>(self) -> Option<ValueRefT<T>> {
        if self.is_instance::<T>() {
            Some(unsafe { self.downcast() })
        } else {
            None
        }
    }

    fn view(self) -> ValueView {
        let self_bits = self.0.get();
        unsafe {
            match self_bits & Self::TAG_MASK {
                Self::INT_TAG   => ValueView::Int((self_bits >> Self::SHIFT) as isize),
                Self::FLOAT_TAG => ValueView::Float(transmute(self_bits & !Self::TAG_MASK)),
                Self::CHAR_TAG  => ValueView::Char(transmute((self_bits >> Self::SHIFT) as u32)),
                Self::BOOL_TAG  => ValueView::Bool(transmute((self_bits >> Self::SHIFT) as u8)),
                _ => ValueView::HeapValue(self.ptr().unwrap())
            }
        }
    }
}

impl From<NonNull<HeapValue>> for ValueRef {
    fn from(sptr: NonNull<HeapValue>) -> ValueRef {
        ValueRef(unsafe { NonZero::new_unchecked(sptr.as_ptr() as usize) })
    }
}

impl<T> From<ValueRefT<T>> for ValueRef {
    fn from(svref: ValueRefT<T>) -> ValueRef { svref.0 }
}

impl AsRef<Option<ValueRef>> for ValueRef {
    fn as_ref(&self) -> &Option<ValueRef> { unsafe { transmute(self) } }
}

impl AsMut<Option<ValueRef>> for ValueRef {
    fn as_mut(&mut self) -> &mut Option<ValueRef> { unsafe { transmute(self) } }
}

impl ObjectRef for ValueRef {
    type Obj = HeapValue;

    fn ptr(self) -> Option<NonNull<HeapValue>> {
        if self.is_ptr() {
            NonNull::new(unsafe { self.unchecked_ptr() })
        } else {
            None
        }
    }

    fn is_pointy(self) -> bool { self.0.get() & Self::POINTY_BITS == Self::POINTY_BITS }
}

impl Debug for ValueRef {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self.view() {
            ValueView::Int(n)   => Debug::fmt(&n, f),
            ValueView::Float(n) => Debug::fmt(&n, f),
            ValueView::Char(c)  => Debug::fmt(&c, f),
            ValueView::Bool(b)  => Debug::fmt(&b, f),
            ValueView::HeapValue(ptr) =>
                Allocator::instance().debug(unsafe { ptr.as_ref() }, f)
        }
    }
}

impl Display for ValueRef {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self.view() {
            ValueView::Int(n)   => Display::fmt(&n, f),
            ValueView::Float(n) => Display::fmt(&n, f),
            ValueView::Char(c)  => Display::fmt(&c, f),
            ValueView::Bool(b)  => Display::fmt(&b, f),
            ValueView::HeapValue(ptr) =>
                Allocator::instance().display(unsafe { ptr.as_ref() }, f)
        }
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

impl<T> From<ValueRefT<T>> for Option<ValueRef> {
    fn from(vref: ValueRefT<T>) -> Option<ValueRef> { Some(vref.into()) }
}

// ================================================================================================

impl From<isize> for ValueRefT<isize> {
    fn from(n: isize) -> ValueRefT<isize> {
        ValueRefT(ValueRef(unsafe { NonZero::new_unchecked(
                               (n as usize) << ValueRef::SHIFT | ValueRef::INT_TAG) }),
                  PhantomData::default())
    }
}

impl From<f64> for ValueRefT<f64> {
    fn from(n: f64) -> ValueRefT<f64> {
        ValueRefT(ValueRef(unsafe { NonZero::new_unchecked(
                               (transmute::<_, usize>(n) & !ValueRef::TAG_MASK)
                               | ValueRef::FLOAT_TAG) }),
                  PhantomData::default())
    }
}

impl From<char> for ValueRefT<char> {
    fn from(c: char) -> ValueRefT<char> {
        ValueRefT(ValueRef(unsafe { NonZero::new_unchecked(
                               (c as usize) << ValueRef::SHIFT | ValueRef::CHAR_TAG) }),
                  PhantomData::default())
    }
}

impl From<bool> for ValueRefT<bool> {
    fn from(b: bool) -> ValueRefT<bool> {
        ValueRefT(ValueRef(unsafe { NonZero::new_unchecked(
                               (b as usize) << ValueRef::SHIFT | ValueRef::BOOL_TAG) }),
                  PhantomData::default())
    }
}

impl Unbox for ValueRefT<isize> {
    type Target = isize;

    fn unbox(self) -> isize { ((self.0).0.get() >> ValueRef::SHIFT) as isize }
}

impl Unbox for ValueRefT<f64> {
    type Target = f64;

    fn unbox(self) -> f64 { unsafe { transmute(((self.0).0.get() & !ValueRef::TAG_MASK) as f64) } }
}

impl Unbox for ValueRefT<char> {
    type Target = char;

    fn unbox(self) -> char { unsafe { transmute(((self.0).0.get() >> ValueRef::SHIFT) as u32) } }
}

impl Unbox for ValueRefT<bool> {
    type Target = bool;

    fn unbox(self) -> bool { unsafe { transmute(((self.0).0.get() >> ValueRef::SHIFT) as u8) } }
}

// impl<T: Copy + Debug> Debug for ValueRefT<T> where Self: Unbox<Target=T> {
//     fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
//         self.unbox().fmt(f)
//     }
// }

// ================================================================================================

impl<T: HeapValueSub> From<Unique<T>> for ValueRefT<T> {
    fn from(ptr: Unique<T>) -> ValueRefT<T> {
        ValueRefT(ValueRef(unsafe { NonZero::new_unchecked(ptr.as_ptr() as usize) }),
                  PhantomData::default())
    }
}

impl<T: HeapValueSub> Deref for ValueRefT<T> {
    type Target = T;

    fn deref(&self) -> &T { unsafe { transmute(self.0.unchecked_ptr()) } }
}

impl<T: HeapValueSub> DerefMut for ValueRefT<T> {
    fn deref_mut(&mut self) -> &mut T { unsafe { transmute(self.0.unchecked_ptr()) } }
}

impl<T: HeapValueSub> AsRef<Option<ValueRef>> for ValueRefT<T> {
    fn as_ref(&self) -> &Option<ValueRef> { unsafe { transmute(self) } }
}

impl<T: HeapValueSub> AsMut<Option<ValueRef>> for ValueRefT<T> {
    fn as_mut(&mut self) -> &mut Option<ValueRef> { unsafe { transmute(self) } }
}

impl<T: HeapValueSub + Debug> Debug for ValueRefT<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.deref().fmt(f)
    }
}

impl<T: HeapValueSub + Display> Display for ValueRefT<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.deref().fmt(f)
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::{ValueRef, ValueRefT};
    use values::Type;

    #[test]
    fn zeroables() {
        assert_eq!(size_of::<Option<ValueRef>>(), size_of::<ValueRef>());
        assert_eq!(size_of::<Option<ValueRefT<Type>>>(), size_of::<ValueRefT<Type>>());
    }
}
