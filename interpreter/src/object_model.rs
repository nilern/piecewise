use std::ptr::{Unique, Shared};
use std::mem::transmute;
use std::ops::{Deref, DerefMut};
use std::fmt::{self, Debug, Formatter};
use std::marker::PhantomData;
use std::hash::{Hash, Hasher};
use std::slice;

use interpreter::Allocator;
use gce::{GSize, Object, ObjectRef};
use value::{ValueView, TypeIndex, TypeRegistry};

// ================================================================================================

/// Unboxable scalar reference.
pub trait Unbox {
    type Target: Copy;

    fn unbox(self) -> Self::Target;
}

/// A subtype of `HeapValue`.
pub trait HeapValueSub: Sized {
    /// Type index of `Self`.
    const TYPE_INDEX: TypeIndex;
    /// The constant portion (or minimum number) of `ValueRef` fields on instances of `self`.
    const UNIFORM_REF_LEN: usize;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>>;
}

/// A subtype of `DynHeapValue`.
pub trait DynHeapValueSub: HeapValueSub {
    /// The type of tail items.
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

/// Like `std::fmt::Debug`, but needs a `TypeRegistry` because of the dynamic typing.
pub trait DynamicDebug: Sized {
    /// Formats the value using the given formatter.
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error>;

    /// Wrap `self` and `types` into a `DynDebugWrapper`.
    fn fmt_wrap<'a, 'b, R: TypeRegistry>(&'a self, types: &'b R)
        -> DynDebugWrapper<'a, 'b, Self, R>
    {
        DynDebugWrapper {
            value: self,
            types: types
        }
    }
}

/// Wraps a `DynamicDebug` and a `TypeRegistry` into a struct that implements `fmt::Debug`.
pub struct DynDebugWrapper<'a, 'b, T: 'a + DynamicDebug, R: 'b + TypeRegistry> {
    value: &'a T,
    types: &'b R
}

impl<'a, 'b, T: DynamicDebug, R: TypeRegistry> Debug for DynDebugWrapper<'a, 'b, T, R> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(f, self.types)
    }
}

impl<'a, T> DynamicDebug for &'a [T] where T: DynamicDebug {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_list()
         .entries(self.iter().map(|entry| entry.fmt_wrap(types)))
         .finish()
    }
}

// ================================================================================================

/// The 'supertype' of all heap values (non-scalars).
#[repr(C)]
pub struct HeapValue {
    /// Multipurpose redirection field
    pub link: ValueRef,
    /// Dynamic type
    pub typ: HeapValueRef<Type>
}

impl HeapValue {
    /// See `ValueRef::force`.
    fn force(&self) -> Option<ValueRef> {
        let mut ptr = self as *const HeapValue;
        let mut vref = ValueRef::from(ptr);

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
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        let self_ref = ValueRef::from(self as *const HeapValue);
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
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
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

/// A dynamic type.
#[repr(C)]
pub struct Type {
    base: HeapValue,
    gsize_with_dyn: usize,
    ref_len_with_dyn: usize
}

impl Type {
    pub fn make<T>(base: HeapValue, has_dyn_gsize: bool, has_dyn_ref_len: bool)
        -> Type where T: HeapValueSub
    {
        Type {
            base,
            gsize_with_dyn: usize::from(GSize::of::<T>()) << 1 | has_dyn_gsize as usize,
            ref_len_with_dyn: T::UNIFORM_REF_LEN << 1 | has_dyn_ref_len as usize
        }
    }

    fn new<T>(allocator: &mut Allocator, has_dyn_gsize: bool, has_dyn_ref_len: bool)
        -> Option<HeapValueRef<Type>> where T: HeapValueSub
    {
        allocator.uniform_create(|base| Type::make::<T>(base, has_dyn_gsize, has_dyn_ref_len))
    }

    /// Create a new dynamic type for `T` with uniformly sized instances.
    pub fn uniform<T: HeapValueSub>(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::new::<T>(allocator, false, false)
    }

    /// Create a new dynamic type for `T` with `ValueRef`-tailed instances.
    pub fn dyn_refs<T>(allocator: &mut Allocator) -> Option<HeapValueRef<Type>>
        where T: DynHeapValueSub, T::TailItem: Into<ValueRef>
    {
        Type::new::<T>(allocator, true, true)
    }

    /// Create a new dynamic type for `T` with byte-tailed instances.
    pub fn dyn_bytes<T: DynHeapValueSub>(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::new::<T>(allocator, true, false)
    }

    /// The constant portion (or minimum) granule size of instances.
    pub fn uniform_gsize(&self) -> usize { self.gsize_with_dyn >> 1 }

    /// Does the size of instances vary?
    pub fn has_dyn_gsize(&self) -> bool { self.gsize_with_dyn & 0b1 == 1 }

    /// The constant portion (or minimum) number of `ValueRef` fields of instances.
    pub fn uniform_ref_len(&self) -> usize { self.ref_len_with_dyn >> 1 }

    /// Does the number of `ValueRef` fields of instances vary?
    pub fn has_dyn_ref_len(&self) -> bool { self.ref_len_with_dyn & 0b1 == 1 }
}

impl HeapValueSub for Type {
    const TYPE_INDEX: TypeIndex = TypeIndex::Type;
    const UNIFORM_REF_LEN: usize = 0;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::uniform::<Self>(allocator)
    }
}

impl DynamicDebug for Type {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Type")
         .field("heap_value", &self.base.fmt_wrap(types))
         .field("gsize", &self.gsize_with_dyn)
         .field("ref_len", &self.ref_len_with_dyn)
         .finish()
    }
}

// ================================================================================================

const SHIFT: usize = 3;
const TAG_MASK: usize = (1 << SHIFT) - 1;
const PTR_BIT: usize = 0b001;
const POINTY_BITS: usize = 0b011;

// ================================================================================================

/// A value reference (tagged pointer).
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ValueRef(usize);

impl ValueRef {
    /// The null reference.
    pub const NULL: ValueRef = ValueRef(0b001);

    /// Does `self` contain a pointer?
    pub fn is_ptr(self) -> bool { self.0 & PTR_BIT == PTR_BIT }

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
    pub unsafe fn downcast<T: HeapValueSub>(self) -> HeapValueRef<T> {
        HeapValueRef(self.0, PhantomData::default())
    }

    /// Get the corresponding `ValueView`.
    pub fn view<T: TypeRegistry>(self, type_reg: &T) -> ValueView {
        if self == Self::NULL {
            ValueView::Null
        } else if let Some(sptr) = self.ptr() {
            match type_reg.index_of(unsafe { sptr.as_ref() }.typ) {
                TypeIndex::Type   => ValueView::Type(unsafe { self.downcast() }),
                TypeIndex::Tuple  => ValueView::Tuple(unsafe { self.downcast() }),
                TypeIndex::Symbol => ValueView::Symbol(unsafe { self.downcast() }),

                TypeIndex::Promise => ValueView::Promise(unsafe { self.downcast() }),

                TypeIndex::Function => ValueView::Function(unsafe { self.downcast() }),
                TypeIndex::Method   => ValueView::Method(unsafe { self.downcast() }),
                TypeIndex::Block    => ValueView::Block(unsafe { self.downcast() }),
                TypeIndex::Call     => ValueView::Call(unsafe { self.downcast() }),
                TypeIndex::Def      => ValueView::Def(unsafe { self.downcast() }),
                TypeIndex::Const    => ValueView::Const(unsafe { self.downcast() }),
                TypeIndex::Lex      => ValueView::Lex(unsafe { self.downcast() }),

                TypeIndex::BlockCont  => ValueView::BlockCont(unsafe { self.downcast() }),
                TypeIndex::DefCont    => ValueView::DefCont(unsafe { self.downcast() }),
                TypeIndex::CalleeCont => ValueView::CalleeCont(unsafe { self.downcast() }),
                TypeIndex::ArgCont    => ValueView::ArgCont(unsafe { self.downcast() }),
                TypeIndex::Halt       => ValueView::Halt(unsafe { self.downcast() }),

                TypeIndex::Env     => ValueView::Env(unsafe { self.downcast() }),
                TypeIndex::Closure => ValueView::Closure(unsafe { self.downcast() })
            }
        } else {
            match self.0 & TAG_MASK {
                0b000 => ValueView::Int(unsafe { transmute(self) }),
                0b010 => ValueView::Float(unsafe { transmute(self) }),
                0b100 => ValueView::Char(unsafe { transmute(self) }),
                0b110 => ValueView::Bool(unsafe { transmute(self) }),
                _ => unreachable!()
            }
        }
    }

    /// Is `self` an instance of `T`?
    pub fn is_instance<R: TypeRegistry, T: HeapValueSub>(self, types: &R) -> bool {
        if let Some(sptr) = self.ptr() {
            types.index_of(unsafe { sptr.as_ref() }.typ) == T::TYPE_INDEX
        } else {
            false
        }
    }
}

impl From<*const HeapValue> for ValueRef {
    fn from(ptr: *const HeapValue) -> ValueRef { ValueRef(ptr as usize | PTR_BIT) }
}

impl<T: Copy> From<ScalarValueRef<T>> for ValueRef {
    fn from(svref: ScalarValueRef<T>) -> ValueRef { ValueRef(svref.0) }
}

impl<T: HeapValueSub> From<HeapValueRef<T>> for ValueRef {
    fn from(tvref: HeapValueRef<T>) -> ValueRef { ValueRef(tvref.0) }
}

impl ObjectRef for ValueRef {
    type Obj = HeapValue;

    fn ptr(self) -> Option<Shared<HeapValue>> {
        if self.is_ptr() {
            Shared::new((self.0 & !TAG_MASK) as _)
        } else {
            None
        }
    }

    fn is_pointy(self) -> bool { self.0 & POINTY_BITS == POINTY_BITS }
}

impl DynamicDebug for ValueRef {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        self.view(type_reg).fmt(f, type_reg)
    }
}

// ================================================================================================

/// A scalar reference that unboxes to a `T`.
pub struct ScalarValueRef<T: Copy>(usize, PhantomData<T>);

impl<T: Copy> Clone for ScalarValueRef<T> {
    fn clone(&self) -> Self { ScalarValueRef(self.0, self.1) }
}

impl<T: Copy> Copy for ScalarValueRef<T> {}

impl From<isize> for ScalarValueRef<isize> {
    fn from(n: isize) -> ScalarValueRef<isize> {
        ScalarValueRef((n as usize) << SHIFT, PhantomData::default())
    }
}

impl From<f64> for ScalarValueRef<f64> {
    fn from(n: f64) -> ScalarValueRef<f64> {
        ScalarValueRef((unsafe { transmute::<_, usize>(n) } & !TAG_MASK) | 0b010,
                       PhantomData::default())
    }
}

impl From<char> for ScalarValueRef<char> {
    fn from(c: char) -> ScalarValueRef<char> {
        ScalarValueRef((c as usize) << SHIFT | 0b100, PhantomData::default())
    }
}

impl From<bool> for ScalarValueRef<bool> {
    fn from(b: bool) -> ScalarValueRef<bool> {
        ScalarValueRef((b as usize) << SHIFT | 0b110, PhantomData::default())
    }
}

impl Unbox for ScalarValueRef<isize> {
    type Target = isize;

    fn unbox(self) -> isize { (self.0 >> SHIFT) as isize }
}

impl Unbox for ScalarValueRef<f64> {
    type Target = f64;

    fn unbox(self) -> f64 { unsafe { transmute((self.0 & !TAG_MASK) as f64) } }
}

impl Unbox for ScalarValueRef<char> {
    type Target = char;

    fn unbox(self) -> char { unsafe { transmute((self.0 >> SHIFT) as u32) } }
}

impl Unbox for ScalarValueRef<bool> {
    type Target = bool;

    fn unbox(self) -> bool { unsafe { transmute((self.0 >> SHIFT) as u8) } }
}

impl<T: Copy + Debug> Debug for ScalarValueRef<T> where Self: Unbox<Target=T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.unbox().fmt(f)
    }
}

// ================================================================================================

/// A statically typed `ValueRef` to a `HeapValueSub`.
pub struct HeapValueRef<T: HeapValueSub>(usize, PhantomData<T>);

impl<T: HeapValueSub> Clone for HeapValueRef<T> {
    fn clone(&self) -> Self { HeapValueRef(self.0, self.1) }
}

impl<T: HeapValueSub> Copy for HeapValueRef<T> {}

impl<T: HeapValueSub> PartialEq for HeapValueRef<T> {
    fn eq(&self, other: &HeapValueRef<T>) -> bool { self.0 == other.0 }
}

impl<T: HeapValueSub> Eq for HeapValueRef<T> {}

impl<T: HeapValueSub> Hash for HeapValueRef<T> {
    fn hash<H>(&self, state: &mut H) where H: Hasher { self.0.hash(state) }
}

impl<T: HeapValueSub> HeapValueRef<T> {
    /// Convert from a (freshly allocated) pointer.
    pub fn new(ptr: Unique<T>) -> HeapValueRef<T> {
        HeapValueRef(ptr.as_ptr() as usize | PTR_BIT, PhantomData::default())
    }
}

impl<T: HeapValueSub> AsRef<ValueRef> for HeapValueRef<T> {
    fn as_ref(&self) -> &ValueRef { unsafe { transmute(self) } }
}

impl<T: HeapValueSub> AsMut<ValueRef> for HeapValueRef<T> {
    fn as_mut(&mut self) -> &mut ValueRef { unsafe { transmute(self) } }
}

impl<T: HeapValueSub> Deref for HeapValueRef<T> {
    type Target = T;

    fn deref(&self) -> &T { unsafe { transmute(self.0 & !TAG_MASK) } }
}

impl<T: HeapValueSub> DerefMut for HeapValueRef<T> {
    fn deref_mut(&mut self) -> &mut T { unsafe { transmute(self.0 & !TAG_MASK) } }
}

impl<T: HeapValueSub + DynamicDebug> DynamicDebug for HeapValueRef<T> {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, type_reg: &R) -> Result<(), fmt::Error> {
        self.deref().fmt(f, type_reg)
    }
}
