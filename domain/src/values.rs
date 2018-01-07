use std::str;
use std::fmt::{self, Formatter};

use pcws_gc::{GSize, start_init};

use super::{Allocator, Viewer, Reifier, SymbolTable, DynamicDebug};
use object_model::{HeapValueSub, DynHeapValueSub, UniformHeapValue, RefTailed, BlobTailed,
                   HeapValue, DynHeapValue, ValueRef, ValueRefT};

// ================================================================================================

#[derive(Debug)]
pub struct Reinit;

// ================================================================================================

/// Tuple
pub struct Tuple {
    base: DynHeapValue
}

impl Tuple {
    pub fn new<A, I>(allocator: &mut A, len: usize, values: I) -> Option<ValueRefT<Tuple>>
        where A: Allocator, I: Iterator<Item=ValueRef>
    {
        allocator.create_with_iter(|base| Tuple { base }, len, values)
    }

    pub fn values(&self) -> &[ValueRef] { self.tail() }
}

impl HeapValueSub for Tuple {
    const UNIFORM_REF_LEN: usize = 0;
}

impl DynHeapValueSub for Tuple {}

impl RefTailed for Tuple {
    type TailItem = ValueRef;
}

impl DynamicDebug for Tuple {
    fn fmt<T: Viewer>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Tuple")
         .field("base", &self.base.fmt_wrap(types))
         .field("values", &self.values().fmt_wrap(types))
         .finish()
    }
}

/// Symbol (hash-consed string)
pub struct Symbol {
    base: DynHeapValue
}

impl Symbol {
    pub fn new<A: Allocator + SymbolTable>(allocator: &mut A, chars: &str)
        -> Option<ValueRefT<Symbol>>
    {
        allocator.get_symbol(chars)
                 .or_else(|| {
                     let bytes = chars.as_bytes();
                     allocator.create_with_iter(|base| Symbol { base }, bytes.len(), bytes)
                              .map(|sym| {
                                  allocator.insert_symbol(sym);
                                  sym
                              })
                 })
    }

    pub fn chars(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.tail()) }
    }
}

impl HeapValueSub for Symbol {
    const UNIFORM_REF_LEN: usize = 0;
}

impl DynHeapValueSub for Symbol {}

impl BlobTailed for Symbol {
    type TailItem = u8;
}

impl DynamicDebug for Symbol {
    fn fmt<T: Viewer>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Symbol")
         .field("base", &self.base.fmt_wrap(types))
         .field("chars", &self.chars())
         .finish()
    }
}

/// Indirection
pub struct Promise {
    base: HeapValue
}

impl Promise {
    pub fn new<A: Allocator + Reifier<Self>>(allocator: &mut A) -> Option<ValueRefT<Promise>> {
        allocator.allocate_t()
                 .map(|iptr| {
                     let mut uptr = start_init(iptr);
                     *unsafe { uptr.as_mut() } = Promise {
                         base: HeapValue {
                             link: ValueRef::NULL,
                             typ: allocator.reify()
                         }
                     };
                     ValueRefT::from(uptr)
                 })
    }

    pub fn init(&mut self, value: ValueRef) -> Result<(), Reinit> {
        if self.base.link == ValueRef::NULL {
            self.base.link = value;
            Ok(())
        } else {
            Err(Reinit)
        }
    }
}

impl HeapValueSub for Promise {
    const UNIFORM_REF_LEN: usize = 0;
}

impl UniformHeapValue for Promise {}

impl DynamicDebug for Promise {
    fn fmt<R: Viewer>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_struct("Promise")
         .field("base", &self.base.fmt_wrap(types))
         .finish()
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
    fn make<T>(base: HeapValue, has_dyn_gsize: bool, has_dyn_ref_len: bool)
        -> Type where T: HeapValueSub
    {
        Type {
            base,
            gsize_with_dyn: usize::from(GSize::of::<T>()) << 1 | has_dyn_gsize as usize,
            ref_len_with_dyn: T::UNIFORM_REF_LEN << 1 | has_dyn_ref_len as usize
        }
    }

    pub fn new<A, T>(allocator: &mut A, has_dyn_gsize: bool, has_dyn_ref_len: bool)
        -> Option<ValueRefT<Type>> where A: Allocator, T: HeapValueSub
    {
        allocator.create_uniform(|base| Type::make::<T>(base, has_dyn_gsize, has_dyn_ref_len))
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
    const UNIFORM_REF_LEN: usize = 0;
}

impl UniformHeapValue for Type {}

impl DynamicDebug for Type {
    fn fmt<R: Viewer>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_struct("Type")
         .field("heap_value", &self.base.fmt_wrap(types))
         .field("gsize", &self.gsize_with_dyn)
         .field("ref_len", &self.ref_len_with_dyn)
         .finish()
    }
}
