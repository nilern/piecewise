use std::mem::transmute;
use std::str;
use std::string;
use std::fmt::{self, Debug, Display, Write, Formatter};
use std::collections::HashMap;

use pcws_gc::{GSize, start_init, Generation};

use super::{Allocator, TypeRegistry};
use object_model::{HeapValueSub, RefTailed, BlobTailed, Sizing, HeapValue, ValueRef, ValueRefT};

// ================================================================================================

#[macro_export]
macro_rules! count_vrefs {
    () => { 0 };
    ( ValueRef $(, $Ts:ty)* ) => { 1 + count_vrefs!($($Ts),*) };
    ( ValueRefT<$T:ty> $(, $Ts:ty)* ) => { 1 + count_vrefs!($($Ts),*) };
    ( $T:ty $(, $Ts:ty)* ) => { count_vrefs!($($Ts),*) };
}

#[macro_export]
macro_rules! heap_struct_base {
    { pub struct $name:ident : $base:ty { $($field_names:ident : $field_types:ty),* }
      const SIZING: $crate::object_model::Sizing = $sizing:expr; } => {
        #[repr(C)]
        pub struct $name {
            base: $base,
            $($field_names : $field_types),*
        }

        impl $crate::object_model::HeapValueSub for $name {
            const SIZING: $crate::object_model::Sizing = $sizing;
            const MIN_REF_LEN: usize = count_vrefs!($($field_types),*);
        }
    }
}

#[macro_export]
macro_rules! heap_struct {
    {
        pub struct $name:ident : UniformHeapValue {
            $($field_names:ident : $field_types:ty),*
        }
    } => {
        heap_struct_base! {
            pub struct $name : $crate::object_model::HeapValue {
                $($field_names : $field_types),*
            }

            const SIZING: $crate::object_model::Sizing = $crate::object_model::Sizing::Static;
        }

        impl $crate::object_model::UniformHeapValue for $name {}
    };

    {
        pub struct $name:ident : RefTailed<TailItem=$tail_typ:ty> {
            $($field_names:ident : $field_types:ty),*
        }
    } => {
        heap_struct_base! {
            pub struct $name : $crate::object_model::DynHeapValue {
                $($field_names : $field_types),*
            }

            const SIZING: $crate::object_model::Sizing = $crate::object_model::Sizing::DynamicRefs;
        }

        impl $crate::object_model::RefTailed for $name { type TailItem = $tail_typ; }
    };

    {
        pub struct $name:ident : BlobTailed<TailItem=$tail_typ:ty> {
            $($field_names:ident : $field_types:ty),*
        }
    } => {
        heap_struct_base! {
            pub struct $name : $crate::object_model::DynHeapValue {
                $($field_names : $field_types),*
            }

            const SIZING: $crate::object_model::Sizing = $crate::object_model::Sizing::DynamicBlob;
        }

        impl $crate::object_model::BlobTailed for $name { type TailItem = $tail_typ; }
    }
}

// ================================================================================================

#[derive(Debug)]
pub struct Reinit;

// ================================================================================================

/// Tuple
heap_struct! {
    pub struct Tuple: RefTailed<TailItem=ValueRef> {}
}

impl Tuple {
    pub fn new<I: Iterator<Item=ValueRef>>(allocator: &mut Allocator, len: usize, values: I)
        -> Option<ValueRefT<Tuple>>
    {
        allocator.create_with_iter(|base| Tuple { base }, len, values)
    }

    pub fn vals(&self) -> &[ValueRef] { self.tail() }
}

impl Debug for Tuple {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Tuple")
         .field("base", &self.base)
         .field("tail", &self.tail())
         .finish()
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.write_char('(')?;
        let mut vals = self.tail().iter();
        if let Some(v) = vals.next() {
            <_ as Display>::fmt(v, f)?;
        }
        for v in vals {
            write!(f, ", {}", v)?;
        }
        f.write_char(')')
    }
}

// ================================================================================================

heap_struct! {
    pub struct String: BlobTailed<TailItem=u8> {}
}

impl String {
    pub fn new(allocator: &mut Allocator, chars: &str) -> Option<ValueRefT<String>> {
        let bytes = chars.as_bytes();
        allocator.create_with_slice(|base| String { base }, bytes)
    }

    pub fn chars(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.tail()) }
    }
}

impl Debug for String {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("String")
         .field("base", &self.base)
         .field("chars", &self.chars())
         .finish()
    }
}

impl Display for String {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self.chars())
    }
}

// ================================================================================================

/// Symbol (hash-consed string)
heap_struct! {
    pub struct Symbol: BlobTailed<TailItem=u8> {}
}

impl Symbol {
    pub fn new(allocator: &mut Allocator, chars: &str) -> Option<ValueRefT<Symbol>> {
        allocator.symbol_table().get_symbol(chars)
                 .or_else(|| {
                     let bytes = chars.as_bytes();
                     allocator.create_with_slice(|base| Symbol { base }, bytes)
                              .map(|sym| {
                                  allocator.symbol_table().insert_symbol(sym);
                                  sym
                              })
                 })
    }

    pub fn chars(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.tail()) }
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Symbol")
         .field("base", &self.base)
         .field("chars", &self.chars())
         .finish()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, ":{}", self.chars())
    }
}

// OPTIMIZE: Use a key that does not duplicate the characters.
/// Symbol table for interning symbols
pub struct SymbolTable(HashMap<string::String, ValueRefT<Symbol>>);

impl SymbolTable {
    pub fn new() -> SymbolTable { SymbolTable(HashMap::new()) }

    fn get_symbol(&self, chars: &str) -> Option<ValueRefT<Symbol>> {
        self.0.get(chars).map(|&vref| vref)
    }

    fn insert_symbol(&mut self, symbol: ValueRefT<Symbol>) {
        self.0.insert(symbol.chars().to_string(), symbol);
    }

    pub fn mark_roots(&mut self, heap: &mut Generation<ValueRef>) {
        for v in self.0.values_mut() {
            *v = unsafe { transmute(heap.mark_ref((*v).into())) };
        }
    }
}

// ================================================================================================

/// Indirection
heap_struct! {
    pub struct Promise: UniformHeapValue {}
}

impl Promise {
    pub fn new(allocator: &mut Allocator) -> Option<ValueRefT<Promise>> {
        allocator.allocate_t()
                 .map(|iptr| {
                     let mut uptr = start_init(iptr);
                     *unsafe { uptr.as_mut() } = Promise {
                         base: HeapValue {
                             link: None,
                             typ: TypeRegistry::instance().reify::<Promise>()
                         }
                     };
                     ValueRefT::from(uptr)
                 })
    }

    pub fn init(&mut self, value: ValueRef) -> Result<(), Reinit> {
        if self.base.link.is_none() {
            self.base.link = Some(value);
            Ok(())
        } else {
            Err(Reinit)
        }
    }
}

impl Debug for Promise {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Promise")
         .field("base", &self.base)
         .finish()
    }
}

impl Display for Promise {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        <_ as Display>::fmt(&self.base.link.unwrap(), f)
    }
}

// ================================================================================================

/// A dynamic type.
heap_struct! {
    pub struct Type: UniformHeapValue {
        gsize_with_dyn: usize,
        ref_len_with_dyn: usize
    }
}

impl Type {
    pub fn make(base: HeapValue, sizing: Sizing, min_gsize: GSize, min_ref_len: usize) -> Type {
        Type {
            base,
            gsize_with_dyn: usize::from(min_gsize) << 1 | match sizing {
                Sizing::Static => 0,
                Sizing::DynamicRefs | Sizing::DynamicBlob => 1
            },
            ref_len_with_dyn: min_ref_len << 1 | match sizing {
                Sizing::Static | Sizing::DynamicBlob => 0,
                Sizing::DynamicRefs => 1
            }
        }
    }

    pub fn from_static<T: HeapValueSub>(allocator: &mut Allocator) -> Option<ValueRefT<Type>> {
        allocator.create_uniform(|base|
            Type::make(base, T::SIZING, GSize::of::<T>(), T::MIN_REF_LEN)
        )
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

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Type")
         .field("heap_value", &self.base)
         .field("gsize_with_dyn", &self.gsize_with_dyn)
         .field("ref_len_with_dyn", &self.ref_len_with_dyn)
         .finish()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "Type({:x}, {:x})", self.gsize_with_dyn, self.ref_len_with_dyn)
    }
}
