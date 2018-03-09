use std::str;
use std::string;
use std::fmt::{self, Formatter};
use std::collections::HashMap;

use pcws_gc::{GSize, start_init};

use super::{Allocator, DynamicDebug};
use object_model::{HeapValueSub, UniformHeapValue, RefTailed, BlobTailed, Sizing,
                   HeapValue, ValueRef, ValueRefT};

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
            const UNIFORM_REF_LEN: usize = count_vrefs!($($field_types),*);
            const SIZING: $crate::object_model::Sizing = $sizing;

            fn type_index(allocator: &mut $crate::Allocator) -> usize {
                use std::sync::atomic::AtomicIsize;
                use std::sync::atomic::Ordering::SeqCst;

                static INDEX: AtomicIsize = AtomicIsize::new(-1);

                unsafe fn fmt_fn(value: &$crate::object_model::HeapValue, f: &mut Formatter,
                                 types: &Allocator) -> Result<(), fmt::Error>
                {
                    use std::mem::transmute;
                    transmute::<_, &$name>(value).fmt(f, types)
                }

                let old = INDEX.load(SeqCst);
                if old >= 0 {
                    old as usize // Already initialized.
                } else {
                    let typ = $crate::values::Type
                                    ::new::<Self>(allocator, Self::SIZING)
                                    .unwrap(); // FIXME: unwrap
                    let new = allocator.register_typ(typ, fmt_fn);
                    if INDEX.compare_and_swap(old, new as isize, SeqCst) == old {
                        new // We won the race.
                    } else {
                        allocator.deregister_typ(new); // Other thread won; roll back...
                        INDEX.load(SeqCst) as usize    // ...and use their value.
                    }
                }
            }
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
    pub fn new<I>(allocator: &mut Allocator, len: usize, values: I) -> Option<ValueRefT<Tuple>>
        where I: Iterator<Item=ValueRef>
    {
        allocator.create_with_iter(|base| Tuple { base }, len, values)
    }
}

impl DynamicDebug for Tuple {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Tuple")
         .field("base", &self.base.fmt_wrap(types))
         .field("tail", &self.tail().fmt_wrap(types))
         .finish()
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

impl DynamicDebug for String {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("String")
         .field("base", &self.base.fmt_wrap(types))
         .field("chars", &self.chars())
         .finish()
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

impl DynamicDebug for Symbol {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Symbol")
         .field("base", &self.base.fmt_wrap(types))
         .field("chars", &self.chars())
         .finish()
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
                             link: ValueRef::NULL,
                             typ: allocator.reify::<Promise>()
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

impl DynamicDebug for Promise {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Promise")
         .field("base", &self.base.fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// A dynamic type.
#[repr(C)]
pub struct Type {
    base: HeapValue,
    gsize_with_dyn: usize,
    ref_len_with_dyn: usize
}

impl HeapValueSub for Type {
    const UNIFORM_REF_LEN: usize = 0;
    const SIZING: Sizing = Sizing::Static;

    fn type_index(_: &mut Allocator) -> usize {
        // Since the type of types is created eagerly in the constructor of `Allocator`, its type
        // index will always be:
        0
    }
}

impl UniformHeapValue for Type {}

impl Type {
    pub fn make<T: HeapValueSub>(base: HeapValue, sizing: Sizing) -> Type {
        Type {
            base,
            gsize_with_dyn: usize::from(GSize::of::<T>()) << 1 | match sizing {
                Sizing::Static => 0,
                Sizing::DynamicRefs | Sizing::DynamicBlob => 1
            },
            ref_len_with_dyn: T::UNIFORM_REF_LEN << 1 | match sizing {
                Sizing::Static | Sizing::DynamicBlob => 0,
                Sizing::DynamicRefs => 1
            }
        }
    }

    pub fn new<T: HeapValueSub>(allocator: &mut Allocator, sizing: Sizing)
        -> Option<ValueRefT<Type>>
    {
        allocator.create_uniform(|base| Type::make::<T>(base, sizing))
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

impl DynamicDebug for Type {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Type")
         .field("heap_value", &self.base.fmt_wrap(types))
         .field("gsize_with_dyn", &self.gsize_with_dyn)
         .field("ref_len_with_dyn", &self.ref_len_with_dyn)
         .finish()
    }
}
