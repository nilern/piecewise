use std::mem::transmute;
use std::str;
use std::fmt::{self, Formatter};
use std::collections::HashMap;
use std::sync::atomic::AtomicIsize;
use std::sync::atomic::Ordering::SeqCst;

use pcws_gc::{GSize, start_init};

use super::{Allocator, DynamicDebug};
use object_model::{HeapValueSub, UniformHeapValue, RefTailed, BlobTailed, Sizing,
                   HeapValue, DynHeapValue, ValueRef, ValueRefT};

// ================================================================================================

macro_rules! count_vrefs {
    () => { 0 };
    ( ValueRef $(, $Ts:ty)* ) => { 1 + count_vrefs!($($Ts),*) };
    ( ValueRefT<$T:ty> $(, $Ts:ty)* ) => { 1 + count_vrefs!($($Ts),*) };
    ( $T:ty $(, $Ts:ty)* ) => { count_vrefs!($($Ts),*) };
}

macro_rules! heap_struct_base {
    { pub struct $name:ident : $base:ty { $($field_names:ident : $field_types:ty),* }
      const SIZING: Sizing = $sizing:expr; } => {
        #[repr(C)]
        pub struct $name {
            base: $base,
            $($field_names : $field_types),*
        }

        impl HeapValueSub for $name {
            const UNIFORM_REF_LEN: usize = count_vrefs!($($field_types),*);
            const SIZING: Sizing = $sizing;

            fn type_index(allocator: &mut Allocator) -> usize {
                static INDEX: AtomicIsize = AtomicIsize::new(-1);

                unsafe fn fmt_fn(value: &HeapValue, f: &mut Formatter, types: &Allocator)
                    -> Result<(), fmt::Error>
                {
                    transmute::<_, &$name>(value).fmt(f, types)
                }

                let old = INDEX.load(SeqCst);
                if old >= 0 {
                    old as usize // Already initialized.
                } else {
                    let new = allocator.register_typ::<Self>(fmt_fn);
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

macro_rules! heap_struct {
    {
        pub struct $name:ident : UniformHeapValue {
            $($field_names:ident : $field_types:ty),*
        }
    } => {
        heap_struct_base! {
            pub struct $name : HeapValue {
                $($field_names : $field_types),*
            }

            const SIZING: Sizing = Sizing::Static;
        }

        impl UniformHeapValue for $name {}
    };

    {
        pub struct $name:ident : RefTailed<TailItem=$tail_typ:ty> {
            $($field_names:ident : $field_types:ty),*
        }
    } => {
        heap_struct_base! {
            pub struct $name : DynHeapValue {
                $($field_names : $field_types),*
            }

            const SIZING: Sizing = Sizing::DynamicRefs;
        }

        impl RefTailed for $name { type TailItem = $tail_typ; }
    };

    {
        pub struct $name:ident : BlobTailed<TailItem=$tail_typ:ty> {
            $($field_names:ident : $field_types:ty),*
        }
    } => {
        heap_struct_base! {
            pub struct $name : DynHeapValue {
                $($field_names : $field_types),*
            }

            const SIZING: Sizing = Sizing::DynamicBlob;
        }

        impl BlobTailed for $name { type TailItem = $tail_typ; }
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
        where I: IntoIterator<Item=ValueRef>
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

/// Symbol (hash-consed string)
heap_struct! {
    pub struct Symbol: BlobTailed<TailItem=u8> {}
}

impl Symbol {
    pub fn new(allocator: &mut Allocator, chars: &str) -> Option<ValueRefT<Symbol>> {
        allocator.symbol_table().get_symbol(chars)
                 .or_else(|| {
                     let bytes = chars.as_bytes();
                     allocator.create_with_iter(|base| Symbol { base }, bytes.len(), bytes)
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
pub struct SymbolTable(HashMap<String, ValueRefT<Symbol>>);

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
heap_struct! {
    pub struct Type: UniformHeapValue {
        gsize_with_dyn: usize,
        ref_len_with_dyn: usize
    }
}

impl Type {
    pub fn new<T: HeapValueSub>(allocator: &mut Allocator, sizing: Sizing)
        -> Option<ValueRefT<Type>>
    {
        allocator.create_uniform(|base| Type {
            base,
            gsize_with_dyn: usize::from(GSize::of::<T>()) << 1 | match sizing {
                Sizing::Static => 0,
                Sizing::DynamicRefs | Sizing::DynamicBlob => 1
            },
            ref_len_with_dyn: T::UNIFORM_REF_LEN << 1 | match sizing {
                Sizing::Static | Sizing::DynamicBlob => 0,
                Sizing::DynamicRefs => 1
            }
        })
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
