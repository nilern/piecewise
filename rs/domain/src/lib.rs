#![feature(nonzero, unique, ptr_internals, const_atomic_isize_new)]

extern crate core;

extern crate pcws_gc;

pub mod object_model;
pub mod values;

use core::nonzero::NonZero;
use std::mem::size_of;
use std::ptr::{Unique, NonNull};
use std::slice;
use std::fmt::{self, Debug, Formatter};
use std::collections::HashMap;

use pcws_gc::{GSize, Initializable, start_init, Generation};
use object_model::{HeapValueSub, HeapValue, DynHeapValue, ValueRef, ValueRefT};
use values::Type;

pub use values::SymbolTable;

// ================================================================================================

type UnsafeFmtFn = unsafe fn(&HeapValue, &mut Formatter, &Allocator) -> Result<(), fmt::Error>;

/// Value allocator.
pub struct Allocator {
    gc: Generation<ValueRef>,

    types: HashMap<usize, ValueRefT<Type>>,
    type_indices: HashMap<ValueRefT<Type>, usize>,
    fmt_fns: HashMap<usize, UnsafeFmtFn>,
    type_counter: usize,

    symbols: SymbolTable
}

impl Allocator {
    pub fn new(max_heap: usize) -> Allocator {
        unsafe fn fmt_fn(value: &HeapValue, f: &mut Formatter, types: &Allocator)
            -> Result<(), fmt::Error>
        {
            use std::mem::transmute;
            transmute::<_, &Type>(value).fmt(f, types)
        }

        let mut allocator = Allocator {
            gc: Generation::new(max_heap),

            types: HashMap::new(),
            type_indices: HashMap::new(),
            fmt_fns: HashMap::new(),
            type_counter: 0,

            symbols: SymbolTable::new()
        };

        let type_type: Initializable<Type> = allocator.allocate_t().unwrap();
        allocator.register_typ(ValueRefT::from(start_init(type_type)), fmt_fn);
        allocator.init_uniform(type_type, |base| Type::make::<Type>(base, Type::SIZING));

        allocator
    }

    fn init<T, F>(&mut self, ptr: Initializable<T>, f: F) -> ValueRefT<T>
        where T: HeapValueSub, F: FnOnce(Unique<T>, HeapValue)
    {
        let uptr = start_init(ptr);
        let tvref = ValueRefT::from(uptr);
        f(uptr, HeapValue {
            link: tvref.into(),
            typ: self.reify::<T>()
        });
        tvref
    }

    /// Initialize a `T`, delegating to `f` for everything but the `HeapValue` part.
    fn init_uniform<T, F>(&mut self, ptr: Initializable<T>, f: F) -> ValueRefT<T>
        where T: HeapValueSub, F: FnOnce(HeapValue) -> T
    {
        self.init(ptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(heap_value);
        })
    }

    fn init_with_iter<T, F, I, E>(&mut self, iptr: Initializable<T>, f: F, len: usize, iter: I)
        -> ValueRefT<T>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T,
              I: Iterator<Item=E>, E: Copy
    {
        self.init(iptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(DynHeapValue {
                base: heap_value,
                dyn_len: len
            });
            let dest_slice: &mut[E] = unsafe {
                slice::from_raw_parts_mut(uptr.as_ptr().offset(1) as *mut E, len)
            };
            for (loc, value) in dest_slice.iter_mut().zip(iter) {
                *loc = value;
            }
        })
    }

    unsafe fn allocate<T>(&mut self, galign: GSize, gsize: GSize) -> Option<Initializable<T>> {
        self.gc.allocate(NonZero::new_unchecked(galign), NonZero::new_unchecked(gsize))
    }

    fn allocate_t<T>(&mut self) -> Option<Initializable<T>> {
        unsafe { self.allocate(GSize::from(1), GSize::of::<T>()) }
    }

    pub fn create_uniform<T, F>(&mut self, f: F) -> Option<ValueRefT<T>>
        where T: HeapValueSub, F: Fn(HeapValue) -> T
    {
        self.allocate_t().map(|typ| self.init_uniform(typ, f))
    }

    pub fn create_with_iter<T, F, I, E>(&mut self, f: F, len: usize, iter: I)
        -> Option<ValueRefT<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, I: Iterator<Item=E>, E: Copy
    {
        let gsize = GSize::from_bytesize(size_of::<T>() + len*size_of::<E>());
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(gsize)) }
            .map(|iptr| self.init_with_iter(iptr, f, len, iter))
    }

    pub fn create_with_slice<T, F, E>(&mut self, f: F, slice: &[E]) -> Option<ValueRefT<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy
    {
        self.create_with_iter(f, slice.len(), slice.iter().cloned())
    }

    pub fn reify<T: HeapValueSub>(&mut self) -> ValueRefT<Type> {
        let index = T::type_index(self);
        self.types[&index]
    }

    pub fn register_typ(&mut self, typ: ValueRefT<Type>, f: UnsafeFmtFn) -> usize {
        let index = self.type_counter;
        self.types.insert(index, typ);
        self.type_indices.insert(typ, index);
        self.fmt_fns.insert(index, f);
        self.type_counter += 1;
        index
    }

    pub fn deregister_typ(&mut self, index: usize) {
        let typ = self.types[&index];
        self.type_indices.remove(&typ);
        self.types.remove(&index);
        self.fmt_fns.remove(&index);
    }

    pub fn fmt_value(&self, vref: NonNull<HeapValue>, f: &mut Formatter) -> Result<(), fmt::Error> {
        unsafe {
            let vref = vref.as_ref();
            self.fmt_fns[&self.type_indices[&vref.typ]](vref, f, self)
        }
    }

    fn symbol_table(&mut self) -> &mut SymbolTable { &mut self.symbols }
}

// ================================================================================================

/// Like `std::fmt::Debug`, but needs an `Allocator` because of the dynamic typing.
pub trait DynamicDebug: Sized {
    /// Formats the value using the given formatter.
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error>;

    /// Wrap `self` and `types` into a `DynDebugWrapper`.
    fn fmt_wrap<'a, 'b>(&'a self, types: &'b Allocator) -> DynDebugWrapper<'a, 'b, Self> {
        DynDebugWrapper {
            value: self,
            types: types
        }
    }
}

/// Wraps a `DynamicDebug` and an `Allocator` into a struct that implements `fmt::Debug`.
pub struct DynDebugWrapper<'a, 'b, T: 'a + DynamicDebug> {
    value: &'a T,
    types: &'b Allocator
}

impl<'a, 'b, T: DynamicDebug> Debug for DynDebugWrapper<'a, 'b, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(f, self.types)
    }
}

impl<'a, T> DynamicDebug for &'a [T] where T: DynamicDebug {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_list()
         .entries(self.iter().map(|entry| entry.fmt_wrap(types)))
         .finish()
    }
}
