#![feature(nonzero, unique, ptr_internals, const_atomic_isize_new)]

extern crate core;

extern crate pcws_gc;

pub mod object_model;
pub mod values;

use core::nonzero::NonZero;
use std::mem::{size_of, transmute};
use std::ptr::{Unique, NonNull};
use std::slice;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::collections::HashMap;
use std::any::TypeId;

use pcws_gc::{GSize, Initializable, start_init, Generation};
use object_model::{Layout, HeapValueSub, HeapValue, DynHeapValue, ValueRef, ValueRefT};
use values::Type;

pub use values::SymbolTable;

// ================================================================================================

type UnsafeFmtFn = unsafe fn(&HeapValue, &mut Formatter, &mut Allocator) -> Result<(), fmt::Error>;

/// Value allocator.
pub struct Allocator {
    gc: Generation<ValueRef>,

    types: HashMap<TypeId, ValueRefT<Type>>,
    debug_fns: HashMap<TypeId, UnsafeFmtFn>,
    display_fns: HashMap<TypeId, UnsafeFmtFn>,
    type_indices: HashMap<ValueRefT<Type>, TypeId>,

    symbols: SymbolTable
}

impl Allocator {
    pub fn new(max_heap: usize, basis: HashMap<TypeId, (Layout, UnsafeFmtFn, UnsafeFmtFn)>)
        -> Allocator
    {
        let mut allocator = Allocator {
            gc: Generation::new(max_heap),

            types: HashMap::new(),
            debug_fns: HashMap::new(),
            display_fns: HashMap::new(),
            type_indices: HashMap::new(),

            symbols: SymbolTable::new()
        };

        let type_type: Initializable<Type> = allocator.allocate_t().unwrap();
        for (index, (layout, debug, display)) in basis {
            let typ = if index == TypeId::of::<Type>() {
                allocator.init_uniform(type_type, |base| Type::make(base, layout));
                ValueRefT::<Type>::from(start_init(type_type)).into()
            } else {
                allocator.create_uniform(|base| Type::make(base, layout)).unwrap().into()
            };
            allocator.register_typ(index, typ, debug, display);
        }

        allocator
    }

    fn init<T, F>(&mut self, ptr: Initializable<T>, f: F) -> ValueRefT<T>
        where T: HeapValueSub + 'static, F: FnOnce(Unique<T>, HeapValue)
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
        where T: HeapValueSub + 'static, F: FnOnce(HeapValue) -> T
    {
        self.init(ptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(heap_value);
        })
    }

    fn init_with_iter<T, F, I, E>(&mut self, iptr: Initializable<T>, f: F, len: usize, iter: I)
        -> ValueRefT<T>
        where T: HeapValueSub + 'static, F: Fn(DynHeapValue) -> T,
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
        where T: HeapValueSub + 'static, F: Fn(HeapValue) -> T
    {
        self.allocate_t().map(|typ| self.init_uniform(typ, f))
    }

    pub fn create_with_iter<T, F, I, E>(&mut self, f: F, len: usize, iter: I)
        -> Option<ValueRefT<T>>
        where T: HeapValueSub + 'static, F: Fn(DynHeapValue) -> T, I: Iterator<Item=E>, E: Copy
    {
        let gsize = GSize::from_bytesize(size_of::<T>() + len*size_of::<E>());
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(gsize)) }
            .map(|iptr| self.init_with_iter(iptr, f, len, iter))
    }

    pub fn create_with_slice<T, F, E>(&mut self, f: F, slice: &[E]) -> Option<ValueRefT<T>>
        where T: HeapValueSub + 'static, F: Fn(DynHeapValue) -> T, E: Copy
    {
        self.create_with_iter(f, slice.len(), slice.iter().cloned())
    }

    pub fn reify<T: HeapValueSub + 'static>(&mut self) -> ValueRefT<Type> {
        self.types[&TypeId::of::<T>()]
    }

    fn register_typ(&mut self, index: TypeId, typ: ValueRefT<Type>, f: UnsafeFmtFn, g: UnsafeFmtFn)
    {
        self.types.insert(index, typ);
        self.debug_fns.insert(index, f);
        self.display_fns.insert(index, g);
        self.type_indices.insert(typ, index);
    }

    pub fn debug_fmt_value(&mut self, vref: NonNull<HeapValue>, f: &mut Formatter)
        -> Result<(), fmt::Error>
    {
        unsafe {
            let vref = vref.as_ref();
            self.debug_fns[&self.type_indices[&vref.typ]](vref, f, self)
        }
    }

    pub fn display_fmt_value(&mut self, vref: NonNull<HeapValue>, f: &mut Formatter)
        -> Result<(), fmt::Error>
    {
        unsafe {
            let vref = vref.as_ref();
            self.display_fns[&self.type_indices[&vref.typ]](vref, f, self)
        }
    }

    fn symbol_table(&mut self) -> &mut SymbolTable { &mut self.symbols }
}

// ================================================================================================

#[macro_export]
macro_rules! typecase_loop {
    ( $v:ident, $types:ident, { $w:ident : $T:ty => $body:expr, $($tail:tt)* } ) => {
        if let Some($w) = $v.try_downcast::<$T>($types) {
            $body
        } else {
            typecase_loop!($v, $types, { $($tail)* })
        }
    };
    ( $v:ident, $types:ident, { mut $w:ident : $T:ty => $body:expr, $($tail:tt)* } ) => {
        if let Some(mut $w) = $v.try_downcast::<$T>($types) {
            $body
        } else {
            typecase_loop!($v, $types, { $($tail)* })
        }
    };
    ( $v:ident, $types:ident, { $T:ty => $body:expr, $($tail:tt)* } ) => {
        if let Some(_) = $v.try_downcast::<$T>($types) {
            $body
        } else {
            typecase_loop!($v, $types, { $($tail)* })
        }
    };
    ( $v:ident, $types:ident, { _ => $body:expr } ) => {{
        $body
    }}
}

#[macro_export]
macro_rules! typecase {
    ( $v:expr, $types:ident, { $($tail:tt)* } ) => {{
        let w = $v;
        typecase_loop!(w, $types, { $($tail)* })
    }}
}

// ================================================================================================

/// Like `std::fmt::Debug`, but needs an `Allocator` because of the dynamic typing.
pub trait DynamicDebug: Sized {
    /// Formats the value using the given formatter.
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error>;

    /// Wrap `self` and `types` into a `DynDebugWrapper`.
    fn debug_wrap<'a, 'b>(&'a self, types: &'b mut Allocator) -> DynDebugWrapper<'a, 'b, Self> {
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

// HACK:
#[allow(mutable_transmutes)]
impl<'a, 'b, T: DynamicDebug> Debug for DynDebugWrapper<'a, 'b, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(f, unsafe { transmute::<_, &mut Allocator>(self.types) })
    }
}

impl<'a, T> DynamicDebug for &'a [T] where T: DynamicDebug {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.write_char('[')?;
        let mut vals = self.iter();
        if let Some(v) = vals.next() { v.fmt(f, types)?; }
        for v in vals { write!(f, ", {:?}", v.debug_wrap(types))?; }
        f.write_char(']')
    }
}

// ================================================================================================

/// Like `std::fmt::Display`, but needs an `Allocator` because of the dynamic typing.
pub trait DynamicDisplay: Sized {
    /// Formats the value using the given formatter.
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error>;

    /// Wrap `self` and `types` into a `DynDisplayWrapper`.
    fn display_wrap<'a, 'b>(&'a self, types: &'b mut Allocator) -> DynDisplayWrapper<'a, 'b, Self>
    {
        DynDisplayWrapper {
            value: self,
            types: types
        }
    }
}

/// Wraps a `DynamicDisplay` and an `Allocator` into a struct that implements `fmt::Display`.
pub struct DynDisplayWrapper<'a, 'b, T: 'a + DynamicDisplay> {
    value: &'a T,
    types: &'b Allocator
}

// HACK:
#[allow(mutable_transmutes)]
impl<'a, 'b, T: DynamicDisplay> Display for DynDisplayWrapper<'a, 'b, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(f, unsafe { transmute::<_, &mut Allocator>(self.types) })
    }
}
