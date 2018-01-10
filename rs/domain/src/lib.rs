#![feature(unique, shared)]

extern crate pcws_gc;

pub mod object_model;
pub mod values;

use std::str;
use std::fmt::{self, Debug, Formatter};

use pcws_gc::{GSize, Initializable};
use object_model::{HeapValueSub,
                   HeapValue, DynHeapValue, ValueRef, ValueRefT};
use values::{Type, Symbol};

// ================================================================================================

/// Value allocator.
pub trait Allocator {
    unsafe fn allocate<T>(&mut self, galign: GSize, gsize: GSize) -> Option<Initializable<T>>;

    fn allocate_t<T>(&mut self) -> Option<Initializable<T>> {
        unsafe { self.allocate(GSize::from(1), GSize::of::<T>()) }
    }

    fn create_uniform<T, F>(&mut self, f: F) -> Option<ValueRefT<T>>
        where T: HeapValueSub, F: Fn(HeapValue) -> T;

    fn create_with_iter<T, F, I, E>(&mut self, f: F, len: usize, iter: I)
        -> Option<ValueRefT<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, I: IntoIterator<Item=E>, E: Copy;
}

/// Type reification witness.
pub trait Reifier<T> {
    /// Reify the type `T`.
    fn reify(&self) -> ValueRefT<Type>;
}

/// Value view witness.
pub trait Viewer {
    /// The view type to produce.
    type View: DynamicDebug;

    /// Create the view for `vref`.
    fn view(&self, vref: ValueRef) -> Self::View;
}

/// Symbol table for interning symbols
pub trait SymbolTable {
    fn get_symbol(&self, chars: &str) -> Option<ValueRefT<Symbol>>;

    fn insert_symbol(&mut self, symbol: ValueRefT<Symbol>);
}

// ================================================================================================

/// Like `std::fmt::Debug`, but needs a `Viewer` because of the dynamic typing.
pub trait DynamicDebug: Sized {
    /// Formats the value using the given formatter.
    fn fmt<R: Viewer>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error>;

    /// Wrap `self` and `types` into a `DynDebugWrapper`.
    fn fmt_wrap<'a, 'b, R: Viewer>(&'a self, types: &'b R) -> DynDebugWrapper<'a, 'b, Self, R> {
        DynDebugWrapper {
            value: self,
            types: types
        }
    }
}

/// Wraps a `DynamicDebug` and a `TypeRegistry` into a struct that implements `fmt::Debug`.
pub struct DynDebugWrapper<'a, 'b, T: 'a + DynamicDebug, R: 'b + Viewer> {
    value: &'a T,
    types: &'b R
}

impl<'a, 'b, T: DynamicDebug, R: Viewer> Debug for DynDebugWrapper<'a, 'b, T, R> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(f, self.types)
    }
}

impl<'a, T> DynamicDebug for &'a [T] where T: DynamicDebug {
    fn fmt<R: Viewer>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_list()
         .entries(self.iter().map(|entry| entry.fmt_wrap(types)))
         .finish()
    }
}
