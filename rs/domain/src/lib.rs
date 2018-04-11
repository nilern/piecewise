#![feature(nonzero, unique, ptr_internals, const_atomic_isize_new)]

extern crate core;
#[macro_use]
extern crate lazy_static;

extern crate pcws_gc;

pub mod object_model;
pub mod values;

use core::nonzero::NonZero;
use std::mem::{self, size_of, transmute};
use std::ptr::Unique;
use std::slice;
use std::collections::HashMap;
use std::any::TypeId;
use std::sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::fmt::{self, Debug, Display, Formatter};

use pcws_gc::{GSize, Initializable, start_init, Generation};
use object_model::{HeapValueSub, HeapValue, DynHeapValue, ValueRef, ValueRefT};
use values::Type;

pub use values::SymbolTable;

// ================================================================================================

pub type UnsafeFmtFn = unsafe fn(&HeapValue, &mut Formatter) -> Result<(), fmt::Error>;

pub unsafe fn debug_fn<T>(value: &HeapValue, f: &mut Formatter) -> Result<(), fmt::Error>
    where T: HeapValueSub + Debug
{
    <T as Debug>::fmt(transmute::<_, &T>(value), f)
}

pub unsafe fn display_fn<T>(value: &HeapValue, f: &mut Formatter) -> Result<(), fmt::Error>
    where T: HeapValueSub + Display
{
    <T as Display>::fmt(transmute::<_, &T>(value), f)
}

// ================================================================================================

/// Value allocator.
pub struct Allocator {
    gc: Generation<ValueRef>,
    symbols: SymbolTable
}

unsafe impl Sync for Allocator {}
unsafe impl Send for Allocator {}

lazy_static! {
    static ref ALLOCATOR: Mutex<Allocator> = {
        Mutex::new(Allocator::new(4*1024*1024)) // FIXME: Heap size should be set by client
    };
}

impl Allocator {
    pub fn instance() -> MutexGuard<'static, Allocator> {
        ALLOCATOR.lock().unwrap()
    }

    fn new(max_heap: usize) -> Allocator {
        Allocator {
            gc: Generation::new(max_heap),
            symbols: SymbolTable::new()
        }
    }

    pub fn mark_ref(&mut self, vref: Option<ValueRef>) -> Option<ValueRef> {
        self.gc.mark_ref(vref)
    }

    pub unsafe fn collect_garbage(&mut self) {
        TypeRegistry::instance_mut().trace(self);
        self.symbols.mark_roots(&mut self.gc);
        self.gc.collect()
    }

    fn init<T, F>(&mut self, ptr: Initializable<T>, f: F) -> ValueRefT<T>
        where T: HeapValueSub + 'static, F: FnOnce(Unique<T>, HeapValue)
    {
        let uptr = start_init(ptr);
        let tvref = ValueRefT::from(uptr);
        f(uptr, HeapValue {
            link: Some(tvref.into()),
            typ: TypeRegistry::instance().reify::<T>()
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

    fn symbol_table(&mut self) -> &mut SymbolTable { &mut self.symbols }
}

// ================================================================================================

pub struct TypeRegistry {
    types: HashMap<TypeId, ValueRefT<Type>>,
    type_indices: HashMap<ValueRefT<Type>, TypeId>,
    debug_fns: HashMap<TypeId, UnsafeFmtFn>,
    display_fns: HashMap<TypeId, UnsafeFmtFn>
}

lazy_static! {
    static ref TYPES: RwLock<TypeRegistry> = RwLock::new(TypeRegistry::new());
}

impl TypeRegistry {
    pub fn instance() -> RwLockReadGuard<'static, TypeRegistry> {
        TYPES.read().unwrap()
    }

    pub fn instance_mut() -> RwLockWriteGuard<'static, TypeRegistry> {
        TYPES.write().unwrap()
    }

    fn new() -> TypeRegistry {
        let allocator = &mut *Allocator::instance();

        let mut types = TypeRegistry {
            types: HashMap::new(),
            type_indices: HashMap::new(),
            debug_fns: HashMap::new(),
            display_fns: HashMap::new()
        };

        {
            let mut type_type: Unique<Type> = start_init(allocator.allocate_t().unwrap());
            let type_type_vref = ValueRefT::from(type_type);
            types.register_typ::<Type>(type_type_vref);
            unsafe {
                *type_type.as_mut() = Type::make(
                    HeapValue {
                        link: Some(type_type_vref.into()),
                        typ: type_type_vref
                    },
                    Type::SIZING, GSize::of::<Type>(), Type::MIN_REF_LEN
                );
            }
        }

        types
    }

    fn register_typ<T: HeapValueSub + Debug + Display + 'static>(&mut self, typ: ValueRefT<Type>) {
        let index = TypeId::of::<T>();
        self.types.insert(index, typ);
        self.type_indices.insert(typ, index);
        self.debug_fns.insert(index, debug_fn::<T>);
        self.display_fns.insert(index, display_fn::<T>);
    }

    /// Get the dynamic type corresponding to the (previously registered) static type `T`.
    pub fn reify<T: HeapValueSub + 'static>(&self) -> ValueRefT<Type> {
        self.types[&TypeId::of::<T>()]
    }

    pub fn debug(&self, value: &HeapValue, f: &mut Formatter) -> Result<(), fmt::Error> {
        unsafe { self.debug_fns[&self.type_indices[&value.typ]](value, f) }
    }

    pub fn display(&self, value: &HeapValue, f: &mut Formatter) -> Result<(), fmt::Error> {
        unsafe { self.display_fns[&self.type_indices[&value.typ]](value, f) }
    }

    fn trace(&mut self, heap: &mut Allocator) {
        unsafe {
            for v in self.types.values_mut() {
                *v = transmute(heap.gc.mark_ref((*v).into()));
            }

            for (k, v) in mem::replace(&mut self.type_indices, HashMap::new()) {
                self.type_indices.insert(transmute(heap.gc.mark_ref(k.into())), v);
            }
        }
    }
}

/// Register the static type `T`, creating the dynamic counterpart.
pub fn register_static_t<T: HeapValueSub + Debug + Display + 'static>() {
    lazy_static::initialize(&TYPES); // HACK
    let typ = Type::from_static::<T>(&mut*Allocator::instance()).unwrap();
    TypeRegistry::instance_mut().register_typ::<T>(typ);
}

// ================================================================================================

#[macro_export]
macro_rules! typecase_loop {
    ( $v:ident, { $w:ident : $T:ty => $body:expr, $($tail:tt)* } ) => {
        if let Some($w) = $v.try_downcast::<$T>() {
            $body
        } else {
            typecase_loop!($v, { $($tail)* })
        }
    };
    ( $v:ident, { mut $w:ident : $T:ty => $body:expr, $($tail:tt)* } ) => {
        if let Some(mut $w) = $v.try_downcast::<$T>() {
            $body
        } else {
            typecase_loop!($v, { $($tail)* })
        }
    };
    ( $v:ident, { $T:ty => $body:expr, $($tail:tt)* } ) => {
        if let Some(_) = $v.try_downcast::<$T>() {
            $body
        } else {
            typecase_loop!($v, { $($tail)* })
        }
    };
    ( $v:ident, { _ => $body:expr } ) => {{
        $body
    }}
}

#[macro_export]
macro_rules! typecase {
    ( $v:expr, { $($tail:tt)* } ) => {{
        let w = $v;
        typecase_loop!(w, { $($tail)* })
    }}
}
