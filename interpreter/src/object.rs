use core::nonzero::NonZero;
use std::mem::{size_of, transmute};
use std::fmt::{self, Debug, Formatter};
use std::slice;
use std::collections::HashMap;

use gce::util::{start_init, Initializable, CeilDiv};
use gce::Object;
use gce::layout::{Granule, GSize};
use gce::mark_n_sweep::Generation;
use value_refs::{ValueRef, TypedValueRef};

// ================================================================================================

/// Like `std::fmt::Debug`, but needs a `TypeRegistry` because of the dynamic typing.
pub trait DynamicDebug {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error>;
}

// ================================================================================================

/// A reified type tag.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum TypeIndex {
    Type,
    Const,
    Symbol,
    Call
}

/// Obtain the `TypeIndex` of `Self`.
trait IndexedType {
    const TYPE_INDEX: TypeIndex;
}

/// Converts between type values and `TypeIndex`:es.
pub trait TypeRegistry {
    /// Add a mapping.
    fn insert(&mut self, index: TypeIndex, typ: TypedValueRef<Type>);

    /// Get the `Type` for `index`.
    fn get(&self, index: TypeIndex) -> TypedValueRef<Type>;

    /// Get the `TypeIndex` for `typ`.
    fn index_of(&self, typ: TypedValueRef<Type>) -> TypeIndex;
}

// ================================================================================================

/// Unwraps scalars and makes heap value typing static.
pub enum ValueView {
    Type(TypedValueRef<Type>),
    Const(TypedValueRef<Const>),
    Symbol(TypedValueRef<Symbol>),
    Call(TypedValueRef<Call>),

    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
}

impl DynamicDebug for ValueView {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error>
    {
        use self::ValueView::*;

        match self {
            &Type(vref) => vref.fmt(f, type_reg),
            &Const(vref) => vref.fmt(f, type_reg),
            &Symbol(vref) => vref.fmt(f, type_reg),
            &Call(vref) => vref.fmt(f, type_reg),

            &Int(v) => v.fmt(f),
            &Float(v) => v.fmt(f),
            &Char(v) => v.fmt(f),
            &Bool(v) => v.fmt(f)
        }
    }
}

// ================================================================================================

/// The 'supertype' of all heap values (non-scalars).
#[repr(C)]
pub struct HeapValue {
    /// Multipurpose redirection field
    link: ValueRef,
    /// Dynamic type
    typ: TypedValueRef<Type>
}

#[repr(C)]
pub struct DynHeapValue {
    base: HeapValue,
    dyn_len: usize
}

impl HeapValue {
    pub fn typ(&self) -> &TypedValueRef<Type> { &self.typ }

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

    pub fn ref_fields(&self) -> ObjRefs {
        let ptr = self.refs_ptr();
        let end = unsafe { ptr.offset(self.ref_len() as isize) };
        ObjRefs { ptr, end }
    }
}

impl Object for HeapValue {
    fn gsize(&self) -> GSize {
        let gsize = self.typ.uniform_gsize();
        if self.typ.has_dyn_ref_len() {
            return GSize::from(gsize + 1 + unsafe { transmute::<_, &DynHeapValue>(self) }.dyn_len);
        }
        if self.typ.has_dyn_gsize() {
            return GSize::from(gsize + 1
                               + unsafe { transmute::<_, &DynHeapValue>(self)
                                              .dyn_len.ceil_div(size_of::<Granule>()) });
        }
        GSize::from(gsize)
    }
}

impl DynamicDebug for HeapValue {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, _: &T) -> Result<(), fmt::Error> {
        // TODO: fields (may point back to self!)
        f.debug_struct("HeapValue").finish()
    }
}

impl DynamicDebug for DynHeapValue {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Dyn {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        write!(f, ", dyn_len: {:?} }}", self.dyn_len)
    }
}

pub struct ObjRefs {
    ptr: *mut ValueRef,
    end: *mut ValueRef
}

impl Iterator for ObjRefs {
    type Item = *mut ValueRef;

    fn next(&mut self) -> Option<*mut ValueRef> {
        if self.ptr < self.end {
            let res = Some(self.ptr);
            self.ptr = unsafe { self.ptr.offset(1) };
            res
        } else {
            None
        }
    }
}

// ================================================================================================

/// A dynamic type.
#[repr(C)]
pub struct Type {
    heap_value: HeapValue,
    gsize_with_dyn: usize,
    ref_len_with_dyn: usize
}

impl Type {
    fn new(heap_value: HeapValue, has_dyn_gsize: bool, gsize: GSize, has_dyn_ref_len: bool,
           ref_len: usize) -> Type {
        Type {
            heap_value,
            gsize_with_dyn: usize::from(gsize) << 1 | if has_dyn_gsize { 1 } else { 0 },
            ref_len_with_dyn: ref_len << 1 | if has_dyn_ref_len { 1 } else { 0 }
        }
    }

    fn uniform_gsize(&self) -> usize { self.gsize_with_dyn >> 1 }

    fn has_dyn_gsize(&self) -> bool { self.gsize_with_dyn & 0b1 == 1 }

    fn uniform_ref_len(&self) -> usize { self.ref_len_with_dyn >> 1 }

    fn has_dyn_ref_len(&self) -> bool { self.ref_len_with_dyn & 0b1 == 1 }
}

impl IndexedType for Type {
    const TYPE_INDEX: TypeIndex = TypeIndex::Type;
}

impl DynamicDebug for Type {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Type {{ heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        write!(f, ", gsize: {:?}", self.gsize_with_dyn)?;
        write!(f, ", ref_len: {:?} }}", self.ref_len_with_dyn)
    }
}

/// An AST node for constants.
#[repr(C)]
pub struct Const {
    heap_value: HeapValue,
    /// The value of the constant
    value: ValueRef
}

impl IndexedType for Const {
    const TYPE_INDEX: TypeIndex = TypeIndex::Const;
}

impl DynamicDebug for Const {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Const {{ heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        f.write_str(", value: ")?;
        self.value.fmt(f, type_reg)?;
        f.write_str(" }}")
    }
}

/// Symbol (hash-consed string)
pub struct Symbol {
    base: DynHeapValue
}

impl IndexedType for Symbol {
    const TYPE_INDEX: TypeIndex = TypeIndex::Symbol;
}

impl DynamicDebug for Symbol {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Symbol {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        // TODO: chars
        f.write_str(" }}")
    }
}

/// Call AST node
pub struct Call {
    base: DynHeapValue,
    callee: ValueRef,
}

impl IndexedType for Call {
    const TYPE_INDEX: TypeIndex = TypeIndex::Call;
}

impl DynamicDebug for Call {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Call {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", callee: ")?;
        self.callee.fmt(f, type_reg)?;
        // TODO: args
        f.write_str(" }}")
    }
}

// ================================================================================================

/// Memory manager and value factory.
pub struct ValueManager {
    gc: Generation<ValueRef>,
    // OPTIMIZE: make the key just point inside the value
    symbol_table: HashMap<String, TypedValueRef<Symbol>>
}

impl ValueManager {
    /// Create a new `ValueManager` with a maximum heap size of `max_heap`.
    pub fn new<R: TypeRegistry>(type_reg: &mut R, max_heap: usize) -> ValueManager {
        let mut res = ValueManager {
            gc: Generation::new(max_heap),
            symbol_table: HashMap::new()
        };
        let type_type: Initializable<Type> = unsafe { res.allocate_t() }.unwrap();
        type_reg.insert(TypeIndex::Type, TypedValueRef::new(unsafe { transmute(type_type) }));
        Self::init(type_type, type_reg,
                   |heap_value| Type::new(heap_value, false, GSize::of::<Type>(),
                                          false, 0));
        res
    }

    /// Initialize a `T`, delegating to `f` for everything but the `HeapValue` part.
    fn init<T, R, F>(ptr: Initializable<T>, type_reg: &R, f: F) -> TypedValueRef<T>
        where T: IndexedType, R: TypeRegistry, F: Fn(HeapValue) -> T
    {
        let mut uptr = start_init(ptr);
        let tvref = TypedValueRef::new(uptr);
        *unsafe { uptr.as_mut() } = f(HeapValue {
            link: tvref.upcast(),
            typ: type_reg.get(T::TYPE_INDEX)
        });
        tvref
    }

    /// Allocate a `T` with a granule alignment of 1.
    unsafe fn allocate_t<T>(&mut self) -> Option<Initializable<T>> {
        self.gc.allocate(NonZero::new_unchecked(1),
                         NonZero::new_unchecked(From::from(GSize::of::<T>())))
    }

    /// Allocate and Initialize a `T` with a granule alignment of 1, delegating to `f` for
    /// everything but the `HeapValue` part.
    fn create<T, R, F>(&mut self, type_reg: &R, f: F) -> Option<TypedValueRef<T>>
        where T: IndexedType, R: TypeRegistry, F: Fn(HeapValue) -> T
    {
        unsafe { self.allocate_t() }.map(|typ| Self::init(typ, type_reg, f))
    }

    /// Create a new dynamic type whose instances have a (byte) size of `size` and `ref_len`
    /// potentially pointer-valued fields.
    pub fn create_type<R: TypeRegistry>(&mut self, type_reg: &R,
                                        has_dyn_gsize: bool, gsize: GSize,
                                        has_dyn_ref_len: bool, ref_len: usize)
        -> Option<TypedValueRef<Type>>
    {
        self.create(type_reg, |heap_value| Type::new(heap_value, has_dyn_gsize, gsize,
                                                     has_dyn_ref_len, ref_len))
    }

    /// Create a new `Const` node of `value`.
    pub fn create_const<R: TypeRegistry>(&mut self, types: &R, value: ValueRef)
        -> Option<TypedValueRef<Const>>
    {
        self.create(types, |heap_value| Const { heap_value, value })
    }

    pub fn create_symbol<R: TypeRegistry>(&mut self, types: &R, chars: &str)
        -> Option<TypedValueRef<Symbol>>
    {
        fn init<R: TypeRegistry>(iptr: Initializable<Symbol>, types: &R, bytes: &[u8])
            -> TypedValueRef<Symbol>
        {
            let mut uptr = start_init(iptr);
            let tvref = TypedValueRef::new(uptr);
            *unsafe { uptr.as_mut() } = Symbol {
                base: DynHeapValue {
                    base: HeapValue {
                        link: tvref.upcast(),
                        typ: types.get(Symbol::TYPE_INDEX)
                    },
                    dyn_len: bytes.len()
                }
            };
            let dest_bytes: &mut[u8] = unsafe {
                slice::from_raw_parts_mut(uptr.as_ptr().offset(1) as _, bytes.len())
            };
            dest_bytes.copy_from_slice(bytes);
            tvref
        }

        fn create<R: TypeRegistry>(mgr: &mut ValueManager, types: &R, chars: &str)
            -> Option<TypedValueRef<Symbol>>
        {
            let bytes = chars.as_bytes();
            let gsize = usize::from(GSize::of::<Symbol>())
                      + bytes.len().ceil_div(size_of::<Granule>());
            unsafe { mgr.gc.allocate(NonZero::new_unchecked(1), NonZero::new_unchecked(gsize)) }
                .map(|iptr| init(iptr, types, bytes))
        }

        self.symbol_table.get(chars).map(|&sym| sym)
                         .or_else(|| {
                             let sym = create(self, types, chars);
                             if let Some(sym) = sym {
                                 self.symbol_table.insert(chars.to_string(), sym);
                             }
                             sym
                         })
    }

    fn create_call<R: TypeRegistry>(&mut self, types: &R, callee: ValueRef, args: &[ValueRef])
        -> Option<TypedValueRef<Call>>
    {
        fn init<R: TypeRegistry>(iptr: Initializable<Call>, types: &R, callee: ValueRef,
                                 args: &[ValueRef]) -> TypedValueRef<Call>
        {
            let mut uptr = start_init(iptr);
            let tvref = TypedValueRef::new(uptr);
            *unsafe { uptr.as_mut() } = Call {
                base: DynHeapValue {
                    base: HeapValue {
                        link: tvref.upcast(),
                        typ: types.get(Call::TYPE_INDEX)
                    },
                    dyn_len: args.len()
                },
                callee: callee
            };
            let dest_args: &mut[ValueRef] = unsafe {
                slice::from_raw_parts_mut(uptr.as_ptr().offset(1) as _, args.len())
            };
            dest_args.copy_from_slice(args);
            tvref
        }

        unsafe { self.gc.allocate(NonZero::new_unchecked(1),
                                  NonZero::new_unchecked(usize::from(GSize::of::<Call>())
                                                         + args.len())) }
            .map(|iptr| init(iptr, types, callee, args))
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use gce::layout::GSize;
    use super::{HeapValue, DynHeapValue, Type};

    #[test]
    fn heap_value_size() {
        assert_eq!(GSize::of::<HeapValue>(), GSize::from(2));
        assert_eq!(GSize::of::<DynHeapValue>(), GSize::from(3));
    }

    #[test]
    fn type_size() {
        assert_eq!(GSize::of::<Type>(), GSize::from(4));
    }
}
