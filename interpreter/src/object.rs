use core::nonzero::NonZero;
use std::mem::{size_of, transmute};
use std::fmt::{self, Debug, Formatter};

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

    Const
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

impl HeapValue {
    pub fn ref_len(&self) -> usize { self.typ.ref_len }
}

impl Object for HeapValue {
    fn gsize(&self) -> GSize { self.typ.gsize }
}

impl DynamicDebug for HeapValue {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, _: &T) -> Result<(), fmt::Error> {
        // TODO: fields (may point back to self!)
        f.debug_struct("HeapValue").finish()
    }
}

// ================================================================================================

/// A dynamic type.
#[repr(C)]
pub struct Type {
    heap_value: HeapValue,
    /// Instance size in granules
    gsize: GSize,
    /// How many potentially pointer-valued fields instances have
    ref_len: usize
}

impl IndexedType for Type {
    const TYPE_INDEX: TypeIndex = TypeIndex::Type;
}

impl DynamicDebug for Type {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Type {{ heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        write!(f, ", gsize: {:?}", self.gsize)?;
        write!(f, ", ref_len: {:?} }}", self.ref_len)
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

// ================================================================================================

/// Memory manager and value factory.
pub struct ValueManager {
    gc: Generation<ValueRef>
}

impl ValueManager {
    /// Create a new `ValueManager` with a maximum heap size of `max_heap`.
    pub fn new<R: TypeRegistry>(type_reg: &mut R, max_heap: usize) -> ValueManager {
        let mut res = ValueManager {
            gc: Generation::new(max_heap)
        };
        let type_type: Initializable<Type> = unsafe { res.allocate_t() }.unwrap();
        type_reg.insert(TypeIndex::Type, TypedValueRef::new(unsafe { transmute(type_type) }));
        Self::init(type_type, type_reg, |heap_value| Type {
            heap_value,
            gsize: GSize::from(GSize::of::<Type>()),
            ref_len: 1
        });
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
    pub fn create_type<R: TypeRegistry>(&mut self, type_reg: &R, size: usize, ref_len: usize)
        -> Option<TypedValueRef<Type>>
    {
        self.create(type_reg, |heap_value| Type {
            heap_value,
            gsize: GSize::from(size.ceil_div(size_of::<Granule>())),
            ref_len
        })
    }

    /// Create a new `Const` node of `value`.
    pub fn create_const<R: TypeRegistry>(&mut self, types: &R, value: ValueRef)
        -> Option<TypedValueRef<Const>>
    {
        self.create(types, |heap_value| Const { heap_value, value })
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use gce::layout::GSize;
    use super::{HeapValue, Type};

    #[test]
    fn heap_value_size() {
        assert_eq!(GSize::of::<HeapValue>(), GSize::from(2));
    }

    #[test]
    fn type_size() {
        assert_eq!(GSize::of::<Type>(), GSize::from(4));
    }
}
