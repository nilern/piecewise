use core::nonzero::NonZero;
use std::ptr::{Unique, Shared};
use std::mem::{size_of, transmute};
use std::fmt::{self, Debug, Formatter};

use gce::util::{Initializable, CeilDiv};
use gce::Object;
use gce::layout::{Granule, GSize};
use gce::mark_n_sweep::Generation;
use value_refs::{ValueRef, TypedValueRef};

// ================================================================================================

pub trait DynamicDebug {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error>;
}

// ================================================================================================

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum TypeIndex {
    Type,

    Const
}

pub trait TypeRegistry {
    fn insert(&mut self, index: TypeIndex, typ: TypedValueRef<Type>);

    fn get(&self, index: TypeIndex) -> TypedValueRef<Type>;
    fn index_of(&self, typ: TypedValueRef<Type>) -> TypeIndex;
}

// ================================================================================================

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

#[repr(C)]
pub struct HeapValue {
    link: ValueRef,
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

#[repr(C)]
pub struct Type {
    heap_value: HeapValue,
    gsize: GSize,
    ref_len: usize
}

impl DynamicDebug for Type {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Type {{ heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        write!(f, ", gsize: {:?}", self.gsize)?;
        write!(f, ", ref_len: {:?} }}", self.ref_len)
    }
}

#[repr(C)]
pub struct Const {
    heap_value: HeapValue,
    value: ValueRef
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

pub struct ValueManager {
    gc: Generation<ValueRef>
}

impl ValueManager {
    pub fn new<T: TypeRegistry>(type_reg: &mut T, max_heap: usize) -> ValueManager {
        let mut res = ValueManager {
            gc: Generation::new(max_heap)
        };
        let type_type = unsafe {
            res.gc.allocate(NonZero::new_unchecked(1),
                            NonZero::new_unchecked(From::from(GSize::of::<Type>())))
                  .unwrap()
        };
        type_reg.insert(TypeIndex::Type, TypedValueRef::new(unsafe { transmute(type_type) }));
        Self::init_type::<T>(type_type, type_reg, GSize::of::<Type>(), 1);
        res
    }

    fn init_type<T: TypeRegistry>(typ: Initializable<Type>, type_reg: &TypeRegistry, gsize: GSize,
                                  ref_len: usize) -> TypedValueRef<Type>
    {
        let mut typ: Unique<Type> = unsafe { transmute(typ) };
        let tvref = TypedValueRef::new(Shared::from(typ));
        *unsafe { typ.as_mut() } = Type {
            heap_value: HeapValue {
                link: tvref.upcast(),
                typ: type_reg.get(TypeIndex::Type)
            },
            gsize: gsize,
            ref_len: ref_len
        };
        tvref
    }

    pub fn create_type<T: TypeRegistry>(&mut self, type_reg: &T, size: usize, ref_len: usize)
        -> Option<TypedValueRef<Type>>
    {
        unsafe {
            self.gc.allocate(NonZero::new_unchecked(1),
                             NonZero::new_unchecked(From::from(GSize::of::<Type>())))
        }.map(|typ: Initializable<Type>|
            Self::init_type::<T>(typ, type_reg, GSize::from(size.ceil_div(size_of::<Granule>())),
                                 ref_len)
        )
    }

    pub fn create_const(&mut self, types: &TypeRegistry, value: ValueRef)
        -> Option<TypedValueRef<Const>>
    {
        unsafe {
            self.gc.allocate(NonZero::new_unchecked(1),
                             NonZero::new_unchecked(From::from(GSize::of::<Const>())))
        }.map(|cv: Initializable<Const>| {
            let mut cv: Unique<Const> = unsafe { transmute(cv) };
            let tvref = TypedValueRef::new(Shared::from(cv));
            *unsafe { cv.as_mut() } = Const {
                heap_value: HeapValue {
                    link: tvref.upcast(),
                    typ: types.get(TypeIndex::Const)
                },
                value: value
            };
            tvref
        })
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
