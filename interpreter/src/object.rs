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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum TypeIndex {
    Type
}

pub trait TypeRegistry {
    fn index_of(&self, typ: TypedValueRef<Type>) -> TypeIndex;
}

// ================================================================================================

#[derive(Debug)]
pub enum ValueView {
    Type(TypedValueRef<Type>),

    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
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

impl Debug for HeapValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        // TODO: fields (may point back to self!)
        f.debug_struct("HeapValue").finish()
    }
}

// ================================================================================================

#[derive(Debug)]
#[repr(C)]
pub struct Type {
    heap_value: HeapValue,
    gsize: GSize,
    ref_len: usize
}

// ================================================================================================

pub struct ValueManager {
    gc: Generation<ValueRef>,
    type_type: TypedValueRef<Type>
}

impl ValueManager {
    pub fn new(max_heap: usize) -> ValueManager {
        let mut gc = Generation::new(max_heap);
        let type_type = unsafe {
            gc.allocate(NonZero::new_unchecked(1),
                        NonZero::new_unchecked(From::from(GSize::of::<Type>())))
              .unwrap()
        };
        let res = ValueManager {
            gc: gc,
            type_type: TypedValueRef::new(unsafe { transmute(type_type) })
        };
        res.init_type(type_type, GSize::of::<Type>(), 1);
        res
    }

    fn init_type(&self, typ: Initializable<Type>, gsize: GSize, ref_len: usize)
        -> TypedValueRef<Type>
    {
        let mut typ: Unique<Type> = unsafe { transmute(typ) };
        let tvref = TypedValueRef::new(Shared::from(typ));
        *unsafe { typ.as_mut() } = Type {
            heap_value: HeapValue {
                link: tvref.upcast(),
                typ: self.type_type
            },
            gsize: gsize,
            ref_len: ref_len
        };
        tvref
    }

    pub fn create_type(&mut self, size: usize, ref_len: usize) -> Option<TypedValueRef<Type>> {
        unsafe {
            self.gc.allocate(NonZero::new_unchecked(1),
                             NonZero::new_unchecked(From::from(GSize::of::<Type>())))
        }.map(|typ: Initializable<Type>|
            self.init_type(typ, GSize::from(size.ceil_div(size_of::<Granule>())), ref_len)
        )
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
