use std::mem::size_of;
use std::fmt::{self, Debug, Formatter};

use gce::util::CeilDiv;
use gce::Object;
use gce::layout::{Granule, GSize};
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
    fn gsize(&self) -> GSize {
        From::from(self.typ.size.ceil_div(size_of::<Granule>()))
    }
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
    size: usize,
    ref_len: usize
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
