use std::fmt::{self, Formatter};

use interpreter::Allocator;
use object_model::{HeapValueSub, DynHeapValueSub, DynamicDebug, Unbox,
                   HeapValue, DynHeapValue, Type,
                   ValueRef, ScalarValueRef, HeapValueRef};
use value::{TypeIndex, TypeRegistry};
use ast::{Block, Call};

/// Block continuation
#[repr(C)]
pub struct BlockCont {
    pub base: HeapValue,
    pub parent: ValueRef,
    pub lenv: ValueRef,
    pub denv: ValueRef,
    pub block: HeapValueRef<Block>,
    pub index: ScalarValueRef<isize>
}

impl BlockCont {
    pub fn new(allocator: &mut Allocator, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
               block: HeapValueRef<Block>, index: usize)
        -> Option<HeapValueRef<BlockCont>>
    {
        allocator.uniform_create(|base| BlockCont {
            base, parent, lenv, denv, block,
            index: (index as isize).into()
        })
    }


    pub fn index(&self) -> usize { self.index.unbox() as usize }
}

impl HeapValueSub for BlockCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::BlockCont;
    const UNIFORM_REF_LEN: usize = 5;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::uniform::<Self>(allocator)
    }
}

impl DynamicDebug for BlockCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("BlockCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("block", &self.block.fmt_wrap(types))
         .field("index", &self.index)
         .finish()
    }
}

/// Assigning continuation
#[repr(C)]
pub struct DefCont {
    pub base: HeapValue,
    pub parent: ValueRef,
    pub lenv: ValueRef,
    pub denv: ValueRef,
    pub var: ValueRef
}

impl DefCont {
    pub fn new(allocator: &mut Allocator, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
               var: ValueRef)
        -> Option<HeapValueRef<DefCont>>
    {
        allocator.uniform_create(|base| DefCont { base, parent, lenv, denv, var })
    }
}

impl HeapValueSub for DefCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::DefCont;
    const UNIFORM_REF_LEN: usize = 4;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::uniform::<Self>(allocator)
    }
}

impl DynamicDebug for DefCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("DefCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("var", &self.var.fmt_wrap(types))
         .finish()
    }
}

/// Continuation expecting callee value
#[repr(C)]
pub struct CalleeCont {
    pub base: HeapValue,
    pub parent: ValueRef,
    pub lenv: ValueRef,
    pub denv: ValueRef,
    pub call: HeapValueRef<Call>
}

impl CalleeCont {
    pub fn new(allocator: &mut Allocator, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
               call: HeapValueRef<Call>) -> Option<HeapValueRef<CalleeCont>>
    {
        allocator.uniform_create(|base| CalleeCont { base, parent, lenv, denv, call })
    }
}

impl HeapValueSub for CalleeCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::CalleeCont;
    const UNIFORM_REF_LEN: usize = 4;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::uniform::<Self>(allocator)
    }
}

impl DynamicDebug for CalleeCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("CalleeCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("call", &self.call.fmt_wrap(types))
         .finish()
    }
}

/// Continuation expecting argument value
#[repr(C)]
pub struct ArgCont {
    pub base: DynHeapValue,
    pub parent: ValueRef,
    pub lenv: ValueRef,
    pub denv: ValueRef,
    pub call: HeapValueRef<Call>,
    pub index: ScalarValueRef<isize>,
    pub callee: ValueRef
}

impl ArgCont {
    pub fn new(allocator: &mut Allocator, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
               call: HeapValueRef<Call>, index: usize, callee: ValueRef,  args: &[ValueRef])
        -> Option<HeapValueRef<ArgCont>>
    {
        allocator.create_with_vref_slice(|base| ArgCont {
            base, parent, lenv, denv, call, index: (index as isize).into(), callee
        }, args)
    }

    pub fn index(&self) -> usize { self.index.unbox() as usize }

    pub fn args(&self) -> &[ValueRef] { self.tail() }
}

impl HeapValueSub for ArgCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::ArgCont;
    const UNIFORM_REF_LEN: usize = 6;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::dyn_refs::<Self>(allocator)
    }
}

impl DynHeapValueSub for ArgCont {
    type TailItem = ValueRef;
}

impl DynamicDebug for ArgCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("ArgCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("call", &self.call.fmt_wrap(types))
         .field("index", &self.index)
         .field("callee", &self.callee.fmt_wrap(types))
         .field("args", &self.args().fmt_wrap(types))
         .finish()
    }
}

/// Halt continuation
#[repr(C)]
pub struct Halt {
    pub base: HeapValue
}

impl Halt {
    pub fn new(allocator: &mut Allocator) -> Option<HeapValueRef<Halt>> {
        allocator.uniform_create(|base| Halt { base })
    }
}

impl HeapValueSub for Halt {
    const TYPE_INDEX: TypeIndex = TypeIndex::Halt;
    const UNIFORM_REF_LEN: usize = 0;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::uniform::<Self>(allocator)
    }
}

impl DynamicDebug for Halt {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Halt")
         .field("base", &self.base.fmt_wrap(types))
         .finish()
    }
}
