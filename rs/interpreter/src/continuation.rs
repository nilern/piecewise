use std::fmt::{self, Debug, Display, Formatter};

use pcws_domain::Allocator;
use pcws_domain::object_model::{ValueRef, ValueRefT};
use ast::{Block, Call};
use env::Env;

// ================================================================================================

heap_struct! {
    pub struct Halt: UniformHeapValue {}
}

impl Halt {
    pub fn new(allocator: &mut Allocator) -> Option<ValueRefT<Halt>> {
        allocator.create_uniform(|base| Halt { base })
    }
}

impl Debug for Halt {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Halt")
         .field("base", &self.base)
         .finish()
    }
}

impl Display for Halt {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "#<ContinuationFrame (Halt)>")
    }
}

// ================================================================================================

heap_struct! {
    pub struct BlockCont: UniformHeapValue {
        parent: ValueRef,
        lenv: Option<ValueRefT<Env>>,
        denv: Option<ValueRefT<Env>>,
        block: ValueRefT<Block>,
        index: usize
    }
}

impl BlockCont {
    pub fn new(heap: &mut Allocator, parent: ValueRef, lenv: Option<ValueRefT<Env>>,
               denv: Option<ValueRefT<Env>>, block: ValueRefT<Block>, index: usize)
        -> Option<ValueRefT<BlockCont>>
    {
        heap.create_uniform(|base| BlockCont { base, parent, lenv, denv, block, index })
    }

    pub fn parent(&self) -> ValueRef { self.parent }
    pub fn lenv(&self) -> Option<ValueRefT<Env>> { self.lenv }
    pub fn denv(&self) -> Option<ValueRefT<Env>> { self.denv }
    pub fn block(&self) -> ValueRefT<Block> { self.block }
    pub fn index(&self) -> usize { self.index }
}

impl Debug for BlockCont {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("CalleeCont")
         .field("base", &self.base)
         .field("parent", &self.parent)
         .field("block", &self.block)
         .field("index", &self.index)
         .finish()
    }
}

impl Display for BlockCont {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "#<ContinuationFrame (Block)>")
    }
}

// ================================================================================================

heap_struct! {
    pub struct CalleeCont: UniformHeapValue {
        parent: ValueRef,
        call: ValueRefT<Call>
    }
}

impl CalleeCont {
    pub fn new(allocator: &mut Allocator, parent: ValueRef, call: ValueRefT<Call>)
        -> Option<ValueRefT<CalleeCont>>
    {
        allocator.create_uniform(|base| CalleeCont { base, parent, call })
    }
}

impl Debug for CalleeCont {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("CalleeCont")
         .field("base", &self.base)
         .field("parent", &self.parent)
         .field("call", &self.call)
         .finish()
    }
}

impl Display for CalleeCont {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "#<ContinuationFrame (Callee)>")
    }
}
