use std::fmt::{self, Formatter};

use pcws_domain::{Allocator, DynamicDebug, DynamicDisplay};
use pcws_domain::object_model::{ValueRef, ValueRefT};
use ast::Call;

// ================================================================================================

heap_struct! {
    pub struct Halt: UniformHeapValue {}
}

impl Halt {
    pub fn new(allocator: &mut Allocator) -> Option<ValueRefT<Halt>> {
        allocator.create_uniform(|base| Halt { base })
    }
}

impl DynamicDebug for Halt {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Halt")
         .field("base", &self.base.debug_wrap(types))
         .finish()
    }
}

impl DynamicDisplay for Halt {
    fn fmt(&self, f: &mut Formatter, _: &mut Allocator) -> Result<(), fmt::Error> {
        write!(f, "#<ContinuationFrame (Halt)>")
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
    pub fn new(heap: &mut Allocator, parent: ValueRef, call: ValueRefT<Call>)
        -> Option<ValueRefT<CalleeCont>>
    {
        heap.create_uniform(|base| CalleeCont { base, parent, call })
    }
}

impl DynamicDebug for CalleeCont {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("CalleeCont")
         .field("base", &self.base.debug_wrap(types))
         .field("parent", &self.parent.debug_wrap(types))
         .field("call", &self.call.debug_wrap(types))
         .finish()
    }
}

impl DynamicDisplay for CalleeCont {
    fn fmt(&self, f: &mut Formatter, _: &mut Allocator) -> Result<(), fmt::Error> {
        write!(f, "#<ContinuationFrame (Callee)>")
    }
}
