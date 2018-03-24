use std::fmt::{self, Debug, Display, Formatter};

use pcws_domain::Allocator;
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
