use std::fmt::{self, Formatter};

use pcws_domain::{Allocator, DynamicDebug, DynamicDisplay};
use pcws_domain::object_model::ValueRefT;

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
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Hlat")
         .field("base", &self.base.debug_wrap(types))
         .finish()
    }
}

impl DynamicDisplay for Halt {
    fn fmt(&self, f: &mut Formatter, _: &Allocator) -> Result<(), fmt::Error> {
        write!(f, "#<ContinuationFrame (Halt)>")
    }
}
