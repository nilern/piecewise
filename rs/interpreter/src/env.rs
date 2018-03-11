use std::fmt::{self, Formatter};

use pcws_domain::{Allocator, DynamicDebug, DynamicDisplay};
use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::values::Symbol;

// ================================================================================================

heap_struct! {
    pub struct Env: RefTailed<TailItem=ValueRef> {

    }
}

impl Env {
    pub fn with_capacity(allocator: &mut Allocator, parent: ValueRefT<Env>, capacity: usize)
        -> ValueRefT<Env>
    {
        unimplemented!()
    }

    pub fn init(&mut self, name: ValueRefT<Symbol>, value: ValueRef) {
        unimplemented!()
    }

    pub fn get(&self, name: ValueRefT<Symbol>) -> Option<ValueRef> {
        unimplemented!()
    }

    fn entry(&self, name: ValueRefT<Symbol>) -> &(ValueRef, ValueRef) {
        unimplemented!()
    }

    fn entry_mut(&mut self, name: ValueRefT<Symbol>) -> &mut (ValueRef, ValueRef) {
        unimplemented!()
    }
}

impl DynamicDebug for Env {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

impl DynamicDisplay for Env {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}
