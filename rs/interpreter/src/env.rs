use std::fmt::{self, Debug, Display, Formatter};

use pcws_domain::Allocator;
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

impl Debug for Env {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

impl Display for Env {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}
