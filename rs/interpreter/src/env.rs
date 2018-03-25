use std::slice;
use std::iter;
use std::fmt::{self, Debug, Display, Formatter};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use pcws_domain::Allocator;
use pcws_domain::object_model::{RefTailed, ValueRef, ValueRefT};
use pcws_domain::values::{Symbol, Reinit};

// ================================================================================================

#[derive(Debug)]
pub struct Unbound(ValueRefT<Symbol>);

#[derive(Debug)]
pub enum InitError {
    Unbound(Unbound),
    Reinit(Reinit)
}

impl From<Unbound> for InitError {
    fn from(err: Unbound) -> InitError { InitError::Unbound(err) }
}

impl From<Reinit> for InitError {
    fn from(err: Reinit) -> InitError { InitError::Reinit(err) }
}

// ================================================================================================

#[derive(Debug)]
#[repr(C)]
struct Entry {
    key: Option<ValueRefT<Symbol>>,
    value: Option<ValueRef>
}

heap_struct! {
    pub struct Env: RefTailed<TailItem=Option<ValueRef>> {
        parent: Option<ValueRefT<Env>>
    }
}

impl Env {
    pub fn new(heap: &mut Allocator, parent: Option<ValueRefT<Env>>, names: &[ValueRefT<Symbol>])
        -> Option<ValueRefT<Env>>
    {
        heap.create_with_iter(|base| Env { base, parent }, names.len() * 4,
                              iter::repeat::<Option<ValueRef>>(None))
            .map(|mut env| {
                for &name in names {
                    env.prepare_entry(name);
                }
                env
            })
    }

    fn prepare_entry(&mut self, name: ValueRefT<Symbol>) {
        let entries = self.entries_mut();
        let mut i = scaled_hash(name, entries.len());
        loop {
            if entries[i].key.is_none() {
                entries[i].key = Some(name);
                return;
            } else {
                i = (i + 1) % entries.len();
            }
        }
    }

    pub fn get(&self, name: ValueRefT<Symbol>) -> Result<Option<ValueRef>, Unbound> {
        if let Some(res) = self.get_local(name) {
            Ok(res)
        } else if let Some(parent) = self.parent {
            parent.get(name)
        } else {
            Err(Unbound(name))
        }
    }

    fn get_local(&self, name: ValueRefT<Symbol>) -> Option<Option<ValueRef>> {
        let entries = self.entries();
        let mut i = scaled_hash(name, entries.len());
        loop {
            match entries[i].key {
                Some(k) if k == name => return Some(entries[i].value),
                Some(k) => { i = (i + 1) % entries.len(); },
                None => return None
            }
        }
    }

    pub fn init(&mut self, name: ValueRefT<Symbol>, value: ValueRef) -> Result<(), InitError> {
        let entries = self.entries_mut();
        let mut i = scaled_hash(name, entries.len());
        loop {
            match entries[i].key {
                Some(k) if k == name => {
                    if entries[i].value.is_none() {
                        entries[i].value = Some(value);
                        return Ok(());
                    } else {
                        return Err(Reinit.into())
                    }
                },
                Some(k) => { i = (i + 1) % entries.len(); },
                None => return Err(Unbound(name).into())
            }
        }
    }

    fn entries(&self) -> &[Entry] {
        let tail = self.tail();
        unsafe { slice::from_raw_parts(tail.as_ptr() as _, tail.len() / 2) }
    }

    fn entries_mut(&mut self) -> &mut [Entry] {
        let tail = self.tail();
        unsafe { slice::from_raw_parts_mut(tail.as_ptr() as _, tail.len() / 2) }
    }
}

impl Debug for Env {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Env")
         .field("base", &self.base)
         .field("parent", &self.parent)
         .field("entries", &self.entries())
         .finish()
    }
}

impl Display for Env {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        Display::fmt("#<Env>", f)
    }
}

fn hash<T: Hash>(v: T) -> u64 {
    let mut hasher = DefaultHasher::new();
    v.hash(&mut hasher);
    hasher.finish()
}

fn scaled_hash<T: Hash>(v: T, len: usize) -> usize { hash(v) as usize % len }
