use std::slice;
use std::iter;
use std::fmt::{self, Debug, Display, Formatter};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use pcws_domain::Allocator;
use pcws_domain::object_model::{RefTailed, ValueRef, ValueRefT};
use pcws_domain::values::{Promise, Symbol, Reinit};

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
    pub fn block(heap: &mut Allocator, parent: Option<ValueRefT<Env>>, names: &[ValueRefT<Symbol>])
        -> Option<ValueRefT<Env>>
    {
        heap.create_with_iter(|base| Env { base, parent }, names.len() * 4,
                              iter::repeat::<Option<ValueRef>>(None))
            .and_then(|mut env| {
                for &name in names {
                    if !env.prepare_entry(heap, name) {
                        return None;
                    }
                }
                Some(env)
            })
    }

    fn prepare_entry(&mut self, heap: &mut Allocator, name: ValueRefT<Symbol>) -> bool {
        let entries = self.entries_mut();
        let mut i = scaled_hash(name, entries.len());
        loop {
            if entries[i].key.is_none() {
                if let Some(value) = Promise::new(heap) {
                    entries[i].key = Some(name);
                    entries[i].value = Some(value.into());
                    return true;
                } else {
                    return false;
                }
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

// ================================================================================================

heap_struct! {
    pub struct EnvBuffer: RefTailed<TailItem=Option<ValueRef>> {
        len: usize
    }
}

impl EnvBuffer {
    pub fn with_capacity(heap: &mut Allocator, cap: usize) -> Option<ValueRefT<EnvBuffer>> {
        heap.create_with_iter(|base| EnvBuffer { base, len: 0 },
                              cap, iter::repeat::<Option<ValueRef>>(None))
    }

    pub fn push(&mut self, val: ValueRef) {
        let len = self.len; // HACK: Until NLL arrives.
        debug_assert!(len <= self.base.dyn_len);
        self.tail_mut()[len] = Some(val);
        self.len += 1;
    }
}

impl Debug for EnvBuffer {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("EnvBuffer")
         .field("base", &self.base)
         .field("lenv", &self.len)
         .field("tail", &self.tail())
         .finish()
    }
}

impl Display for EnvBuffer {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        Display::fmt("#<EnvBuffer>", f)
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use pcws_domain::{Allocator, register_static_t};
    use pcws_domain::object_model::{ValueRefT, ValueRef};
    use pcws_domain::values::{Symbol, Promise};
    use super::Env;

    #[test]
    fn get() {
        register_static_t::<Promise>();
        register_static_t::<Symbol>();
        register_static_t::<Env>();

        let heap = &mut *Allocator::instance();
        let key = Symbol::new(heap, "foo").unwrap();
        let value = ValueRefT::from(5isize);
        let env = Env::block(heap, None, &[key]).unwrap();

        env.get(key).unwrap().unwrap()
           .try_downcast::<Promise>().unwrap()
           .init(value.into());

        assert_eq!(env.get(key).unwrap().unwrap()
                      .force().unwrap(),
                   ValueRef::from(value));
    }
}

// ================================================================================================
