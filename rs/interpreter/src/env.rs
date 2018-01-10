use std::fmt::{self, Formatter};
use std::ops::Deref;

use domain::{TypeRegistry, Type, Symbol};
use interpreter::Allocator;
use object_model::{HeapValueSub, DynHeapValueSub, DynamicDebug,
                   HeapValue, DynHeapValue,
                   ValueRef, HeapValueRef};
use value::{TypeIndex, ValueView};
use ast::Function;

// ================================================================================================

/// Error for unbound variables.
pub struct Unbound(pub HeapValueRef<Symbol>);

impl DynamicDebug for Unbound {
   fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
       f.debug_tuple("Unbound")
        .field(&self.0.fmt_wrap(types))
        .finish()
   }
}

// ================================================================================================

/// Environment
#[repr(C)]
pub struct Env {
    pub base: DynHeapValue,
    pub parent: ValueRef
}

impl Env {
    pub fn block_lenv(allocator: &mut Allocator, parent: ValueRef, stmts: &[ValueRef])
        -> Option<HeapValueRef<Env>>
    {
        let mut bindings: Vec<ValueRef> = Vec::with_capacity(2*stmts.len());
        for stmt in stmts {
            if let ValueView::Def(def) = stmt.view(allocator.interpreter) {
                if let ValueView::Lex(lvar) = def.pattern.view(allocator.interpreter) {
                    bindings.push(lvar.name.into());
                    if let Some(promise) = allocator.create_promise() {
                        bindings.push(promise.into());
                    } else {
                        return None;
                    }
                }
            }
        }

        allocator.create_with_vref_slice(|base| Env { base, parent }, &bindings)
    }

    pub fn block_denv(allocator: &mut Allocator, parent: ValueRef, stmts: &[ValueRef])
        -> Option<HeapValueRef<Env>>
    {
        let empty_dummy: [ValueRef; 0] = [];
        allocator.create_with_vref_slice(|base| Env { base, parent }, &empty_dummy)
    }

    pub fn method_lenv(allocator: &mut Allocator, parent: ValueRef, param: HeapValueRef<Symbol>,
                       arg: ValueRef) -> Option<HeapValueRef<Env>>
    {
        allocator.create_with_vref_slice(|base| Env { base, parent }, &[param.into(), arg])
    }

    fn parent<R: TypeRegistry>(&self, types: &R) -> Option<HeapValueRef<Env>> {
        if let ValueView::Env(parent) = self.parent.view(types) {
            Some(parent)
        } else {
            None
        }
    }

    pub fn get<R: TypeRegistry>(&self, name: HeapValueRef<Symbol>, types: &R)
        -> Result<ValueRef, Unbound>
    {
        let mut frame: *const Env = self as _;

        loop {
            let mut iter = unsafe { (*frame).tail() }.iter();

            while let Some(&item) = iter.next() {
                if unsafe { item.downcast() } == name {
                    return Ok((*iter.next().unwrap()).into());
                } else {
                    iter.next();
                }
            }

            if let Some(parent) = unsafe { (*frame).parent(types) } {
                frame = parent.deref() as _;
            } else {
                break;
            }
        }

        return Err(Unbound(name))
    }
}

impl HeapValueSub for Env {
    const TYPE_INDEX: TypeIndex = TypeIndex::Env;
    const UNIFORM_REF_LEN: usize = 1;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::dyn_refs::<Self>(allocator)
    }
}

impl DynHeapValueSub for Env {
    type TailItem = ValueRef;
}

impl DynamicDebug for Env {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Env")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("bindings", &self.tail().fmt_wrap(types))
         .finish()
    }
}

/// Function closure
pub struct Closure {
    pub base: HeapValue,
    pub function: HeapValueRef<Function>,
    pub lenv: ValueRef
}

impl Closure {
    pub fn new(allocator: &mut Allocator, function: HeapValueRef<Function>, lenv: ValueRef)
        -> Option<HeapValueRef<Closure>>
    {
        allocator.uniform_create(|base| Closure { base, function, lenv })
    }
}

impl HeapValueSub for Closure {
    const TYPE_INDEX: TypeIndex = TypeIndex::Closure;
    const UNIFORM_REF_LEN: usize = 2;

    fn new_typ(allocator: &mut Allocator) -> Option<HeapValueRef<Type>> {
        Type::uniform::<Self>(allocator)
    }
}

impl DynamicDebug for Closure {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Closure")
         .field("base", &self.base.fmt_wrap(types))
         .field("function", &self.function.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .finish()
    }
}
