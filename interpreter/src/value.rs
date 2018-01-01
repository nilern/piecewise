use core::nonzero::NonZero;
use std::mem::transmute;
use std::fmt::{self, Debug, Formatter};
use std::slice;
use std::collections::HashMap;
use std::ptr::Unique;
use std::str;

use gce::{GSize, Initializable, start_init, Generation};
use object_model::{HeapValueSub, DynHeapValueSub, DynamicDebug,
                   HeapValue, DynHeapValue, Type,
                   ValueRef, ScalarValueRef, HeapValueRef};
use ast::{Function, Method, Block, Def, Call, Const, Lex};
use continuations::{BlockCont, DefCont, CalleeCont, ArgCont, Halt};
use env::{Env, Closure};

// ================================================================================================

#[derive(Debug)]
pub struct Reinit;

// ================================================================================================

/// A reified type tag.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum TypeIndex {
    Type,
    Tuple,
    Symbol,

    Promise,

    Function,
    Method,
    Block,
    Call,
    Def,
    Lex,
    Const,

    BlockCont,
    DefCont,
    CalleeCont,
    ArgCont,
    Halt,

    Env,
    Closure
}

/// Converts between type values and `TypeIndex`:es.
pub trait TypeRegistry {
    /// Add a mapping.
    fn insert(&mut self, index: TypeIndex, typ: HeapValueRef<Type>);

    /// Get the `Type` for `index`.
    fn get(&self, index: TypeIndex) -> HeapValueRef<Type>;

    /// Get the `TypeIndex` for `typ`.
    fn index_of(&self, typ: HeapValueRef<Type>) -> TypeIndex;
}

// ================================================================================================

/// Unwraps scalars and makes heap value typing static.
pub enum ValueView {
    Type(HeapValueRef<Type>),
    Tuple(HeapValueRef<Tuple>),
    Symbol(HeapValueRef<Symbol>),

    Promise(HeapValueRef<Promise>),

    Function(HeapValueRef<Function>),
    Method(HeapValueRef<Method>),
    Block(HeapValueRef<Block>),
    Call(HeapValueRef<Call>),
    Def(HeapValueRef<Def>),
    Lex(HeapValueRef<Lex>),
    Const(HeapValueRef<Const>),

    BlockCont(HeapValueRef<BlockCont>),
    DefCont(HeapValueRef<DefCont>),
    CalleeCont(HeapValueRef<CalleeCont>),
    ArgCont(HeapValueRef<ArgCont>),
    Halt(HeapValueRef<Halt>),

    Env(HeapValueRef<Env>),
    Closure(HeapValueRef<Closure>),

    Null,

    Int(ScalarValueRef<isize>),
    Float(ScalarValueRef<f64>),
    Char(ScalarValueRef<char>),
    Bool(ScalarValueRef<bool>)
}

impl DynamicDebug for ValueView {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error>
    {
        use self::ValueView::*;

        match self {
            &Type(vref) => vref.fmt(f, types),
            &Tuple(vref) => vref.fmt(f, types),
            &Symbol(vref) => vref.fmt(f, types),

            &Promise(vref) => vref.fmt(f, types),

            &Function(vref) => vref.fmt(f, types),
            &Method(vref) => vref.fmt(f, types),
            &Block(vref) => vref.fmt(f, types),
            &Call(vref) => vref.fmt(f, types),
            &Def(vref) => vref.fmt(f, types),
            &Lex(vref) => vref.fmt(f, types),
            &Const(vref) => vref.fmt(f, types),

            &BlockCont(vref) => vref.fmt(f, types),
            &DefCont(vref) => vref.fmt(f, types),
            &CalleeCont(vref) => vref.fmt(f, types),
            &ArgCont(vref) => vref.fmt(f, types),
            &Halt(vref) => vref.fmt(f, types),

            &Env(vref) => vref.fmt(f, types),
            &Closure(vref) => vref.fmt(f, types),

            &Null => f.write_str("NULL"),

            &Int(v) => v.fmt(f),
            &Float(v) => v.fmt(f),
            &Char(v) => v.fmt(f),
            &Bool(v) => v.fmt(f)
        }
    }
}

// ================================================================================================

// TODO: `mod domain` for these

/// Tuple
pub struct Tuple {
    base: DynHeapValue
}

impl Tuple {
    pub fn values(&self) -> &[ValueRef] { self.tail() }
}

impl HeapValueSub for Tuple {
    const TYPE_INDEX: TypeIndex = TypeIndex::Tuple;
    const UNIFORM_REF_LEN: usize = 0;

    fn new_typ(typ_base: HeapValue) -> Type { Type::dyn_refs::<Self>(typ_base) }
}

impl DynHeapValueSub for Tuple {
    type TailItem = ValueRef;
}

impl DynamicDebug for Tuple {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Tuple")
         .field("base", &self.base.fmt_wrap(types))
         .field("values", &self.values().fmt_wrap(types))
         .finish()
    }
}

/// Symbol (hash-consed string)
pub struct Symbol {
    base: DynHeapValue
}

impl Symbol {
    fn new(allocator: &mut Allocator, chars: &str) -> Option<HeapValueRef<Symbol>> {
        allocator.get_symbol(chars)
                 .or_else(|| {
                     let bytes = chars.as_bytes();
                     let gsize = GSize::of::<Symbol>() + GSize::from_bytesize(bytes.len());
                     let sym = allocator.create_with_slice(gsize, |base| Symbol { base }, bytes);
                     if let Some(sym) = sym {
                         allocator.insert_symbol(chars.to_string(), sym);
                     }
                     sym
                 })
    }

    fn chars(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.tail()) }
    }
}

impl HeapValueSub for Symbol {
    const TYPE_INDEX: TypeIndex = TypeIndex::Symbol;
    const UNIFORM_REF_LEN: usize = 0;

    fn new_typ(typ_base: HeapValue) -> Type { Type::dyn_bytes::<Self>(typ_base) }
}

impl DynHeapValueSub for Symbol {
    type TailItem = u8;
}

impl DynamicDebug for Symbol {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Symbol")
         .field("base", &self.base.fmt_wrap(types))
         .field("chars", &self.chars())
         .finish()
    }
}

/// Indirection
pub struct Promise {
    base: HeapValue
}

impl HeapValueSub for Promise {
    const TYPE_INDEX: TypeIndex = TypeIndex::Promise;
    const UNIFORM_REF_LEN: usize = 0;

    fn new_typ(typ_base: HeapValue) -> Type { Type::uniform::<Self>(typ_base) }
}

impl Promise {
    pub fn init<R: TypeRegistry>(&mut self, value: ValueRef, types: &R) -> Result<(), Reinit> {
        if let ValueView::Null = self.base.link.view(types) {
            self.base.link = value;
            Ok(())
        } else {
            Err(Reinit)
        }
    }
}

impl DynamicDebug for Promise {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_struct("Promise")
         .field("base", &self.base.fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

#[derive(Debug)]
pub struct OutOfMemory;

/// Memory manager and value factory.
pub struct Allocator {
    gc: Generation<ValueRef>,
    // OPTIMIZE: make the key just point inside the value
    symbol_table: HashMap<String, HeapValueRef<Symbol>>,
    types: HashMap<TypeIndex, HeapValueRef<Type>>,
    type_idxs: HashMap<HeapValueRef<Type>, TypeIndex>
}

impl Allocator {
    /// Create a new `Allocator` with a maximum heap size of `max_heap`.
    pub fn new(max_heap: usize) -> Allocator {
        let mut res = Allocator {
            gc: Generation::new(max_heap),
            symbol_table: HashMap::new(),
            types: HashMap::new(),
            type_idxs: HashMap::new()
        };

        let type_type: Initializable<Type> = unsafe { res.allocate_t() }.unwrap();
        res.insert(TypeIndex::Type, HeapValueRef::new(unsafe { transmute(type_type) }));
        res.uniform_init(type_type, |typ_base| Type::new_typ(typ_base));

        res.insert_typ::<Tuple>().unwrap();
        res.insert_typ::<Symbol>().unwrap();

        res.insert_typ::<Promise>().unwrap();

        res.insert_typ::<Function>().unwrap();
        res.insert_typ::<Method>().unwrap();
        res.insert_typ::<Block>().unwrap();
        res.insert_typ::<Call>().unwrap();
        res.insert_typ::<Def>().unwrap();
        res.insert_typ::<Const>().unwrap();
        res.insert_typ::<Lex>().unwrap();

        res.insert_typ::<BlockCont>().unwrap();
        res.insert_typ::<DefCont>().unwrap();
        res.insert_typ::<CalleeCont>().unwrap();
        res.insert_typ::<ArgCont>().unwrap();
        res.insert_typ::<Halt>().unwrap();

        res.insert_typ::<Env>().unwrap();
        res.insert_typ::<Closure>().unwrap();

        res
    }

    fn insert_typ<T: HeapValueSub>(&mut self) -> Option<()> {
        self.create_typ::<T>()
            .map(|typ| self.insert(T::TYPE_INDEX, typ))
    }

    fn get_symbol(&self, name: &str) -> Option<HeapValueRef<Symbol>> {
        self.symbol_table.get(name).map(|&sym| sym)
    }

    fn insert_symbol(&mut self, name: String, sym: HeapValueRef<Symbol>) {
        self.symbol_table.insert(name, sym);
    }

    pub fn with_gc_retry<C, R>(&mut self, mut create: C, roots: &mut [&mut ValueRef])
        -> Result<R, OutOfMemory>
        where C: FnMut(&mut Self) -> Option<R>
    {
        create(self).or_else(|| {
            unsafe {
                for root in roots {
                    **root = self.gc.mark_ref(**root);
                }
                // FIXME: mark roots in type and symbol tables within `self`.
                self.gc.collect();
            }
            create(self)
        })
        .ok_or(OutOfMemory)
    }

    fn init<T, F>(&self, ptr: Initializable<T>, f: F) -> HeapValueRef<T>
        where T: HeapValueSub, F: FnOnce(Unique<T>, HeapValue)
    {
        let uptr = start_init(ptr);
        let tvref = HeapValueRef::new(uptr);
        f(uptr, HeapValue {
            link: tvref.into(),
            typ: self.get(T::TYPE_INDEX)
        });
        tvref
    }

    /// Initialize a `T`, delegating to `f` for everything but the `HeapValue` part.
    fn uniform_init<T, F>(&self, ptr: Initializable<T>, f: F) -> HeapValueRef<T>
        where T: HeapValueSub, F: FnOnce(HeapValue) -> T
    {
        self.init(ptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(heap_value);
        })
    }

    fn init_with_slice<T, F, E>(&self, iptr: Initializable<T>, f: F, slice: &[E])
        -> HeapValueRef<T>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy
    {
        self.init(iptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(DynHeapValue {
                base: heap_value,
                dyn_len: slice.len()
            });
            let dest_slice: &mut[E] = unsafe {
                slice::from_raw_parts_mut(uptr.as_ptr().offset(1) as *mut E, slice.len())
            };
            dest_slice.copy_from_slice(slice);
        })
    }

    fn init_with_iter<T, F, I, E>(&self, iptr: Initializable<T>, f: F, len: usize, iter: I)
        -> HeapValueRef<T>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T,
              I: Iterator<Item=E>, E: Copy
    {
        self.init(iptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(DynHeapValue {
                base: heap_value,
                dyn_len: len
            });
            let dest_slice: &mut[E] = unsafe {
                slice::from_raw_parts_mut(uptr.as_ptr().offset(1) as *mut E, len)
            };
            for (loc, value) in dest_slice.iter_mut().zip(iter) {
                *loc = value;
            }
        })
    }

    /// Allocate a `T` with a granule alignment of 1.
    unsafe fn allocate_t<T>(&mut self) -> Option<Initializable<T>> {
        self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                         NonZero::new_unchecked(GSize::of::<T>()))
    }

    /// Allocate and Initialize a `T` with a granule alignment of 1, delegating to `f` for
    /// everything but the `HeapValue` part.
    fn uniform_create<T, F>(&mut self, f: F) -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(HeapValue) -> T
    {
        unsafe { self.allocate_t() }.map(|typ| self.uniform_init(typ, f))
    }

    fn create_with_slice<T, F, E>(&mut self, gsize: GSize, f: F, slice: &[E])
        -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy
    {
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(gsize)) }
            .map(|iptr| self.init_with_slice(iptr, f, slice))
    }

    fn create_with_vref_slice<T, F, E>(&mut self, f: F, slice: &[E]) -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy + Into<ValueRef>
    {
        self.create_with_slice(GSize::of::<T>() + GSize::from(slice.len()), f, slice)
    }

    fn create_with_vref_iter<T, F, I, E>(&mut self, f: F, len: usize, iter: I)
        -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T,
              I: Iterator<Item=E>, E: Copy + Into<ValueRef>
    {
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(GSize::of::<T>() + GSize::from(len))) }
            .map(|iptr| self.init_with_iter(iptr, f, len, iter))
    }

    /// Create a new `Int` from `n`.
    pub fn create_int(&self, n: isize) -> ScalarValueRef<isize> { ScalarValueRef::from(n) }

    /// Create a new `Float` from `n`.
    pub fn create_float(&self, n: f64) -> ScalarValueRef<f64> { ScalarValueRef::from(n) }

    /// Create a new `Char` from `c`.
    pub fn create_char(&self, c: char) -> ScalarValueRef<char> { ScalarValueRef::from(c) }

    /// Create a new `Bool` from `b`.
    pub fn create_bool(&self, b: bool) -> ScalarValueRef<bool> { ScalarValueRef::from(b) }

    /// Create a new dynamic type for `T` (using `T::new_typ`)
    pub fn create_typ<T: HeapValueSub>(&mut self) -> Option<HeapValueRef<Type>> {
        self.uniform_create(T::new_typ)
    }

    /// Create a new Tuple.
    pub fn create_tuple<I>(&mut self, len: usize, values: I) -> Option<HeapValueRef<Tuple>>
        where I: Iterator<Item=ValueRef>
    {
        self.create_with_vref_iter(|base| Tuple { base }, len, values)
    }

    /// Create a new `Symbol` from `chars`.
    pub fn create_symbol(&mut self, chars: &str) -> Option<HeapValueRef<Symbol>> {
        self.symbol_table
            .get(chars).map(|&sym| sym)
            .or_else(|| {
                let bytes = chars.as_bytes();
                let gsize = GSize::of::<Symbol>() + GSize::from_bytesize(bytes.len());
                let sym = self.create_with_slice(gsize, |base| Symbol { base }, bytes);
                if let Some(sym) = sym {
                    self.symbol_table.insert(chars.to_string(), sym);
                }
                sym
            })
    }

    /// Create an uninitialized `Promise`.
    pub fn create_promise(&mut self) -> Option<HeapValueRef<Promise>> {
        unsafe { self.allocate_t() }
            .map(|iptr| {
                let mut uptr = start_init(iptr);
                *unsafe { uptr.as_mut() } = Promise {
                    base: HeapValue {
                        link: ValueRef::NULL,
                        typ: self.get(TypeIndex::Promise)
                    }
                };
                HeapValueRef::new(uptr)
            })
    }

    /// Create a new `Function` node with `methods`.
    pub fn create_function(&mut self, methods: &[HeapValueRef<Method>])
        -> Option<HeapValueRef<Function>>
    {
        self.create_with_vref_slice(|base| Function { base }, methods)
    }

    /// Create a new `Method` node.
    pub fn create_method(&mut self, pattern: ValueRef, guard: ValueRef, body: ValueRef)
        -> Option<HeapValueRef<Method>>
    {
        self.uniform_create(|base| Method { base, pattern, guard, body })
    }

    /// Create a new `Block` node from `stmts` and `expr`.
    pub fn create_block(&mut self, stmts: &[ValueRef], expr: ValueRef)
        -> Option<HeapValueRef<Block>>
    {
        self.create_with_vref_slice(|base| Block { base, expr }, stmts)
    }

    /// Create a new `Call` node from `callee` and `args`.
    pub fn create_def(&mut self, pattern: ValueRef, expr: ValueRef) -> Option<HeapValueRef<Def>> {
        self.uniform_create(|base| Def { base, pattern, expr })
    }

    /// Create a new `Call` node from `callee` and `args`.
    pub fn create_call(&mut self, callee: ValueRef, args: &[ValueRef])
        -> Option<HeapValueRef<Call>>
    {
        self.create_with_vref_slice(|base| Call { base, callee }, args)
    }

    /// Create a new `Const` node of `value`.
    pub fn create_const(&mut self, value: ValueRef) -> Option<HeapValueRef<Const>> {
        self.uniform_create(|heap_value| Const { heap_value, value })
    }

    /// Create a new `Lex` node for the variable named `name`.
    pub fn create_lex(&mut self, name: HeapValueRef<Symbol>) -> Option<HeapValueRef<Lex>> {
        self.uniform_create(|base| Lex { base, name })
    }

    /// Create a new block continuation
    pub fn create_block_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                             block: HeapValueRef<Block>, index: usize)
        -> Option<HeapValueRef<BlockCont>>
    {
        self.uniform_create(|base| BlockCont {
            base, parent, lenv, denv, block,
            index: (index as isize).into()
        })
    }

    /// Create a new assignment continuation
    pub fn create_def_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                           var: ValueRef)
        -> Option<HeapValueRef<DefCont>>
    {
        self.uniform_create(|base| DefCont { base, parent, lenv, denv, var })
    }

    /// Create new callee continuation
    pub fn create_callee_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                              call: HeapValueRef<Call>) -> Option<HeapValueRef<CalleeCont>>
    {
        self.uniform_create(|base| CalleeCont { base, parent, lenv, denv, call })
    }

    /// Create new argument continuation
    pub fn create_arg_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                           call: HeapValueRef<Call>, index: usize, callee: ValueRef,
                           args: &[ValueRef])
        -> Option<HeapValueRef<ArgCont>>
    {
        self.create_with_vref_slice(|base| ArgCont {
            base, parent, lenv, denv, call, index: (index as isize).into(), callee
        }, args)
    }

    /// Create a new halt continuation.
    pub fn create_halt(&mut self) -> Option<HeapValueRef<Halt>> {
        self.uniform_create(|base| Halt { base })
    }

    /// Create a new lexical block `Env` that inherits from (= represents inner scope of) `Env`.
    pub fn create_block_lenv(&mut self, parent: ValueRef, stmts: &[ValueRef])
        -> Option<HeapValueRef<Env>>
    {
        let mut bindings: Vec<ValueRef> = Vec::with_capacity(2*stmts.len());
        for stmt in stmts {
            if let ValueView::Def(def) = stmt.view(&*self) {
                if let ValueView::Lex(lvar) = def.pattern.view(&*self) {
                    bindings.push(lvar.name.into());
                    if let Some(promise) = self.create_promise() {
                        bindings.push(promise.into());
                    } else {
                        return None;
                    }
                }
            }
        }

        self.create_with_vref_slice(|base| Env { base, parent }, &bindings)
    }

    /// Create a new dynamic block `Env` that inherits from (= represents inner scope of) `Env`.
    pub fn create_block_denv(&mut self, parent: ValueRef, stmts: &[ValueRef])
        -> Option<HeapValueRef<Env>>
    {
        let empty_dummy: [ValueRef; 0] = [];
        self.create_with_vref_slice(|base| Env { base, parent }, &empty_dummy)
    }

    /// Create a new lexical method `Env` that inherits from `parent`.
    pub fn create_method_lenv(&mut self, parent: ValueRef, param: HeapValueRef<Symbol>,
                              arg: ValueRef)
        -> Option<HeapValueRef<Env>>
    {
        self.create_with_vref_slice(|base| Env { base, parent }, &[param.into(), arg])
    }

    /// Close `function` over `lenv`.
    pub fn create_closure(&mut self, function: HeapValueRef<Function>, lenv: ValueRef)
        -> Option<HeapValueRef<Closure>>
    {
        self.uniform_create(|base| Closure { base, function, lenv })
    }
}

impl TypeRegistry for Allocator {
    fn insert(&mut self, index: TypeIndex, typ: HeapValueRef<Type>) {
        self.types.insert(index, typ);
        self.type_idxs.insert(typ, index);
    }

    fn get(&self, index: TypeIndex) -> HeapValueRef<Type> {
        *self.types.get(&index).expect(&format!("No type found for {:?}", index))
    }

    fn index_of(&self, typ: HeapValueRef<Type>) -> TypeIndex {
        *self.type_idxs.get(&typ).unwrap()
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use super::*;

    use std::iter;

    use gce::GSize;

    #[test]
    fn sizes() {
        assert_eq!(GSize::of::<HeapValue>(), GSize::from(2));
        assert_eq!(GSize::of::<DynHeapValue>(), GSize::from(3));

        assert_eq!(GSize::of::<Type>(), GSize::from(4));
        assert_eq!(GSize::of::<Tuple>(), GSize::from(3));
        assert_eq!(GSize::of::<Symbol>(), GSize::from(3));

        assert_eq!(GSize::of::<Promise>(), GSize::of::<HeapValue>());

        assert_eq!(GSize::of::<Function>(), GSize::from(3));
        assert_eq!(GSize::of::<Method>(), GSize::from(5));
        assert_eq!(GSize::of::<Block>(), GSize::from(4));
        assert_eq!(GSize::of::<Call>(), GSize::from(4));
        assert_eq!(GSize::of::<Def>(), GSize::from(4));
        assert_eq!(GSize::of::<Lex>(), GSize::from(3));
        assert_eq!(GSize::of::<Const>(), GSize::from(3));

        assert_eq!(GSize::of::<BlockCont>(), GSize::from(7));
        assert_eq!(GSize::of::<DefCont>(), GSize::from(6));
        assert_eq!(GSize::of::<CalleeCont>(), GSize::from(6));
        assert_eq!(GSize::of::<ArgCont>(), GSize::from(9));
        assert_eq!(GSize::of::<Halt>(), GSize::from(2));

        assert_eq!(GSize::of::<Env>(), GSize::from(4));
    }

    #[test]
    fn create() {
        let mut factory = Allocator::new(1024);

        let n = factory.create_int(5).into();
        let tup = factory.create_tuple(1, iter::once(n)).unwrap();
        let sym = factory.create_symbol(&"foo").unwrap();

        let promise = factory.create_promise().unwrap();

        let call = factory.create_call(From::from(sym), &[From::from(n), From::from(sym)])
                          .unwrap();
        let lex = factory.create_lex(sym).unwrap().into();
        let c = factory.create_const(From::from(n)).unwrap().into();
        let def = factory.create_def(lex, c).unwrap();
        let block = factory.create_block(&[def.into(), call.into(), lex], c)
                           .unwrap();
        let method = factory.create_method(From::from(sym), From::from(sym), From::from(block))
                            .unwrap();
        factory.create_function(&[method]).unwrap();

        factory.create_halt().unwrap();

        // TODO: factory.create_..._env(...);
        //       factory.create_..._cont(...).unwrap();
    }
}
