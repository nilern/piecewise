use core::nonzero::NonZero;
use std::mem::transmute;
use std::ptr::Unique;
use std::slice;
use std::collections::HashMap;

use gce::{GSize, Initializable, start_init, Generation};
use object_model::{HeapValueSub,
                   HeapValue, DynHeapValue, Type,
                   ValueRef, ScalarValueRef, HeapValueRef};
use value::{TypeIndex, TypeRegistry, ValueView, Promise, Symbol, Tuple};
use ast::{Function, Method, Block, Def, Call, Const, Lex};
use continuations::{BlockCont, DefCont, CalleeCont, ArgCont, Halt};
use env::{Env, Closure};

// ================================================================================================

/// Main interpreter state
pub struct Interpreter {
    gc: Generation<ValueRef>,
    // OPTIMIZE: make the key just point inside the value
    symbol_table: HashMap<String, HeapValueRef<Symbol>>,
    types: HashMap<TypeIndex, HeapValueRef<Type>>,
    type_idxs: HashMap<HeapValueRef<Type>, TypeIndex>
}

impl Interpreter {
    /// Create a new `Interpreter` with a maximum heap size of `max_heap` bytes.
    pub fn new(max_heap: usize) -> Interpreter {
        let mut res = Interpreter {
            gc: Generation::new(max_heap),
            symbol_table: HashMap::new(),
            types: HashMap::new(),
            type_idxs: HashMap::new()
        };

        res.with_gc_retry(|allocator| {
            let type_type: Initializable<Type> = unsafe { allocator.allocate_t() }.unwrap();
            allocator.interpreter
                     .insert(TypeIndex::Type, HeapValueRef::new(unsafe { transmute(type_type) }));
            allocator.uniform_init(type_type, |base| Type::make::<Type>(base, false, false));

            Self::insert_typ::<Tuple>(allocator).unwrap();
            Self::insert_typ::<Symbol>(allocator).unwrap();

            Self::insert_typ::<Promise>(allocator).unwrap();

            Self::insert_typ::<Function>(allocator).unwrap();
            Self::insert_typ::<Method>(allocator).unwrap();
            Self::insert_typ::<Block>(allocator).unwrap();
            Self::insert_typ::<Call>(allocator).unwrap();
            Self::insert_typ::<Def>(allocator).unwrap();
            Self::insert_typ::<Const>(allocator).unwrap();
            Self::insert_typ::<Lex>(allocator).unwrap();

            Self::insert_typ::<BlockCont>(allocator).unwrap();
            Self::insert_typ::<DefCont>(allocator).unwrap();
            Self::insert_typ::<CalleeCont>(allocator).unwrap();
            Self::insert_typ::<ArgCont>(allocator).unwrap();
            Self::insert_typ::<Halt>(allocator).unwrap();

            Self::insert_typ::<Env>(allocator).unwrap();
            Self::insert_typ::<Closure>(allocator).unwrap();

            Some(()) // HACK
        }, &mut []).unwrap();

        res
    }

    fn insert_typ<T: HeapValueSub>(allocator: &mut Allocator) -> Option<()> {
        allocator.create_typ::<T>()
                 .map(|typ| allocator.interpreter.insert(T::TYPE_INDEX, typ))
    }

    /// Call `f` with an `Allocator`. If `f` fails because of memory shortage it should return
    /// `None`. The first time this happens a garbage collection is performed using `roots` as the
    /// root set. The second time this happens the collector gives up and this function just
    /// returns the `None`. Due to the retry `f` must be idempotent.
    pub fn with_gc_retry<C, R>(&mut self, mut f: C, roots: &mut [&mut ValueRef]) -> Option<R>
        where C: FnMut(&mut Allocator) -> Option<R>
    {
        f(&mut Allocator { interpreter: self }).or_else(|| {
            unsafe {
                for root in roots {
                    **root = self.gc.mark_ref(**root);
                }
                // FIXME: mark roots in type and symbol tables within `self`.
                self.gc.collect();
            }
            f(&mut Allocator { interpreter: self })
        })
    }
}

impl TypeRegistry for Interpreter {
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

// OPTIMIZE: make this Copy
/// Memory allocator and value factory.
pub struct Allocator<'a> {
    interpreter: &'a mut Interpreter
}

impl<'a> Allocator<'a> {
    /// Find a symbol from the symbol table.
    pub fn get_symbol(&self, name: &str) -> Option<HeapValueRef<Symbol>> {
        self.interpreter.symbol_table.get(name).map(|&sym| sym)
    }

    /// Add a symbol to the symbol table.
    pub fn insert_symbol(&mut self, name: String, sym: HeapValueRef<Symbol>) {
        self.interpreter.symbol_table.insert(name, sym);
    }

    /// Allocate a `T` with a granule alignment of 1.
    pub unsafe fn allocate_t<T>(&mut self) -> Option<Initializable<T>> {
        self.interpreter.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                     NonZero::new_unchecked(GSize::of::<T>()))
    }

    fn init<T, F>(&self, ptr: Initializable<T>, f: F) -> HeapValueRef<T>
        where T: HeapValueSub, F: FnOnce(Unique<T>, HeapValue)
    {
        let uptr = start_init(ptr);
        let tvref = HeapValueRef::new(uptr);
        f(uptr, HeapValue {
            link: tvref.into(),
            typ: self.interpreter.get(T::TYPE_INDEX)
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

    /// Allocate and Initialize a `T` with a granule alignment of 1, delegating to `f` for
    /// everything but the `HeapValue` part.
    pub fn uniform_create<T, F>(&mut self, f: F) -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(HeapValue) -> T
    {
        unsafe { self.allocate_t() }.map(|typ| self.uniform_init(typ, f))
    }

    pub fn create_with_slice<T, F, E>(&mut self, gsize: GSize, f: F, slice: &[E])
        -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy
    {
        unsafe { self.interpreter.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(gsize)) }
            .map(|iptr| self.init_with_slice(iptr, f, slice))
    }

    pub fn create_with_vref_slice<T, F, E>(&mut self, f: F, slice: &[E]) -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy + Into<ValueRef>
    {
        self.create_with_slice(GSize::of::<T>() + GSize::from(slice.len()), f, slice)
    }

    pub fn create_with_vref_iter<T, F, I, E>(&mut self, f: F, len: usize, iter: I)
        -> Option<HeapValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T,
              I: Iterator<Item=E>, E: Copy + Into<ValueRef>
    {
        unsafe { self.interpreter.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
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
        T::new_typ(self)
    }

    /// Create a new Tuple.
    pub fn create_tuple<I>(&mut self, len: usize, values: I) -> Option<HeapValueRef<Tuple>>
        where I: Iterator<Item=ValueRef>
    {
        Tuple::new(self, len, values)
    }

    /// Create a new `Symbol` from `chars`.
    pub fn create_symbol(&mut self, chars: &str) -> Option<HeapValueRef<Symbol>> {
        Symbol::new(self, chars)
    }

    /// Create an uninitialized `Promise`.
    pub fn create_promise(&mut self) -> Option<HeapValueRef<Promise>> {
        Promise::new(self)
    }

    /// Create a new `Function` node with `methods`.
    pub fn create_function(&mut self, methods: &[HeapValueRef<Method>])
        -> Option<HeapValueRef<Function>>
    {
        Function::new(self, methods)
    }

    /// Create a new `Method` node.
    pub fn create_method(&mut self, pattern: ValueRef, guard: ValueRef, body: ValueRef)
        -> Option<HeapValueRef<Method>>
    {
        Method::new(self, pattern, guard, body)
    }

    /// Create a new `Block` node from `stmts` and `expr`.
    pub fn create_block(&mut self, stmts: &[ValueRef], expr: ValueRef)
        -> Option<HeapValueRef<Block>>
    {
        Block::new(self, stmts, expr)
    }

    /// Create a new `Call` node from `callee` and `args`.
    pub fn create_def(&mut self, pattern: ValueRef, expr: ValueRef) -> Option<HeapValueRef<Def>> {
        Def::new(self, pattern, expr)
    }

    /// Create a new `Call` node from `callee` and `args`.
    pub fn create_call(&mut self, callee: ValueRef, args: &[ValueRef])
        -> Option<HeapValueRef<Call>>
    {
        Call::new(self, callee, args)
    }

    /// Create a new `Const` node of `value`.
    pub fn create_const(&mut self, value: ValueRef) -> Option<HeapValueRef<Const>> {
        Const::new(self, value)
    }

    /// Create a new `Lex` node for the variable named `name`.
    pub fn create_lex(&mut self, name: HeapValueRef<Symbol>) -> Option<HeapValueRef<Lex>> {
        Lex::new(self, name)
    }

    /// Create a new block continuation
    pub fn create_block_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                             block: HeapValueRef<Block>, index: usize)
        -> Option<HeapValueRef<BlockCont>>
    {
        BlockCont::new(self, parent, lenv, denv, block, index)
    }

    /// Create a new assignment continuation
    pub fn create_def_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                           var: ValueRef)
        -> Option<HeapValueRef<DefCont>>
    {
        DefCont::new(self, parent, lenv, denv, var)
    }

    /// Create new callee continuation
    pub fn create_callee_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                              call: HeapValueRef<Call>) -> Option<HeapValueRef<CalleeCont>>
    {
        CalleeCont::new(self, parent, lenv, denv, call)
    }

    /// Create new argument continuation
    pub fn create_arg_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                           call: HeapValueRef<Call>, index: usize, callee: ValueRef,
                           args: &[ValueRef])
        -> Option<HeapValueRef<ArgCont>>
    {
        ArgCont::new(self, parent, lenv, denv, call, index, callee, args)
    }

    /// Create a new halt continuation.
    pub fn create_halt(&mut self) -> Option<HeapValueRef<Halt>> {
        Halt::new(self)
    }

    /// Create a new lexical block `Env` that inherits from (= represents inner scope of) `Env`.
    pub fn create_block_lenv(&mut self, parent: ValueRef, stmts: &[ValueRef])
        -> Option<HeapValueRef<Env>>
    {
        let mut bindings: Vec<ValueRef> = Vec::with_capacity(2*stmts.len());
        for stmt in stmts {
            if let ValueView::Def(def) = stmt.view(self.interpreter) {
                if let ValueView::Lex(lvar) = def.pattern.view(self.interpreter) {
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

impl<'a> TypeRegistry for Allocator<'a> {
    fn insert(&mut self, index: TypeIndex, typ: HeapValueRef<Type>) {
        self.interpreter.insert(index, typ)
    }

    fn get(&self, index: TypeIndex) -> HeapValueRef<Type> {
        self.interpreter.get(index)
    }

    fn index_of(&self, typ: HeapValueRef<Type>) -> TypeIndex {
        self.interpreter.index_of(typ)
    }
}
