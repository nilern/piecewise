use core::nonzero::NonZero;
use std::mem::transmute;
use std::fmt::{self, Debug, Formatter};
use std::slice;
use std::collections::HashMap;
use std::ptr::Unique;
use std::str;
use std::iter;

use gce::util::{start_init, Initializable};
use gce::Object;
use gce::layout::GSize;
use gce::mark_n_sweep::Generation;
use value_refs::{ValueRef, TypedValueRef};

// ================================================================================================

/// Like `std::fmt::Debug`, but needs a `TypeRegistry` because of the dynamic typing.
pub trait DynamicDebug: Sized {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error>;

    fn fmt_wrap<'a, 'b, R: TypeRegistry>(&'a self, type_reg: &'b R)
        -> DynDebugWrapper<'a, 'b, Self, R>
    {
        DynDebugWrapper {
            value: self,
            types: type_reg
        }
    }
}

pub struct DynDebugWrapper<'a, 'b, T: 'a + DynamicDebug, R: 'b + TypeRegistry> {
    value: &'a T,
    types: &'b R
}

impl<'a, 'b, T: DynamicDebug, R: TypeRegistry> Debug for DynDebugWrapper<'a, 'b, T, R> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        self.value.fmt(f, self.types)
    }
}

// ================================================================================================

/// A reified type tag.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum TypeIndex {
    Type,
    Symbol,

    Function,
    Method,
    Block,
    Call,
    Def,
    Lex,
    Const,

    BlockCont,
    Halt,

    Env
}

/// Obtain the `TypeIndex` of `Self`.
pub trait IndexedType {
    const TYPE_INDEX: TypeIndex;
}

/// Converts between type values and `TypeIndex`:es.
pub trait TypeRegistry {
    /// Add a mapping.
    fn insert(&mut self, index: TypeIndex, typ: TypedValueRef<Type>);

    /// Get the `Type` for `index`.
    fn get(&self, index: TypeIndex) -> TypedValueRef<Type>;

    /// Get the `TypeIndex` for `typ`.
    fn index_of(&self, typ: TypedValueRef<Type>) -> TypeIndex;
}

// ================================================================================================

/// Unwraps scalars and makes heap value typing static.
pub enum ValueView {
    Type(TypedValueRef<Type>),
    Symbol(TypedValueRef<Symbol>),

    Function(TypedValueRef<Function>),
    Method(TypedValueRef<Method>),
    Block(TypedValueRef<Block>),
    Call(TypedValueRef<Call>),
    Def(TypedValueRef<Def>),
    Lex(TypedValueRef<Lex>),
    Const(TypedValueRef<Const>),

    BlockCont(TypedValueRef<BlockCont>),
    Halt(TypedValueRef<Halt>),

    Env(TypedValueRef<Env>),

    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
}

impl DynamicDebug for ValueView {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error>
    {
        use self::ValueView::*;

        match self {
            &Type(vref) => vref.fmt(f, type_reg),
            &Symbol(vref) => vref.fmt(f, type_reg),

            &Function(vref) => vref.fmt(f, type_reg),
            &Method(vref) => vref.fmt(f, type_reg),
            &Block(vref) => vref.fmt(f, type_reg),
            &Call(vref) => vref.fmt(f, type_reg),
            &Def(vref) => vref.fmt(f, type_reg),
            &Lex(vref) => vref.fmt(f, type_reg),
            &Const(vref) => vref.fmt(f, type_reg),

            &BlockCont(vref) => vref.fmt(f, type_reg),
            &Halt(vref) => vref.fmt(f, type_reg),

            &Env(vref) => vref.fmt(f, type_reg),

            &Int(v) => v.fmt(f),
            &Float(v) => v.fmt(f),
            &Char(v) => v.fmt(f),
            &Bool(v) => v.fmt(f)
        }
    }
}

// ================================================================================================

/// The 'supertype' of all heap values (non-scalars).
#[repr(C)]
pub struct HeapValue {
    /// Multipurpose redirection field
    link: ValueRef,
    /// Dynamic type
    typ: TypedValueRef<Type>
}

#[repr(C)]
pub struct DynHeapValue {
    base: HeapValue,
    dyn_len: usize
}

impl HeapValue {
    pub fn typ(&self) -> &TypedValueRef<Type> { &self.typ }

    fn ref_len(&self) -> usize {
        if self.typ.has_dyn_ref_len() {
            self.typ.uniform_ref_len() + unsafe { transmute::<_, &DynHeapValue>(self) }.dyn_len
        } else {
            self.typ.uniform_ref_len()
        }
    }

    fn refs_ptr(&self) -> *mut ValueRef {
        if self.typ.has_dyn_ref_len() {
            (unsafe { transmute::<_, *const DynHeapValue>(self).offset(1) }) as _
        } else {
            (unsafe { transmute::<_, *const HeapValue>(self).offset(1) }) as _
        }
    }

    pub fn ref_fields(&self) -> ObjRefs {
        let ptr = self.refs_ptr();
        let end = unsafe { ptr.offset(self.ref_len() as isize) };
        ObjRefs { ptr, end }
    }
}

impl Object for HeapValue {
    fn gsize(&self) -> GSize {
        let gsize = self.typ.uniform_gsize();
        if self.typ.has_dyn_ref_len() {
            return GSize::from(gsize + 1 + unsafe { transmute::<_, &DynHeapValue>(self) }.dyn_len);
        }
        if self.typ.has_dyn_gsize() {
            return GSize::from(gsize + 1)
                 + GSize::from_bytesize(unsafe { transmute::<_, &DynHeapValue>(self) }.dyn_len);
        }
        GSize::from(gsize)
    }
}

impl DynamicDebug for HeapValue {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, _: &T) -> Result<(), fmt::Error> {
        // TODO: fields (may point back to self!)
        f.debug_struct("HeapValue").finish()
    }
}

impl DynamicDebug for DynHeapValue {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("DynHeapValue { base: ")?;
        self.base.fmt(f, type_reg)?;
        write!(f, ", dyn_len: {:?} }}", self.dyn_len)
    }
}

pub struct ObjRefs {
    ptr: *mut ValueRef,
    end: *mut ValueRef
}

impl Iterator for ObjRefs {
    type Item = *mut ValueRef;

    fn next(&mut self) -> Option<*mut ValueRef> {
        if self.ptr < self.end {
            let res = Some(self.ptr);
            self.ptr = unsafe { self.ptr.offset(1) };
            res
        } else {
            None
        }
    }
}

// ================================================================================================

/// A dynamic type.
#[repr(C)]
pub struct Type {
    heap_value: HeapValue,
    gsize_with_dyn: usize,
    ref_len_with_dyn: usize
}

impl Type {
    fn new(heap_value: HeapValue, has_dyn_gsize: bool, gsize: GSize, has_dyn_ref_len: bool,
           ref_len: usize) -> Type {
        Type {
            heap_value,
            gsize_with_dyn: usize::from(gsize) << 1 | has_dyn_gsize as usize,
            ref_len_with_dyn: ref_len << 1 | has_dyn_ref_len as usize
        }
    }

    fn uniform_gsize(&self) -> usize { self.gsize_with_dyn >> 1 }

    fn has_dyn_gsize(&self) -> bool { self.gsize_with_dyn & 0b1 == 1 }

    fn uniform_ref_len(&self) -> usize { self.ref_len_with_dyn >> 1 }

    fn has_dyn_ref_len(&self) -> bool { self.ref_len_with_dyn & 0b1 == 1 }
}

impl IndexedType for Type {
    const TYPE_INDEX: TypeIndex = TypeIndex::Type;
}

impl DynamicDebug for Type {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Type { heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        write!(f, ", gsize: {:?}", self.gsize_with_dyn)?;
        write!(f, ", ref_len: {:?} }}", self.ref_len_with_dyn)
    }
}

/// Symbol (hash-consed string)
pub struct Symbol {
    base: DynHeapValue
}

impl Symbol {
    fn chars(&self) -> &str {
        let ptr = self as *const Symbol;
        unsafe { str::from_utf8_unchecked(slice::from_raw_parts(
            ptr.offset(1) as *const u8,
            (*(ptr as *const DynHeapValue)).dyn_len
        )) }
    }
}

impl IndexedType for Symbol {
    const TYPE_INDEX: TypeIndex = TypeIndex::Symbol;
}

impl DynamicDebug for Symbol {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Symbol { base: ")?;
        self.base.fmt(f, type_reg)?;
        write!(f, ", chars: {:?} }}", self.chars())
    }
}

/// Function AST node
pub struct Function {
    base: DynHeapValue
}

impl IndexedType for Function {
    const TYPE_INDEX: TypeIndex = TypeIndex::Function;
}

impl Function {
    fn methods(&self) -> &[TypedValueRef<Method>] {
        let ptr = self as *const Function;
        unsafe { slice::from_raw_parts(
            ptr.offset(1) as *const TypedValueRef<Method>,
            (*(ptr as *const DynHeapValue)).dyn_len
        ) }
    }
}

impl DynamicDebug for Function {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Function { base: ")?;
        self.base.fmt(f, type_reg)?;
        write!(f, ", methods: {:?}", self.methods().iter()
                                         .map(|vref| vref.fmt_wrap(type_reg))
                                         .collect::<Vec<_>>())?;
        f.write_str(" }")
    }
}

/// Method AST (function sub)node
pub struct Method {
    base: HeapValue,
    pattern: ValueRef,
    guard: ValueRef,
    body: ValueRef
}

impl IndexedType for Method {
    const TYPE_INDEX: TypeIndex = TypeIndex::Method;
}

impl DynamicDebug for Method {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Method { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", pattern: ")?;
        self.pattern.fmt(f, type_reg)?;
        f.write_str(", guard: ")?;
        self.guard.fmt(f, type_reg)?;
        f.write_str(", body: ")?;
        self.body.fmt(f, type_reg)?;
        f.write_str(" }")
    }
}

/// Block AST node
pub struct Block {
    base: DynHeapValue,
    expr: ValueRef,
}

impl Block {
    pub fn expr(&self) -> ValueRef { self.expr }

    pub fn stmts(&self) -> &[ValueRef] {
        let ptr = self as *const Block;
        unsafe { slice::from_raw_parts(
            ptr.offset(1) as *const ValueRef,
            (*(ptr as *const DynHeapValue)).dyn_len
        ) }
    }

    pub fn lex_binders(&self) -> iter::Empty<TypedValueRef<Symbol>> { iter::empty() } // FIXME

    pub fn dyn_binders(&self) -> iter::Empty<TypedValueRef<Symbol>> { iter::empty() } // FIXME
}

impl IndexedType for Block {
    const TYPE_INDEX: TypeIndex = TypeIndex::Block;
}

impl DynamicDebug for Block {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Block { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", expr: ")?;
        self.expr.fmt(f, type_reg)?;
        write!(f, ", stmts: {:?}", self.stmts().iter()
                                       .map(|vref| vref.fmt_wrap(type_reg))
                                       .collect::<Vec<_>>())?;
        f.write_str(" }")
    }
}

/// Call AST node
pub struct Call {
    base: DynHeapValue,
    callee: ValueRef,
}

impl Call {
    fn args(&self) -> &[ValueRef] {
        let ptr = self as *const Call;
        unsafe { slice::from_raw_parts(
            ptr.offset(1) as *const ValueRef,
            (*(ptr as *const DynHeapValue)).dyn_len
        ) }
    }
}

impl IndexedType for Call {
    const TYPE_INDEX: TypeIndex = TypeIndex::Call;
}

impl DynamicDebug for Call {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Call { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", callee: ")?;
        self.callee.fmt(f, type_reg)?;
        write!(f, ", args: {:?}", self.args().iter()
                                      .map(|vref| vref.fmt_wrap(type_reg))
                                      .collect::<Vec<_>>())?;
        f.write_str(" }")
    }
}

/// An AST node for definitions.
#[repr(C)]
pub struct Def {
    base: HeapValue,
    pattern: ValueRef,
    expr: ValueRef
}

impl Def {
    pub fn pattern(&self) -> ValueRef { self.pattern }

    pub fn expr(&self) -> ValueRef { self.expr }
}

impl IndexedType for Def {
    const TYPE_INDEX: TypeIndex = TypeIndex::Const;
}

impl DynamicDebug for Def {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Def { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", pattern: ")?;
        self.pattern.fmt(f, type_reg)?;
        f.write_str(", expr: ")?;
        self.expr.fmt(f, type_reg)?;
        f.write_str(" }")
    }
}

/// An AST node for constants.
#[repr(C)]
pub struct Const {
    heap_value: HeapValue,
    /// The value of the constant
    value: ValueRef
}

impl Const {
    pub fn value(&self) -> ValueRef { self.value }
}

impl IndexedType for Const {
    const TYPE_INDEX: TypeIndex = TypeIndex::Const;
}

impl DynamicDebug for Const {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Const { heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        f.write_str(", value: ")?;
        self.value.fmt(f, type_reg)?;
        f.write_str(" }")
    }
}

/// An AST node for lexical variable names
#[repr(C)]
pub struct Lex {
    base: HeapValue,
    name: TypedValueRef<Symbol>
}

impl IndexedType for Lex {
    const TYPE_INDEX: TypeIndex = TypeIndex::Lex;
}

impl DynamicDebug for Lex {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Lex { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", name: ")?;
        self.name.fmt(f, type_reg)?;
        f.write_str(" }")
    }
}

/// Block continuation
#[repr(C)]
pub struct BlockCont {
    base: HeapValue,
    parent: ValueRef,
    lenv: ValueRef,
    denv: ValueRef,
    block: TypedValueRef<Block>,
    index: ValueRef
}

impl BlockCont {
    pub fn parent(&self) -> ValueRef { self.parent }

    pub fn lenv(&self) -> ValueRef { self.lenv }

    pub fn denv(&self) -> ValueRef { self.denv }

    pub fn block(&self) -> TypedValueRef<Block> { self.block }

    pub fn index(&self) -> usize { unsafe { transmute::<_, usize>(self.index) >> 3 } } // FIXME
}

impl IndexedType for BlockCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::BlockCont;
}

impl DynamicDebug for BlockCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("BlockCont { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", parent: ")?;
        self.parent.fmt(f, type_reg)?;
        f.write_str(", lenv: ")?;
        self.lenv.fmt(f, type_reg)?;
        f.write_str(", denv: ")?;
        self.denv.fmt(f, type_reg)?;
        f.write_str(", block: ")?;
        self.block.fmt(f, type_reg)?;
        f.write_str(", index: ")?;
        self.index.fmt(f, type_reg)?;
        f.write_str(" }")
    }
}

/// Halt continuation
#[repr(C)]
pub struct Halt {
    base: HeapValue
}

impl IndexedType for Halt {
    const TYPE_INDEX: TypeIndex = TypeIndex::Halt;
}

impl DynamicDebug for Halt {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Halt { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(" }")
    }
}

/// Environment
#[repr(C)]
pub struct Env {
    base: DynHeapValue,
    parent: ValueRef
}

impl IndexedType for Env {
    const TYPE_INDEX: TypeIndex = TypeIndex::Env;
}

impl DynamicDebug for Env {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Env { base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", parent: ")?;
        self.parent.fmt(f, type_reg)?;
        f.write_str(" }")
    }
}

// ================================================================================================

#[derive(Debug)]
pub struct OutOfMemory;

/// Memory manager and value factory.
pub struct ValueManager {
    gc: Generation<ValueRef>,
    // OPTIMIZE: make the key just point inside the value
    symbol_table: HashMap<String, TypedValueRef<Symbol>>,
    types: HashMap<TypeIndex, TypedValueRef<Type>>,
    type_idxs: HashMap<TypedValueRef<Type>, TypeIndex>
}

impl ValueManager {
    /// Create a new `ValueManager` with a maximum heap size of `max_heap`.
    pub fn new(max_heap: usize) -> ValueManager {
        let mut res = ValueManager {
            gc: Generation::new(max_heap),
            symbol_table: HashMap::new(),
            types: HashMap::new(),
            type_idxs: HashMap::new()
        };

        let type_type: Initializable<Type> = unsafe { res.allocate_t() }.unwrap();
        res.insert(TypeIndex::Type, TypedValueRef::new(unsafe { transmute(type_type) }));
        res.uniform_init(type_type, |heap_value|
            Type::new(heap_value, false, GSize::of::<Type>(), false, 0)
        );

        let symbol_type = res.create_type(true, GSize::of::<Symbol>(), false, 0).unwrap();
        res.insert(TypeIndex::Symbol, symbol_type);

        let func_type = res.create_type(true, GSize::of::<Function>(), true, 0).unwrap();
        res.insert(TypeIndex::Function, func_type);
        let method_type = res.create_type(false, GSize::of::<Method>(), false, 3).unwrap();
        res.insert(TypeIndex::Method, method_type);
        let block_type = res.create_type(true, GSize::of::<Block>(), true, 1).unwrap();
        res.insert(TypeIndex::Block, block_type);
        let call_type = res.create_type(true, GSize::of::<Call>(), true, 1).unwrap();
        res.insert(TypeIndex::Call, call_type);
        let def_type = res.create_type(false, GSize::of::<Def>(), false, 2).unwrap();
        res.insert(TypeIndex::Def, def_type);
        let const_type = res.create_type(false, GSize::of::<Const>(), false, 1).unwrap();
        res.insert(TypeIndex::Const, const_type);
        let lex_type = res.create_type(false, GSize::of::<Lex>(), false, 1).unwrap();
        res.insert(TypeIndex::Lex, lex_type);

        let block_cont_type = res.create_type(false, GSize::of::<BlockCont>(), false, 5).unwrap();
        res.insert(TypeIndex::BlockCont, block_cont_type);
        let halt_type = res.create_type(false, GSize::of::<Halt>(), false, 0).unwrap();
        res.insert(TypeIndex::Halt, halt_type);

        let env_type = res.create_type(true, GSize::of::<Env>(), true, 1).unwrap();
        res.insert(TypeIndex::Env, env_type);

        res
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
                // TODO: mark roots in type and symbol tables within `self`.
                self.gc.collect();
            }
            create(self)
        })
        .ok_or(OutOfMemory)
    }

    fn init<T, F>(&self, ptr: Initializable<T>, f: F) -> TypedValueRef<T>
        where T: IndexedType, F: Fn(Unique<T>, HeapValue)
    {
        let uptr = start_init(ptr);
        let tvref = TypedValueRef::new(uptr);
        f(uptr, HeapValue {
            link: ValueRef::from(tvref),
            typ: self.get(T::TYPE_INDEX)
        });
        tvref
    }

    /// Initialize a `T`, delegating to `f` for everything but the `HeapValue` part.
    fn uniform_init<T, F>(&self, ptr: Initializable<T>, f: F) -> TypedValueRef<T>
        where T: IndexedType, F: Fn(HeapValue) -> T
    {
        self.init(ptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(heap_value);
        })
    }

    fn init_with_slice<T, F, E>(&self, iptr: Initializable<T>, f: F, slice: &[E])
        -> TypedValueRef<T>
        where T: IndexedType, F: Fn(DynHeapValue) -> T, E: Copy
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

    /// Allocate a `T` with a granule alignment of 1.
    unsafe fn allocate_t<T>(&mut self) -> Option<Initializable<T>> {
        self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                         NonZero::new_unchecked(GSize::of::<T>()))
    }

    /// Allocate and Initialize a `T` with a granule alignment of 1, delegating to `f` for
    /// everything but the `HeapValue` part.
    fn uniform_create<T, F>(&mut self, f: F) -> Option<TypedValueRef<T>>
        where T: IndexedType, F: Fn(HeapValue) -> T
    {
        unsafe { self.allocate_t() }.map(|typ| self.uniform_init(typ, f))
    }

    fn create_with_slice<T, F, E>(&mut self, gsize: GSize, f: F, slice: &[E])
        -> Option<TypedValueRef<T>>
        where T: IndexedType, F: Fn(DynHeapValue) -> T, E: Copy
    {
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(gsize)) }
            .map(|iptr| self.init_with_slice(iptr, f, slice))
    }

    fn create_with_vref_slice<T, F, E>(&mut self, f: F, slice: &[E]) -> Option<TypedValueRef<T>>
        where T: IndexedType, F: Fn(DynHeapValue) -> T, E: Copy + Into<ValueRef>
    {
        self.create_with_slice(GSize::of::<T>() + GSize::from(slice.len()), f, slice)
    }

    /// Create a new `Int` from `n`.
    pub fn create_int(&self, n: isize) -> ValueRef { ValueRef::from(n) }

    /// Create a new `Float` from `n`.
    pub fn create_float(&self, n: f64) -> ValueRef { ValueRef::from(n) }

    /// Create a new `Char` from `c`.
    pub fn create_char(&self, c: char) -> ValueRef { ValueRef::from(c) }

    /// Create a new `Bool` from `b`.
    pub fn create_bool(&self, b: bool) -> ValueRef { ValueRef::from(b) }

    /// Create a new dynamic type whose instances have a (byte) size of `size` and `ref_len`
    /// potentially pointer-valued fields.
    pub fn create_type(&mut self, has_dyn_gsize: bool, gsize: GSize, has_dyn_ref_len: bool,
                       ref_len: usize) -> Option<TypedValueRef<Type>>
    {
        self.uniform_create(|heap_value|
            Type::new(heap_value, has_dyn_gsize, gsize, has_dyn_ref_len, ref_len)
        )
    }

    /// Create a new `Symbol` from `chars`.
    pub fn create_symbol(&mut self, chars: &str) -> Option<TypedValueRef<Symbol>> {
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

    /// Create a new `Function` node with `methods`.
    pub fn create_function(&mut self, methods: &[TypedValueRef<Method>])
        -> Option<TypedValueRef<Function>>
    {
        self.create_with_vref_slice(|base| Function { base }, methods)
    }

    /// Create a new `Method` node.
    pub fn create_method(&mut self, pattern: ValueRef, guard: ValueRef, body: ValueRef)
        -> Option<TypedValueRef<Method>>
    {
        self.uniform_create(|base| Method { base, pattern, guard, body })
    }

    /// Create a new `Block` node from `stmts` and `expr`.
    pub fn create_block(&mut self, stmts: &[ValueRef], expr: ValueRef)
        -> Option<TypedValueRef<Block>>
    {
        self.create_with_vref_slice(|base| Block { base, expr }, stmts)
    }

    /// Create a new `Call` node from `callee` and `args`.
    pub fn create_def(&mut self, pattern: ValueRef, expr: ValueRef) -> Option<TypedValueRef<Def>> {
        self.uniform_create(|base| Def { base, pattern, expr })
    }

    /// Create a new `Call` node from `callee` and `args`.
    pub fn create_call(&mut self, callee: ValueRef, args: &[ValueRef])
        -> Option<TypedValueRef<Call>>
    {
        self.create_with_vref_slice(|base| Call { base, callee }, args)
    }

    /// Create a new `Const` node of `value`.
    pub fn create_const(&mut self, value: ValueRef) -> Option<TypedValueRef<Const>> {
        self.uniform_create(|heap_value| Const { heap_value, value })
    }

    /// Create a new `Lex` node for the variable named `name`.
    pub fn create_lex(&mut self, name: TypedValueRef<Symbol>) -> Option<TypedValueRef<Lex>> {
        self.uniform_create(|base| Lex { base, name })
    }

    /// Create a new block continuation
    pub fn create_block_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                             block: TypedValueRef<Block>, index: usize)
        -> Option<TypedValueRef<BlockCont>>
    {
        self.uniform_create(|base| BlockCont {
            base, parent, lenv, denv, block,
            index: (index as isize).into()
        })
    }

    /// Create a new halt continuation.
    pub fn create_halt(&mut self) -> Option<TypedValueRef<Halt>> {
        self.uniform_create(|base| Halt { base })
    }

    /// Create a new block `Env` that inherits from (= represents inner scope of) `Env`.
    pub fn create_block_env<I>(&mut self, parent: ValueRef, names: I)
        -> Option<TypedValueRef<Env>>
        where I: Iterator<Item=TypedValueRef<Symbol>>
    {
        let empty_dummy: [ValueRef; 0] = [];
        self.create_with_vref_slice(|base| Env { base, parent }, &empty_dummy)
    }
}

impl TypeRegistry for ValueManager {
    fn insert(&mut self, index: TypeIndex, typ: TypedValueRef<Type>) {
        self.types.insert(index, typ);
        self.type_idxs.insert(typ, index);
    }

    fn get(&self, index: TypeIndex) -> TypedValueRef<Type> {
        *self.types.get(&index).expect(&format!("No type found for {:?}", index))
    }

    fn index_of(&self, typ: TypedValueRef<Type>) -> TypeIndex {
        *self.type_idxs.get(&typ).unwrap()
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use gce::layout::GSize;
    use super::*;

    #[test]
    fn sizes() {
        assert_eq!(GSize::of::<HeapValue>(), GSize::from(2));
        assert_eq!(GSize::of::<DynHeapValue>(), GSize::from(3));

        assert_eq!(GSize::of::<Type>(), GSize::from(4));
        assert_eq!(GSize::of::<Symbol>(), GSize::from(3));

        assert_eq!(GSize::of::<Function>(), GSize::from(3));
        assert_eq!(GSize::of::<Method>(), GSize::from(5));
        assert_eq!(GSize::of::<Block>(), GSize::from(4));
        assert_eq!(GSize::of::<Call>(), GSize::from(4));
        assert_eq!(GSize::of::<Lex>(), GSize::from(3));
        assert_eq!(GSize::of::<Const>(), GSize::from(3));

        assert_eq!(GSize::of::<BlockCont>(), GSize::from(7));
        assert_eq!(GSize::of::<Halt>(), GSize::from(2));

        assert_eq!(GSize::of::<Env>(), GSize::from(4));
    }

    #[test]
    fn create() {
        let mut factory = ValueManager::new(1024);

        let typ = factory.create_type(false, GSize::from(2), false, 2).unwrap();
        let sym = factory.create_symbol(&"foo").unwrap();

        let call = factory.create_call(From::from(sym), &[From::from(typ), From::from(sym)])
                          .unwrap();
        let lex = factory.create_lex(sym).unwrap();
        let c = factory.create_const(From::from(typ)).unwrap();
        let block = factory.create_block(&[From::from(call), From::from(lex)], From::from(c))
                           .unwrap();
        let method = factory.create_method(From::from(sym), From::from(sym), From::from(block))
                            .unwrap();
        factory.create_function(&[method]).unwrap();

        factory.create_halt().unwrap();

        // TODO: factory.create_block_env(...);
    }
}
