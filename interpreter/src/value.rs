use core::nonzero::NonZero;
use std::mem::transmute;
use std::fmt::{self, Debug, Formatter};
use std::slice;
use std::collections::HashMap;
use std::ptr::Unique;
use std::str;
use std::iter;
use std::ops::Deref;

use gce::util::{start_init, Initializable};
use gce::{Object, ObjectRef};
use gce::layout::GSize;
use gce::mark_n_sweep::Generation;
use value_refs::{ValueRef, TypedValueRef};

// ================================================================================================

pub trait HeapValueSub: Sized {
    const TYPE_INDEX: TypeIndex;

    const UNIFORM_REF_LEN: usize;
}

trait DynHeapValueSub: HeapValueSub {
    type TailItem;

    fn tail(&self) -> &[Self::TailItem] {
        unsafe {
            slice::from_raw_parts((self as *const Self).offset(1) as *const Self::TailItem,
                                  transmute::<_, &DynHeapValue>(self).dyn_len)
        }
    }
}

// ================================================================================================

/// Like `std::fmt::Debug`, but needs a `TypeRegistry` because of the dynamic typing.
pub trait DynamicDebug: Sized {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error>;

    fn fmt_wrap<'a, 'b, R: TypeRegistry>(&'a self, types: &'b R)
        -> DynDebugWrapper<'a, 'b, Self, R>
    {
        DynDebugWrapper {
            value: self,
            types: types
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

impl<'a, T> DynamicDebug for &'a [T] where T: DynamicDebug {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_list()
         .entries(self.iter().map(|entry| entry.fmt_wrap(types)))
         .finish()
    }
}

// ================================================================================================

pub struct Unbound(TypedValueRef<Symbol>);

impl DynamicDebug for Unbound {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_tuple("Unbound")
         .field(&self.0.fmt_wrap(types))
         .finish()
    }
}

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
    Tuple(TypedValueRef<Tuple>),
    Symbol(TypedValueRef<Symbol>),

    Promise(TypedValueRef<Promise>),

    Function(TypedValueRef<Function>),
    Method(TypedValueRef<Method>),
    Block(TypedValueRef<Block>),
    Call(TypedValueRef<Call>),
    Def(TypedValueRef<Def>),
    Lex(TypedValueRef<Lex>),
    Const(TypedValueRef<Const>),

    BlockCont(TypedValueRef<BlockCont>),
    DefCont(TypedValueRef<DefCont>),
    CalleeCont(TypedValueRef<CalleeCont>),
    ArgCont(TypedValueRef<ArgCont>),
    Halt(TypedValueRef<Halt>),

    Env(TypedValueRef<Env>),
    Closure(TypedValueRef<Closure>),

    Null,

    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
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

    pub fn force(&self) -> Option<ValueRef> {
        let mut ptr = self as *const HeapValue;
        let mut vref = ValueRef::from(ptr);

        loop {
            let link = unsafe { (*ptr).link };
            if link == vref {
                return Some(vref);
            } else if link == ValueRef::NULL {
                return None;
            } else {
                vref = link;
                if let Some(sptr) = vref.ptr() {
                    ptr = sptr.as_ptr();
                } else {
                    return Some(vref);
                }
            }
        }
    }

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
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        let self_ref = ValueRef::from(self as *const HeapValue);
        let mut dbg = f.debug_struct("HeapValue");

        if self.link == self_ref {
            dbg.field("link", &"#[cycle]");
        } else {
            dbg.field("link", &self.link.fmt_wrap(types));
        }

        if ValueRef::from(self.typ) == self_ref {
            dbg.field("typ", &"#[cycle]");
        } else {
            dbg.field("typ", &self.typ.fmt_wrap(types));
        }

        dbg.finish()
    }
}

impl DynamicDebug for DynHeapValue {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("DynHeapValue")
         .field("base", &self.base.fmt_wrap(types))
         .field("dyn_len", &self.dyn_len)
         .finish()
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

impl HeapValueSub for Type {
    const TYPE_INDEX: TypeIndex = TypeIndex::Type;

    const UNIFORM_REF_LEN: usize = 0;
}

impl DynamicDebug for Type {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Type")
         .field("heap_value", &self.heap_value.fmt_wrap(types))
         .field("gsize", &self.gsize_with_dyn)
         .field("ref_len", &self.ref_len_with_dyn)
         .finish()
    }
}

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
    fn chars(&self) -> &str {
        unsafe { str::from_utf8_unchecked(self.tail()) }
    }
}

impl HeapValueSub for Symbol {
    const TYPE_INDEX: TypeIndex = TypeIndex::Symbol;

    const UNIFORM_REF_LEN: usize = 0;
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

/// Function AST node
pub struct Function {
    base: DynHeapValue
}

impl Function {
    pub fn methods(&self) -> &[TypedValueRef<Method>] { self.tail() }
}

impl HeapValueSub for Function {
    const TYPE_INDEX: TypeIndex = TypeIndex::Function;

    const UNIFORM_REF_LEN: usize = 0;
}

impl DynHeapValueSub for Function {
    type TailItem = TypedValueRef<Method>;
}

impl DynamicDebug for Function {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        f.debug_struct("Function")
         .field("base", &self.base.fmt_wrap(types))
         .field("methods", &self.methods().fmt_wrap(types))
         .finish()
    }
}

/// Method AST (function sub)node
pub struct Method {
    base: HeapValue,
    pub pattern: ValueRef,
    guard: ValueRef,
    pub body: ValueRef
}

impl HeapValueSub for Method {
    const TYPE_INDEX: TypeIndex = TypeIndex::Method;

    const UNIFORM_REF_LEN: usize = 3;
}

impl DynamicDebug for Method {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Method")
         .field("base", &self.base.fmt_wrap(types))
         .field("pattern", &self.pattern.fmt_wrap(types))
         .field("guard", &self.guard.fmt_wrap(types))
         .field("body", &self.body.fmt_wrap(types))
         .finish()
    }
}

/// Block AST node
pub struct Block {
    base: DynHeapValue,
    expr: ValueRef,
}

fn stmt_binders(stmt: &ValueRef) -> iter::Empty<ValueRef> { iter::empty() } // FIXME

fn downcast_binder(vref: ValueRef) -> TypedValueRef<Symbol> { unsafe { vref.downcast() } }

impl Block {
    pub fn expr(&self) -> ValueRef { self.expr }

    pub fn stmts(&self) -> &[ValueRef] { self.tail() }
}

impl HeapValueSub for Block {
    const TYPE_INDEX: TypeIndex = TypeIndex::Block;

    const UNIFORM_REF_LEN: usize = 1;
}

impl DynHeapValueSub for Block {
    type TailItem = ValueRef;
}

impl DynamicDebug for Block {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Block")
         .field("base", &self.base.fmt_wrap(types))
         .field("expr", &self.expr.fmt_wrap(types))
         .field("stmts", &self.stmts().fmt_wrap(types))
         .finish()
    }
}

/// Call AST node
pub struct Call {
    base: DynHeapValue,
    pub callee: ValueRef,
}

impl Call {
    pub fn args(&self) -> &[ValueRef] { self.tail() }
}

impl HeapValueSub for Call {
    const TYPE_INDEX: TypeIndex = TypeIndex::Call;

    const UNIFORM_REF_LEN: usize = 1;
}

impl DynHeapValueSub for Call {
    type TailItem = ValueRef;
}

impl DynamicDebug for Call {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Call")
         .field("base", &self.base.fmt_wrap(types))
         .field("callee", &self.callee.fmt_wrap(types))
         .field("args", &self.args().fmt_wrap(types))
         .finish()
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

impl HeapValueSub for Def {
    const TYPE_INDEX: TypeIndex = TypeIndex::Def;

    const UNIFORM_REF_LEN: usize = 2;
}

impl DynamicDebug for Def {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Def")
         .field("base", &self.base.fmt_wrap(types))
         .field("pattern", &self.pattern.fmt_wrap(types))
         .field("expr", &self.expr.fmt_wrap(types))
         .finish()
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

impl HeapValueSub for Const {
    const TYPE_INDEX: TypeIndex = TypeIndex::Const;

    const UNIFORM_REF_LEN: usize = 1;
}

impl DynamicDebug for Const {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Const")
         .field("heap_value", &self.heap_value.fmt_wrap(types))
         .field("value", &self.value.fmt_wrap(types))
         .finish()
    }
}

/// An AST node for lexical variable names
#[repr(C)]
pub struct Lex {
    base: HeapValue,
    name: TypedValueRef<Symbol>
}

impl Lex {
    pub fn name(&self) -> TypedValueRef<Symbol> { self.name }
}

impl HeapValueSub for Lex {
    const TYPE_INDEX: TypeIndex = TypeIndex::Lex;

    const UNIFORM_REF_LEN: usize = 1;
}

impl DynamicDebug for Lex {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Lex")
         .field("base", &self.base.fmt_wrap(types))
         .field("name", &self.name.fmt_wrap(types))
         .finish()
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

impl HeapValueSub for BlockCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::BlockCont;

    const UNIFORM_REF_LEN: usize = 5;
}

impl DynamicDebug for BlockCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("BlockCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("block", &self.block.fmt_wrap(types))
         .field("index", &self.index.fmt_wrap(types))
         .finish()
    }
}

/// Assigning continuation
#[repr(C)]
pub struct DefCont {
    base: HeapValue,
    parent: ValueRef,
    lenv: ValueRef,
    denv: ValueRef,
    var: ValueRef
}

impl DefCont {
    pub fn parent(&self) -> ValueRef { self.parent }

    pub fn lenv(&self) -> ValueRef { self.lenv }

    pub fn denv(&self) -> ValueRef { self.denv }

    pub fn var(&self) -> ValueRef { self.var }
}

impl HeapValueSub for DefCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::DefCont;

    const UNIFORM_REF_LEN: usize = 4;
}

impl DynamicDebug for DefCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("DefCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("var", &self.var.fmt_wrap(types))
         .finish()
    }
}

/// Continuation expecting callee value
#[repr(C)]
pub struct CalleeCont {
    base: HeapValue,
    pub parent: ValueRef,
    pub lenv: ValueRef,
    pub denv: ValueRef,
    pub call: TypedValueRef<Call>
}

impl HeapValueSub for CalleeCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::CalleeCont;

    const UNIFORM_REF_LEN: usize = 4;
}

impl DynamicDebug for CalleeCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("CalleeCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("call", &self.call.fmt_wrap(types))
         .finish()
    }
}

/// Continuation expecting argument value
#[repr(C)]
pub struct ArgCont {
    base: DynHeapValue,
    pub parent: ValueRef,
    pub lenv: ValueRef,
    pub denv: ValueRef,
    pub call: TypedValueRef<Call>,
    index: ValueRef,
    pub callee: ValueRef
}

impl ArgCont {
    pub fn index(&self) -> usize { unsafe { transmute::<_, usize>(self) >> 3 } }

    pub fn args(&self) -> &[ValueRef] { self.tail() }
}

impl HeapValueSub for ArgCont {
    const TYPE_INDEX: TypeIndex = TypeIndex::ArgCont;

    const UNIFORM_REF_LEN: usize = 6;
}

impl DynHeapValueSub for ArgCont {
    type TailItem = ValueRef;
}

impl DynamicDebug for ArgCont {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("ArgCont")
         .field("base", &self.base.fmt_wrap(types))
         .field("parent", &self.parent.fmt_wrap(types))
         .field("lenv", &self.lenv.fmt_wrap(types))
         .field("denv", &self.denv.fmt_wrap(types))
         .field("call", &self.call.fmt_wrap(types))
         .field("index", &self.index.fmt_wrap(types))
         .field("callee", &self.callee.fmt_wrap(types))
         .field("args", &self.args().fmt_wrap(types))
         .finish()
    }
}

/// Halt continuation
#[repr(C)]
pub struct Halt {
    base: HeapValue
}

impl HeapValueSub for Halt {
    const TYPE_INDEX: TypeIndex = TypeIndex::Halt;

    const UNIFORM_REF_LEN: usize = 0;
}

impl DynamicDebug for Halt {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, types: &T) -> Result<(), fmt::Error> {
        f.debug_struct("Halt")
         .field("base", &self.base.fmt_wrap(types))
         .finish()
    }
}

/// Environment
#[repr(C)]
pub struct Env {
    base: DynHeapValue,
    parent: ValueRef
}

impl Env {
    fn parent<R: TypeRegistry>(&self, types: &R) -> Option<TypedValueRef<Env>> {
        if let ValueView::Env(parent) = self.parent.view(types) {
            Some(parent)
        } else {
            None
        }
    }

    pub fn get<R: TypeRegistry>(&self, name: TypedValueRef<Symbol>, types: &R)
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
    base: HeapValue,
    pub function: TypedValueRef<Function>,
    pub lenv: ValueRef
}

impl HeapValueSub for Closure {
    const TYPE_INDEX: TypeIndex = TypeIndex::Closure;

    const UNIFORM_REF_LEN: usize = 2;
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

        let tuple_type =
            res.create_type(true, GSize::of::<Tuple>(), true, Tuple::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Tuple, tuple_type);
        let symbol_type =
            res.create_type(true, GSize::of::<Symbol>(), false, Symbol::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Symbol, symbol_type);

        let promise_type =
            res.create_type(false, GSize::of::<Promise>(), false, Promise::UNIFORM_REF_LEN)
               .unwrap();
        res.insert(TypeIndex::Promise, promise_type);

        let func_type =
            res.create_type(true, GSize::of::<Function>(), true, Function::UNIFORM_REF_LEN)
               .unwrap();
        res.insert(TypeIndex::Function, func_type);
        let method_type =
            res.create_type(false, GSize::of::<Method>(), false, Method::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Method, method_type);
        let block_type =
            res.create_type(true, GSize::of::<Block>(), true, Block::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Block, block_type);
        let call_type =
            res.create_type(true, GSize::of::<Call>(), true, Call::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Call, call_type);
        let def_type =
            res.create_type(false, GSize::of::<Def>(), false, Def::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Def, def_type);
        let const_type =
            res.create_type(false, GSize::of::<Const>(), false, Const::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Const, const_type);
        let lex_type =
            res.create_type(false, GSize::of::<Lex>(), false, Lex::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Lex, lex_type);

        let block_cont_type =
            res.create_type(false, GSize::of::<BlockCont>(), false, BlockCont::UNIFORM_REF_LEN)
               .unwrap();
        res.insert(TypeIndex::BlockCont, block_cont_type);
        let def_cont_type =
            res.create_type(false, GSize::of::<DefCont>(), false, DefCont::UNIFORM_REF_LEN)
               .unwrap();
        res.insert(TypeIndex::DefCont, def_cont_type);
        let callee_cont_type =
            res.create_type(false, GSize::of::<CalleeCont>(), false, CalleeCont::UNIFORM_REF_LEN)
               .unwrap();
        res.insert(TypeIndex::CalleeCont, callee_cont_type);
        let arg_cont_type =
            res.create_type(true, GSize::of::<ArgCont>(), true, ArgCont::UNIFORM_REF_LEN)
               .unwrap();
        res.insert(TypeIndex::ArgCont, arg_cont_type);
        let halt_type =
            res.create_type(false, GSize::of::<Halt>(), false, Halt::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Halt, halt_type);

        let env_type =
            res.create_type(true, GSize::of::<Env>(), true, Env::UNIFORM_REF_LEN).unwrap();
        res.insert(TypeIndex::Env, env_type);
        let closure_type =
            res.create_type(false, GSize::of::<Closure>(), false, Closure::UNIFORM_REF_LEN)
               .unwrap();
        res.insert(TypeIndex::Closure, closure_type);

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
                // FIXME: mark roots in type and symbol tables within `self`.
                self.gc.collect();
            }
            create(self)
        })
        .ok_or(OutOfMemory)
    }

    fn init<T, F>(&self, ptr: Initializable<T>, f: F) -> TypedValueRef<T>
        where T: HeapValueSub, F: FnOnce(Unique<T>, HeapValue)
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
        where T: HeapValueSub, F: FnOnce(HeapValue) -> T
    {
        self.init(ptr, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(heap_value);
        })
    }

    fn init_with_slice<T, F, E>(&self, iptr: Initializable<T>, f: F, slice: &[E])
        -> TypedValueRef<T>
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
        -> TypedValueRef<T>
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
    fn uniform_create<T, F>(&mut self, f: F) -> Option<TypedValueRef<T>>
        where T: HeapValueSub, F: Fn(HeapValue) -> T
    {
        unsafe { self.allocate_t() }.map(|typ| self.uniform_init(typ, f))
    }

    fn create_with_slice<T, F, E>(&mut self, gsize: GSize, f: F, slice: &[E])
        -> Option<TypedValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy
    {
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(gsize)) }
            .map(|iptr| self.init_with_slice(iptr, f, slice))
    }

    fn create_with_vref_slice<T, F, E>(&mut self, f: F, slice: &[E]) -> Option<TypedValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T, E: Copy + Into<ValueRef>
    {
        self.create_with_slice(GSize::of::<T>() + GSize::from(slice.len()), f, slice)
    }

    fn create_with_vref_iter<T, F, I, E>(&mut self, f: F, len: usize, iter: I)
        -> Option<TypedValueRef<T>>
        where T: HeapValueSub, F: Fn(DynHeapValue) -> T,
              I: Iterator<Item=E>, E: Copy + Into<ValueRef>
    {
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(GSize::of::<T>() + GSize::from(len))) }
            .map(|iptr| self.init_with_iter(iptr, f, len, iter))
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

    /// Create a new Tuple.
    pub fn create_tuple<I>(&mut self, len: usize, values: I) -> Option<TypedValueRef<Tuple>>
        where I: Iterator<Item=ValueRef>
    {
        self.create_with_vref_iter(|base| Tuple { base }, len, values)
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

    /// Create an uninitialized `Promise`.
    pub fn create_promise(&mut self) -> Option<TypedValueRef<Promise>> {
        unsafe { self.allocate_t() }
            .map(|iptr| {
                let mut uptr = start_init(iptr);
                *unsafe { uptr.as_mut() } = Promise {
                    base: HeapValue {
                        link: ValueRef::NULL,
                        typ: self.get(TypeIndex::Promise)
                    }
                };
                TypedValueRef::new(uptr)
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

    /// Create a new assignment continuation
    pub fn create_def_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                           var: ValueRef)
        -> Option<TypedValueRef<DefCont>>
    {
        self.uniform_create(|base| DefCont { base, parent, lenv, denv, var })
    }

    /// Create new callee continuation
    pub fn create_callee_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                              call: TypedValueRef<Call>) -> Option<TypedValueRef<CalleeCont>>
    {
        self.uniform_create(|base| CalleeCont { base, parent, lenv, denv, call })
    }

    /// Create new argument continuation
    pub fn create_arg_cont(&mut self, parent: ValueRef, lenv: ValueRef, denv: ValueRef,
                           call: TypedValueRef<Call>, index: usize, callee: ValueRef,
                           args: &[ValueRef])
        -> Option<TypedValueRef<ArgCont>>
    {
        self.create_with_vref_slice(|base| ArgCont {
            base, parent, lenv, denv, call, index: (index as isize).into(), callee
        }, args)
    }

    /// Create a new halt continuation.
    pub fn create_halt(&mut self) -> Option<TypedValueRef<Halt>> {
        self.uniform_create(|base| Halt { base })
    }

    /// Create a new lexical block `Env` that inherits from (= represents inner scope of) `Env`.
    pub fn create_block_lenv(&mut self, parent: ValueRef, stmts: &[ValueRef])
        -> Option<TypedValueRef<Env>>
    {
        let mut bindings: Vec<ValueRef> = Vec::with_capacity(2*stmts.len());
        for stmt in stmts {
            if let ValueView::Def(def) = stmt.view(&*self) {
                if let ValueView::Lex(lvar) = def.pattern.view(&*self) {
                    bindings.push(lvar.name().into());
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
        -> Option<TypedValueRef<Env>>
    {
        let empty_dummy: [ValueRef; 0] = [];
        self.create_with_vref_slice(|base| Env { base, parent }, &empty_dummy)
    }

    /// Create a new lexical method `Env` that inherits from `parent`.
    pub fn create_method_lenv(&mut self, parent: ValueRef, param: TypedValueRef<Symbol>,
                              arg: ValueRef)
        -> Option<TypedValueRef<Env>>
    {
        self.create_with_vref_slice(|base| Env { base, parent }, &[param.into(), arg])
    }

    /// Close `function` over `lenv`.
    pub fn create_closure(&mut self, function: TypedValueRef<Function>, lenv: ValueRef)
        -> Option<TypedValueRef<Closure>>
    {
        self.uniform_create(|base| Closure { base, function, lenv })
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
        let mut factory = ValueManager::new(1024);

        let typ = factory.create_type(false, GSize::from(2), false, 2).unwrap();
        let tup = factory.create_tuple(1, iter::once(typ.into())).unwrap();
        let sym = factory.create_symbol(&"foo").unwrap();

        let promise = factory.create_promise().unwrap();

        let call = factory.create_call(From::from(sym), &[From::from(typ), From::from(sym)])
                          .unwrap();
        let lex = factory.create_lex(sym).unwrap().into();
        let c = factory.create_const(From::from(typ)).unwrap().into();
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
