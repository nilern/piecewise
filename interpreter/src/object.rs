use core::nonzero::NonZero;
use std::mem::{size_of, transmute};
use std::fmt::{self, Debug, Formatter};
use std::slice;
use std::collections::HashMap;
use std::ptr::Unique;

use gce::util::{start_init, Initializable, CeilDiv};
use gce::Object;
use gce::layout::{Granule, GSize};
use gce::mark_n_sweep::Generation;
use value_refs::{ValueRef, TypedValueRef};

// ================================================================================================

/// Like `std::fmt::Debug`, but needs a `TypeRegistry` because of the dynamic typing.
pub trait DynamicDebug {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error>;
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
    Lex,
    Const
}

/// Obtain the `TypeIndex` of `Self`.
trait IndexedType {
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
    Const(TypedValueRef<Const>),
    Symbol(TypedValueRef<Symbol>),

    Function(TypedValueRef<Function>),
    Method(TypedValueRef<Method>),
    Block(TypedValueRef<Block>),
    Call(TypedValueRef<Call>),
    Lex(TypedValueRef<Lex>),

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
            &Const(vref) => vref.fmt(f, type_reg),
            &Symbol(vref) => vref.fmt(f, type_reg),

            &Function(vref) => vref.fmt(f, type_reg),
            &Method(vref) => vref.fmt(f, type_reg),
            &Block(vref) => vref.fmt(f, type_reg),
            &Call(vref) => vref.fmt(f, type_reg),
            &Lex(vref) => vref.fmt(f, type_reg),

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
        f.write_str("Dyn {{ base: ")?;
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
            gsize_with_dyn: usize::from(gsize) << 1 | if has_dyn_gsize { 1 } else { 0 },
            ref_len_with_dyn: ref_len << 1 | if has_dyn_ref_len { 1 } else { 0 }
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
        f.write_str("Type {{ heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        write!(f, ", gsize: {:?}", self.gsize_with_dyn)?;
        write!(f, ", ref_len: {:?} }}", self.ref_len_with_dyn)
    }
}

/// Symbol (hash-consed string)
pub struct Symbol {
    base: DynHeapValue
}

impl IndexedType for Symbol {
    const TYPE_INDEX: TypeIndex = TypeIndex::Symbol;
}

impl DynamicDebug for Symbol {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Symbol {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        // TODO: chars
        f.write_str(" }}")
    }
}

/// Function AST node
pub struct Function {
    base: DynHeapValue
}

impl IndexedType for Function {
    const TYPE_INDEX: TypeIndex = TypeIndex::Function;
}

impl DynamicDebug for Function {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Function {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        // TODO: methods
        f.write_str(" }}")
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
        f.write_str("Method {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", pattern: ")?;
        self.pattern.fmt(f, type_reg)?;
        f.write_str(", guard: ")?;
        self.guard.fmt(f, type_reg)?;
        f.write_str(", body: ")?;
        self.body.fmt(f, type_reg)?;
        f.write_str(" }}")
    }
}

/// Block AST node
pub struct Block {
    base: DynHeapValue,
    expr: ValueRef,
}

impl IndexedType for Block {
    const TYPE_INDEX: TypeIndex = TypeIndex::Block;
}

impl DynamicDebug for Block {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Block {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", expr: ")?;
        self.expr.fmt(f, type_reg)?;
        // TODO: stmts
        f.write_str(" }}")
    }
}

/// Call AST node
pub struct Call {
    base: DynHeapValue,
    callee: ValueRef,
}

impl IndexedType for Call {
    const TYPE_INDEX: TypeIndex = TypeIndex::Call;
}

impl DynamicDebug for Call {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Call {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", callee: ")?;
        self.callee.fmt(f, type_reg)?;
        // TODO: args
        f.write_str(" }}")
    }
}

/// An AST node for constants.
#[repr(C)]
pub struct Const {
    heap_value: HeapValue,
    /// The value of the constant
    value: ValueRef
}

impl IndexedType for Const {
    const TYPE_INDEX: TypeIndex = TypeIndex::Const;
}

impl DynamicDebug for Const {
    fn fmt<T: TypeRegistry>(&self, f: &mut Formatter, type_reg: &T) -> Result<(), fmt::Error> {
        f.write_str("Const {{ heap_value: ")?;
        self.heap_value.fmt(f, type_reg)?;
        f.write_str(", value: ")?;
        self.value.fmt(f, type_reg)?;
        f.write_str(" }}")
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
        f.write_str("Lex {{ base: ")?;
        self.base.fmt(f, type_reg)?;
        f.write_str(", name: ")?;
        self.name.fmt(f, type_reg)?;
        f.write_str(" }}")
    }
}

// ================================================================================================

/// Memory manager and value factory.
pub struct ValueManager {
    gc: Generation<ValueRef>,
    // OPTIMIZE: make the key just point inside the value
    symbol_table: HashMap<String, TypedValueRef<Symbol>>
}

impl ValueManager {
    /// Create a new `ValueManager` with a maximum heap size of `max_heap`.
    pub fn new<R: TypeRegistry>(types: &mut R, max_heap: usize) -> ValueManager {
        // TODO: allocate all the types and store them in `types` (?)
        let mut res = ValueManager {
            gc: Generation::new(max_heap),
            symbol_table: HashMap::new()
        };
        let type_type: Initializable<Type> = unsafe { res.allocate_t() }.unwrap();
        types.insert(TypeIndex::Type, TypedValueRef::new(unsafe { transmute(type_type) }));
        Self::uniform_init(type_type, types, |heap_value|
            Type::new(heap_value, false, GSize::of::<Type>(), false, 0));
        res
    }

    fn init<T, R, F>(ptr: Initializable<T>, type_reg: &R, f: F) -> TypedValueRef<T>
        where T: IndexedType, R: TypeRegistry, F: Fn(Unique<T>, HeapValue)
    {
        let mut uptr = start_init(ptr);
        let tvref = TypedValueRef::new(uptr);
        f(uptr, HeapValue {
            link: ValueRef::from(tvref),
            typ: type_reg.get(T::TYPE_INDEX)
        });
        tvref
    }

    /// Initialize a `T`, delegating to `f` for everything but the `HeapValue` part.
    fn uniform_init<T, R, F>(ptr: Initializable<T>, type_reg: &R, f: F) -> TypedValueRef<T>
        where T: IndexedType, R: TypeRegistry, F: Fn(HeapValue) -> T
    {
        Self::init(ptr, type_reg, |mut uptr, heap_value| {
            *unsafe { uptr.as_mut() } = f(heap_value);
        })
    }

    fn init_with_slice<T, R, F, E>(iptr: Initializable<T>, type_reg: &R, f: F, slice: &[E])
        -> TypedValueRef<T>
        where T: IndexedType, R: TypeRegistry, F: Fn(DynHeapValue) -> T, E: Copy
    {
        Self::init(iptr, type_reg, |mut uptr, heap_value| {
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
    fn uniform_create<T, R, F>(&mut self, type_reg: &R, f: F)
        -> Option<TypedValueRef<T>>
        where T: IndexedType, R: TypeRegistry, F: Fn(HeapValue) -> T
    {
        unsafe { self.allocate_t() }.map(|typ| Self::uniform_init(typ, type_reg, f))
    }

    fn create_with_slice<T, R, F, E>(&mut self, types: &R, gsize: GSize, f: F, slice: &[E])
        -> Option<TypedValueRef<T>>
        where T: IndexedType, R: TypeRegistry, F: Fn(DynHeapValue) -> T, E: Copy
    {
        unsafe { self.gc.allocate(NonZero::new_unchecked(GSize::from(1)),
                                  NonZero::new_unchecked(gsize)) }
            .map(|iptr| ValueManager::init_with_slice(iptr, types, f, slice))
    }

    fn create_with_vref_slice<T, R, F, E>(&mut self, types: &R, f: F, slice: &[E])
        -> Option<TypedValueRef<T>>
        where T: IndexedType, R: TypeRegistry, F: Fn(DynHeapValue) -> T, E: Copy + Into<ValueRef>
    {
        self.create_with_slice(types, GSize::of::<T>() + GSize::from(slice.len()), f, slice)
    }

    /// Create a new dynamic type whose instances have a (byte) size of `size` and `ref_len`
    /// potentially pointer-valued fields.
    pub fn create_type<R: TypeRegistry>(&mut self, types: &R,
                                        has_dyn_gsize: bool, gsize: GSize,
                                        has_dyn_ref_len: bool, ref_len: usize)
        -> Option<TypedValueRef<Type>>
    {
        self.uniform_create(types, |heap_value|
            Type::new(heap_value, has_dyn_gsize, gsize, has_dyn_ref_len, ref_len)
        )
    }

    /// Create a new `Symbol` from `chars`.
    pub fn create_symbol<R: TypeRegistry>(&mut self, types: &R, chars: &str)
        -> Option<TypedValueRef<Symbol>>
    {
        self.symbol_table
            .get(chars).map(|&sym| sym)
            .or_else(|| {
                let bytes = chars.as_bytes();
                let gsize = GSize::of::<Symbol>() + GSize::from_bytesize(bytes.len());
                let sym = self.create_with_slice(types, gsize, |base| Symbol { base }, bytes);
                if let Some(sym) = sym {
                    self.symbol_table.insert(chars.to_string(), sym);
                }
                sym
            })
    }

    /// Create a new `Function` node with `methods`.
    pub fn create_function<R: TypeRegistry>(&mut self, types: &R,
                                            methods: &[TypedValueRef<Method>])
        -> Option<TypedValueRef<Function>>
    {
        self.create_with_vref_slice(types, |base| Function { base }, methods)
    }

    /// Create a new `Method` node.
    pub fn create_method<R: TypeRegistry>(&mut self, types: &R, pattern: ValueRef, guard: ValueRef,
                                          body: ValueRef) -> Option<TypedValueRef<Method>>
    {
        self.uniform_create(types, |base| Method { base, pattern, guard, body })
    }

    /// Create a new `Block` node from `stmts` and `expr`.
    pub fn create_block<R: TypeRegistry>(&mut self, types: &R, stmts: &[ValueRef], expr: ValueRef)
        -> Option<TypedValueRef<Block>>
    {
        self.create_with_vref_slice(types, |base| Block { base, expr }, stmts)
    }

    /// Create a new `Call` node from `callee` and `args`.
    pub fn create_call<R: TypeRegistry>(&mut self, types: &R, callee: ValueRef, args: &[ValueRef])
        -> Option<TypedValueRef<Call>>
    {
        self.create_with_vref_slice(types, |base| Call { base, callee }, args)
    }

    /// Create a new `Const` node of `value`.
    pub fn create_const<R: TypeRegistry>(&mut self, types: &R, value: ValueRef)
        -> Option<TypedValueRef<Const>>
    {
        self.uniform_create(types, |heap_value| Const { heap_value, value })
    }

    /// Create a new `Lex` node for the variable named `name`.
    pub fn crate_lex<R: TypeRegistry>(&mut self, types: &R, name: TypedValueRef<Symbol>)
        -> Option<TypedValueRef<Lex>>
    {
        self.uniform_create(types, |base| Lex { base, name })
    }
}

// ================================================================================================

#[cfg(test)]
mod tests {
    use gce::layout::GSize;
    use super::{HeapValue, DynHeapValue, Type};

    #[test]
    fn heap_value_size() {
        assert_eq!(GSize::of::<HeapValue>(), GSize::from(2));
        assert_eq!(GSize::of::<DynHeapValue>(), GSize::from(3));
    }

    #[test]
    fn type_size() {
        assert_eq!(GSize::of::<Type>(), GSize::from(4));
    }
}
