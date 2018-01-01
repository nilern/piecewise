use std::fmt::{self, Debug, Formatter};
use std::str;

use gce::GSize;
use interpreter::Allocator;
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
    pub base: DynHeapValue
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
    pub fn new(allocator: &mut Allocator, chars: &str) -> Option<HeapValueRef<Symbol>> {
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
    pub base: HeapValue
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

#[cfg(test)]
mod tests {
    use super::*;

    use std::iter;

    use gce::GSize;
    use interpreter::Interpreter;

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
        let mut itp = Interpreter::new(1024);
        itp.with_gc_retry(|factory| {
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

            Some(()) // HACK
        }, &mut []).unwrap();
    }
}
