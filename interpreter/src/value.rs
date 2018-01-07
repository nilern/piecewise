use std::fmt::{self, Debug, Formatter};

use domain::{TypeRegistry, DynamicDebug, ScalarValueRef, HeapValueRef,
             Type, Promise, Tuple, Symbol};
use ast::{Function, Method, Block, Def, Call, Const, Lex};
use continuations::{BlockCont, DefCont, CalleeCont, ArgCont, Halt};
use env::{Env, Closure};

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

impl ValueRef {
    /// Get the corresponding `ValueView`.
    pub fn view<T: TypeRegistry>(self, type_reg: &T) -> ValueView {
        if self == Self::NULL {
            ValueView::Null
        } else if let Some(sptr) = self.ptr() {
            match type_reg.index_of(unsafe { sptr.as_ref() }.typ) {
                TypeIndex::Type   => ValueView::Type(unsafe { self.downcast() }),
                TypeIndex::Tuple  => ValueView::Tuple(unsafe { self.downcast() }),
                TypeIndex::Symbol => ValueView::Symbol(unsafe { self.downcast() }),

                TypeIndex::Promise => ValueView::Promise(unsafe { self.downcast() }),

                TypeIndex::Function => ValueView::Function(unsafe { self.downcast() }),
                TypeIndex::Method   => ValueView::Method(unsafe { self.downcast() }),
                TypeIndex::Block    => ValueView::Block(unsafe { self.downcast() }),
                TypeIndex::Call     => ValueView::Call(unsafe { self.downcast() }),
                TypeIndex::Def      => ValueView::Def(unsafe { self.downcast() }),
                TypeIndex::Const    => ValueView::Const(unsafe { self.downcast() }),
                TypeIndex::Lex      => ValueView::Lex(unsafe { self.downcast() }),

                TypeIndex::BlockCont  => ValueView::BlockCont(unsafe { self.downcast() }),
                TypeIndex::DefCont    => ValueView::DefCont(unsafe { self.downcast() }),
                TypeIndex::CalleeCont => ValueView::CalleeCont(unsafe { self.downcast() }),
                TypeIndex::ArgCont    => ValueView::ArgCont(unsafe { self.downcast() }),
                TypeIndex::Halt       => ValueView::Halt(unsafe { self.downcast() }),

                TypeIndex::Env     => ValueView::Env(unsafe { self.downcast() }),
                TypeIndex::Closure => ValueView::Closure(unsafe { self.downcast() })
            }
        } else {
            match self.0 & TAG_MASK {
                0b000 => ValueView::Int(unsafe { transmute(self) }),
                0b010 => ValueView::Float(unsafe { transmute(self) }),
                0b100 => ValueView::Char(unsafe { transmute(self) }),
                0b110 => ValueView::Bool(unsafe { transmute(self) }),
                _ => unreachable!()
            }
        }
    }
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
