use std::mem::transmute;

use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::Allocator;
use ast::{Block, Call, Def, Const};
use env::Env;
use continuation::{BlockCont, CalleeCont, Halt};

// ================================================================================================

trait AsRoot {
    fn as_root(self) -> Option<ValueRef>;
}

impl AsRoot for ValueRef {
    fn as_root(self) -> Option<ValueRef> { Some(self) }
}

impl<T> AsRoot for ValueRefT<T> {
    fn as_root(self) -> Option<ValueRef> { Some(self.into()) }
}

impl<T> AsRoot for Option<ValueRefT<T>> {
    fn as_root(self) -> Option<ValueRef> { self.map(ValueRef::from) }
}

macro_rules! allocate {
    ($f:path, ($($args:expr),*), { $($live_in:ident),* } ) => {{
        let mut heap = Allocator::instance_mut();
        if let Some(v) = $f(&mut*heap, $($args),*) {
            Ok(v)
        } else {
            unsafe {
                $($live_in = transmute(heap.mark_ref($live_in.as_root()));)*
                heap.collect_garbage();
            }
            $f(&mut*heap, $($args),*).ok_or(EvalError::OOM)
        }
    }}
}

// ================================================================================================

#[derive(Debug)]
pub enum EvalError {
    OOM
}

pub type EvalResult<T> = Result<T, EvalError>;

pub fn interpret(mut program: ValueRef) -> EvalResult<ValueRef> {
    let cont = allocate!(Halt::new, (), { program })?.into();
    let mut state = State::Eval {
        expr: program,
        lenv: None,
        denv: None,
        cont
    };

    loop { // trampoline
        state = match state {
            State::Eval { expr, lenv, denv, cont } => eval(expr, lenv, denv, cont)?,
            State::Exec { stmt, lenv, denv, cont } => exec(stmt, lenv, denv, cont)?,
            State::Continue { value, cont } => invoke(value, cont)?,
            State::Halt(value) => return Ok(value)
        };
    }
}

// ================================================================================================

enum State {
    Eval {
        expr: ValueRef,
        lenv: Option<ValueRefT<Env>>,
        denv: Option<ValueRefT<Env>>,
        cont: ValueRef
    },
    Exec {
        stmt: ValueRef,
        lenv: Option<ValueRefT<Env>>,
        denv: Option<ValueRefT<Env>>,
        cont: ValueRef
    },
    Continue {
        value: ValueRef,
        cont: ValueRef
    },
    Halt(ValueRef)
}

fn eval(expr: ValueRef, mut lenv: Option<ValueRefT<Env>>, mut denv: Option<ValueRefT<Env>>,
        mut cont: ValueRef) -> EvalResult<State>
{
    typecase!(expr, {
        mut block: Block =>
            if block.stmts().len() > 0 {
                let i = 0;
                let cont = allocate!(BlockCont::new, (cont, lenv, denv, block, i), {
                    block, lenv, denv, cont
                })?.into();
                Ok(State::Exec { stmt: block.stmts()[i], lenv, denv, cont })
            } else {
                Ok(State::Eval { expr: block.expr(), lenv, denv, cont })
            },
        mut call: Call => {
            let cont = allocate!(CalleeCont::new, (cont, call), {call, lenv, denv, cont})?;
            Ok(State::Eval { expr: call.callee(), lenv, denv, cont: cont.into() })
        },
        c: Const => Ok(State::Continue { value: c.value(), cont }),
        _ => unimplemented!()
    })
}

fn exec(stmt: ValueRef, lenv: Option<ValueRefT<Env>>, denv: Option<ValueRefT<Env>>,
        cont: ValueRef) -> EvalResult<State>
{
    typecase!(stmt, {
        def: Def => unimplemented!(),
        _ => eval(stmt, lenv, denv, cont)
    })
}

fn invoke(value: ValueRef, cont: ValueRef) -> EvalResult<State> {
    typecase!(cont, {
        bcont: BlockCont => {
            let mut parent = bcont.parent();
            let mut lenv = bcont.lenv();
            let mut denv = bcont.denv();
            let mut block = bcont.block();
            let index = bcont.index() + 1;
            if let Some(mut stmt) = block.stmts().get(index).map(|&v| v) {
                let cont = allocate!(BlockCont::new, (parent, lenv, denv, block, index), {
                    parent, lenv, denv, block, stmt
                })?.into();
                Ok(State::Exec { stmt, lenv, denv, cont })
            } else {
                Ok(State::Eval { expr: block.expr(), lenv, denv, cont: parent })
            }
        },
        Halt => Ok(State::Halt(value)),
        _ => unimplemented!()
    })
}
