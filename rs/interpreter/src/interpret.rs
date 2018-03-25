use std::mem::transmute;

use pcws_domain::object_model::{HeapValueSub, ValueRef, ValueRefT};
use pcws_domain::Allocator;
use ast::{Call, Def, Const};
use env::Env;
use continuation::{CalleeCont, Halt};

// ================================================================================================

#[derive(Debug)]
pub enum EvalError {
    OOM
}

pub type EvalResult<T> = Result<T, EvalError>;

pub fn interpret(mut program: ValueRef) -> EvalResult<ValueRef> {
    let cont = safepoint(Halt::new, &mut [program.as_mut()])?.into();
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
        mut call: Call => {
            let cont = safepoint(move |heap| CalleeCont::new(heap, cont.into(), call),
                                 &mut [call.as_mut(), as_mut(&mut lenv), as_mut(&mut denv),
                                       cont.as_mut()])?;
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
        Halt => Ok(State::Halt(value)),
        _ => unimplemented!()
    })
}

// ================================================================================================

fn safepoint<T, F>(f: F, roots: &mut [&mut Option<ValueRef>]) -> EvalResult<T>
    where F: Fn(&mut Allocator) -> Option<T>
{
    Allocator::safepoint(f, roots).ok_or(EvalError::OOM)
}

fn as_mut<T: HeapValueSub>(vref: &mut Option<ValueRefT<T>>) -> &mut Option<ValueRef> {
    unsafe { transmute(vref) }
}
