use pcws_domain::object_model::ValueRef;
use pcws_domain::Allocator;

use ast::{Call, Def, Const};
use continuation::{CalleeCont, Halt};

// ================================================================================================

#[derive(Debug)]
pub enum EvalError {
    OOM
}

pub type EvalResult<T> = Result<T, EvalError>;

pub fn interpret(alloc: &mut Allocator, program: ValueRef) -> EvalResult<ValueRef> {
    let cont = Halt::new(alloc).unwrap().into();
    let mut state = State::Eval { expr: program, cont };

    loop { // trampoline
        state = match state {
            State::Eval { expr, cont } => eval(alloc, expr, cont)?,
            State::Exec { stmt, cont } => exec(alloc, stmt, cont)?,
            State::Continue { value, cont } => invoke(alloc, value, cont)?,
            State::Halt(value) => return Ok(value)
        };
    }
}

// ================================================================================================

enum State {
    Eval {
        expr: ValueRef,
        cont: ValueRef
    },
    Exec {
        stmt: ValueRef,
        cont: ValueRef
    },
    Continue {
        value: ValueRef,
        cont: ValueRef
    },
    Halt(ValueRef)
}

fn eval(alloc: &mut Allocator, expr: ValueRef, mut cont: ValueRef) -> EvalResult<State> {
    typecase!(expr, alloc, {
        mut call: Call => {
            let cont = safepointed(alloc, move |alloc| CalleeCont::new(alloc, cont.into(), call),
                                   &mut [call.as_mut(), &mut cont])?;
            Ok(State::Eval { expr: call.callee(), cont: cont.into() })
        },
        c: Const => Ok(State::Continue { value: c.value(), cont }),
        _ => unimplemented!()
    })
}

fn exec(alloc: &mut Allocator, stmt: ValueRef, cont: ValueRef) -> EvalResult<State> {
    typecase!(stmt, alloc, {
        def: Def => unimplemented!(),
        _ => eval(alloc, stmt, cont)
    })
}

fn invoke(alloc: &mut Allocator, value: ValueRef, cont: ValueRef) -> EvalResult<State> {
    typecase!(cont, alloc, {
        Halt => Ok(State::Halt(value)),
        _ => unimplemented!()
    })
}

fn safepointed<T, F>(alloc: &mut Allocator, f: F, roots: &mut [&mut ValueRef]) -> EvalResult<T>
    where F: Fn(&mut Allocator) -> Option<T>
{
    f(alloc).or_else(|| {
        unimplemented!();
        f(alloc)
    }).ok_or(EvalError::OOM)
}
