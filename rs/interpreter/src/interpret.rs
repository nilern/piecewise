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

pub fn interpret(mut program: ValueRef) -> EvalResult<ValueRef> {
    let cont = safepoint(Halt::new, &mut [&mut program])?.into();
    let mut state = State::Eval { expr: program, cont };

    loop { // trampoline
        state = match state {
            State::Eval { expr, cont } => eval(expr, cont)?,
            State::Exec { stmt, cont } => exec(stmt, cont)?,
            State::Continue { value, cont } => invoke(value, cont)?,
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

fn eval(expr: ValueRef, mut cont: ValueRef) -> EvalResult<State> {
    typecase!(expr, {
        mut call: Call => {
            let cont = safepoint(move |heap| CalleeCont::new(heap, cont.into(), call),
                                 &mut [call.as_mut(), &mut cont])?;
            Ok(State::Eval { expr: call.callee(), cont: cont.into() })
        },
        c: Const => Ok(State::Continue { value: c.value(), cont }),
        _ => unimplemented!()
    })
}

fn exec(stmt: ValueRef, cont: ValueRef) -> EvalResult<State> {
    typecase!(stmt, {
        def: Def => unimplemented!(),
        _ => eval(stmt, cont)
    })
}

fn invoke(value: ValueRef, cont: ValueRef) -> EvalResult<State> {
    typecase!(cont, {
        Halt => Ok(State::Halt(value)),
        _ => unimplemented!()
    })
}

fn safepoint<T, F>(f: F, roots: &mut [&mut ValueRef]) -> EvalResult<T>
    where F: Fn(&mut Allocator) -> Option<T>
{
    Allocator::safepoint(f, roots).ok_or(EvalError::OOM)
}
