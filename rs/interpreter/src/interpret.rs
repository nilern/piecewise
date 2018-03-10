use pcws_domain::object_model::ValueRef;
use pcws_domain::Allocator;

use ast::Const;
use continuation::Halt;

// ================================================================================================

#[derive(Debug)]
pub enum EvalError {}

pub type EvalResult<T> = Result<T, EvalError>;

pub fn interpret(allocator: &mut Allocator, program: ValueRef) -> EvalResult<ValueRef> {
    let cont = Halt::new(allocator).unwrap().into();
    let mut state = State::Eval { alloc: allocator, expr: program, cont };

    loop { // trampoline
        state = match state {
            State::Eval { alloc, expr, cont } => eval(alloc, expr, cont)?,
            State::Continue { alloc, value, cont } => invoke(alloc, value, cont)?,
            State::Halt(value) => return Ok(value)
        };
    }
}

// ================================================================================================

enum State<'a> {
    Eval {
        alloc: &'a mut Allocator,
        expr: ValueRef,
        cont: ValueRef
    },
    Continue {
        alloc: &'a mut Allocator,
        value: ValueRef,
        cont: ValueRef
    },
    Halt(ValueRef)
}

fn eval(alloc: &mut Allocator, expr: ValueRef, cont: ValueRef) -> EvalResult<State> {
    typecase!(expr, alloc, {
        c: Const => Ok(State::Continue { alloc, value: c.value(), cont }),
        _ => unimplemented!()
    })
}

fn invoke(alloc: &mut Allocator, value: ValueRef, cont: ValueRef) -> EvalResult<State> {
    typecase!(cont, alloc, {
        Halt => Ok(State::Halt(value)),
        _ => unimplemented!()
    })
}
