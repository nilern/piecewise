use std::collections::HashMap;
use gc::Gc;

use value::{Value, ValueRef};
use ast::{Expr, Block};

#[derive(Debug)]
pub enum EvalError {
    
}

enum State {
    Eval {
        expr: Gc<Expr>,
        lenv: Gc<Env>,
        denv: Gc<Env>,
        cont: Cont
    },
    Continue {
        value: ValueRef,
        cont: Cont
    },
    Apply {
        function: ValueRef,
        args: Vec<ValueRef>,
        denv: Gc<Env>,
        cont: Cont
    }
}

#[derive(Debug, Trace, Finalize)]
struct Env {
    parent: Option<Gc<Env>>,
    bindings: HashMap<String, ValueRef>
}

#[derive(Debug, Trace, Finalize)]
enum Cont {
    Halt
}

impl State {
    fn start(program: Block) -> State {
        State::Eval {
            expr: Gc::new(Expr::Block(program)),
            lenv: Gc::new(Env::new(None)),
            denv: Gc::new(Env::new(None)),
            cont: Cont::Halt
        }
    }
}

impl Env {
    fn new(parent: Option<Gc<Env>>) -> Env {
        Env {
            parent,
            bindings: HashMap::new()
        }
    } 
}

pub fn run(program: Block) -> Result<Gc<Value>, EvalError> {
    let mut state = State::start(program);
    loop {
        state = match state {
            State::Eval { expr, lenv, denv, cont } => eval(expr, lenv, denv, cont)?,
            State::Continue { value, cont: Cont::Halt } => return Ok(value),
            State::Continue { value, cont } => invoke(cont, value)?,
            State::Apply { function, args, denv, cont } => apply(function, args, denv, cont)?
        }
    }
}

fn eval(expr: Gc<Expr>, lenv: Gc<Env>, denv: Gc<Env>, cont: Cont) -> Result<State, EvalError> {
    match *expr {
        Expr::Block(Block { ref stmts, ref expr }) if stmts.is_empty() =>
            Ok(State::Eval { expr: expr.clone(), lenv, denv, cont }),
        Expr::Const(ref value) => Ok(State::Continue { value: value.clone(), cont }),
        _ => unimplemented!()
    }
}

fn invoke(cont: Cont, value: ValueRef) -> Result<State, EvalError> {
    unimplemented!()
}

fn apply(function: ValueRef, args: Vec<ValueRef>, denv: Gc<Env>, cont: Cont) -> Result<State, EvalError> {
    unimplemented!()
}
