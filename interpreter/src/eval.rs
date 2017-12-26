use value_refs::{ValueRef, TypedValueRef};
use value::{ValueManager, ValueView, Block, Halt};

#[derive(Debug)]
pub enum EvalError {

}

enum State {
    Eval {
        expr: ValueRef,
        lenv: ValueRef,
        denv: ValueRef,
        cont: ValueRef
    },
    Continue {
        value: ValueRef,
        cont: ValueRef
    }
    // Apply {
    //     function: ValueRef,
    //     args: Vec<ValueRef>,
    //     denv: Gc<Env>,
    //     cont: Cont
    // }
}

impl State {
    fn start(factory: &mut ValueManager, program: TypedValueRef<Block>) -> State {
        State::Eval {
            expr: ValueRef::from(program),
            lenv: ValueRef::from(false),
            denv: ValueRef::from(false),
            cont: ValueRef::from(factory.create_halt().unwrap())
        }
    }
}

// #[derive(Debug, Trace, Finalize)]
// struct Env {
//     parent: Option<Gc<Env>>,
//     bindings: HashMap<String, ValueRef>
// }
//
// #[derive(Debug, Trace, Finalize)]
// enum Cont {
//     Halt
// }

// impl Env {
//     fn new(parent: Option<Gc<Env>>) -> Env {
//         Env {
//             parent,
//             bindings: HashMap::new()
//         }
//     }
// }

pub struct Interpreter {
    pub values: ValueManager
}

impl Interpreter {
    pub fn new(max_heap: usize) -> Interpreter {
        Interpreter {
            values: ValueManager::new(max_heap)
        }
    }

    pub fn run(&mut self, program: TypedValueRef<Block>) -> Result<ValueRef, EvalError> {
        let mut state = State::start(&mut self.values, program);

        loop {
            state = match state {
                State::Eval { expr, lenv, denv, cont } => self.eval(expr, lenv, denv, cont)?,
                State::Continue { value, cont }
                    if cont.is_instance::<ValueManager, Halt>(&self.values) => return Ok(value),
                State::Continue { value, cont } => self.invoke(cont, value)?,
                //State::Apply { function, args, denv, cont } => apply(function, args, denv, cont)?
            }
        }
    }

    fn eval(&self, expr: ValueRef, lenv: ValueRef, denv: ValueRef, cont: ValueRef)
        -> Result<State, EvalError>
    {
        match expr.view(&self.values) {
            ValueView::Block(tvref) if tvref.stmts().is_empty() =>
                Ok(State::Eval { expr: tvref.expr(), lenv, denv, cont }),
            ValueView::Const(tvref) => Ok(State::Continue { value: tvref.value(), cont }),
            _ => unimplemented!()
        }
    }

    fn invoke(&self, cont: ValueRef, value: ValueRef) -> Result<State, EvalError> {
        unimplemented!()
    }

    // fn apply(&mut self, function: ValueRef, args: Vec<ValueRef>, denv: Gc<Env>, cont: Cont)
    //     -> Result<State, EvalError>
    // {
    //     unimplemented!()
    // }
}
