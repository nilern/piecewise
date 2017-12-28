use value_refs::{ValueRef, TypedValueRef};
use value::{ValueManager, OutOfMemory, ValueView, Block, Halt};

#[derive(Debug)]
pub enum EvalError {
    OOM(OutOfMemory)
}

impl From<OutOfMemory> for EvalError {
    fn from(oom: OutOfMemory) -> EvalError { EvalError::OOM(oom) }
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

    fn eval(&mut self, expr: ValueRef, mut lenv: ValueRef, mut denv: ValueRef, mut cont: ValueRef)
        -> Result<State, EvalError>
    {
        match expr.view(&self.values) {
            ValueView::Block(tvref) if tvref.stmts().is_empty() =>
                Ok(State::Eval { expr: tvref.expr(), lenv, denv, cont }),
            ValueView::Block(mut block) => self.with_gc_retry(move |factory| {
                factory
                .create_block_cont(cont, lenv, denv, block, 0)
                .and_then(|cont| {
                    factory.create_block_env(lenv, block.lex_binders())
                    .and_then(|lenv| {
                        factory.create_block_env(denv, block.dyn_binders())
                        .map(|denv| State::Eval {
                            expr: block.stmts()[0],
                            lenv: lenv.into(),
                            denv: denv.into(),
                            cont: cont.into()
                        })
                    })
                })
            }, &mut [block.as_mut(), &mut lenv, &mut denv, &mut cont]),
            ValueView::Const(tvref) => Ok(State::Continue { value: tvref.value(), cont }),
            _ => unimplemented!()
        }
    }

    fn invoke(&mut self, cont: ValueRef, value: ValueRef) -> Result<State, EvalError> {
        match cont.view(&self.values) {
            ValueView::BlockCont(mut cont) => {
                let index = cont.index() + 1;
                if index == cont.block().stmts().len() {
                    Ok(State::Eval {
                        expr: cont.block().expr(),
                        lenv: cont.lenv(),
                        denv: cont.denv(),
                        cont: cont.parent()
                    })
                } else {
                    self.with_gc_retry(move |factory| {
                        let lenv = cont.lenv();
                        let denv = cont.denv();
                        let block = cont.block();
                        factory
                        .create_block_cont(cont.parent(), lenv, denv, block, index)
                        .map(|cont| State::Eval {
                            expr: cont.block().stmts()[index],
                            lenv, denv, cont: cont.into()
                        })
                    }, &mut [cont.as_mut()])
                }
            },
            _ => unimplemented!()
        }
    }

    // fn apply(&mut self, function: ValueRef, args: Vec<ValueRef>, denv: Gc<Env>, cont: Cont)
    //     -> Result<State, EvalError>
    // {
    //     unimplemented!()
    // }

    fn with_gc_retry<C, R>(&mut self, mut create: C, roots: &mut [&mut ValueRef])
        -> Result<R, EvalError>
        where C: FnMut(&mut ValueManager) -> Option<R>
    {
        self.values.with_gc_retry(create, roots).map_err(EvalError::from)
    }
}
