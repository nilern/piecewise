use std::fmt::{self, Formatter, Debug};

use value_refs::{ValueRef, TypedValueRef};
use value::{DynamicDebug, TypeRegistry, ValueManager, OutOfMemory, Unbound, Reinit, ValueView,
            Block, Halt};

pub enum EvalError {
    OOM(OutOfMemory),
    Unbound(Unbound),
    Reassignment
}

impl From<OutOfMemory> for EvalError {
    fn from(oom: OutOfMemory) -> EvalError { EvalError::OOM(oom) }
}

impl From<Unbound> for EvalError {
    fn from(unbound: Unbound) -> EvalError { EvalError::Unbound(unbound) }
}

impl From<Reinit> for EvalError {
    fn from(ri: Reinit) -> EvalError { EvalError::Reassignment }
}

impl DynamicDebug for EvalError {
    fn fmt<R: TypeRegistry>(&self, f: &mut Formatter, types: &R) -> Result<(), fmt::Error> {
        match self {
            &EvalError::OOM(ref oom) => oom.fmt(f),
            &EvalError::Unbound(ref unb) => unb.fmt(f, types),
            &EvalError::Reassignment => f.debug_tuple("Reassignment").finish()
        }
    }
}

enum State {
    Eval {
        expr: ValueRef,
        lenv: ValueRef,
        denv: ValueRef,
        cont: ValueRef
    },
    Exec {
        stmt: ValueRef,
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
                State::Exec { stmt, lenv, denv, cont } => self.exec(stmt, lenv, denv, cont)?,
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
                factory.create_block_lenv(lenv, block.stmts())
                .and_then(|lenv| {
                    factory.create_block_denv(denv, block.stmts())
                    .and_then(|denv| {
                        factory.create_block_cont(cont, lenv.into(), denv.into(), block, 0)
                        .map(|cont| State::Exec {
                            stmt: block.stmts()[0],
                            lenv: lenv.into(),
                            denv: denv.into(),
                            cont: cont.into()
                        })
                    })
                })
            }, &mut [block.as_mut(), &mut lenv, &mut denv, &mut cont]),
            ValueView::Lex(lvar) => if let ValueView::Env(lenv) = lenv.view(&self.values) {
                lenv.get(lvar.name(), &self.values)
                    .map(|value| State::Continue { value: value, cont })
                    .map_err(EvalError::from)
            } else {
                unimplemented!()
            },
            ValueView::Const(tvref) => Ok(State::Continue { value: tvref.value(), cont }),
            _ => unimplemented!()
        }
    }

    fn exec(&mut self, stmt: ValueRef, mut lenv: ValueRef, mut denv: ValueRef, mut cont: ValueRef)
        -> Result<State, EvalError>
    {
        match stmt.view(&self.values) {
            ValueView::Def(def) => {
                if let ValueView::Lex(mut lvar) = def.pattern().view(&self.values) {
                    let mut expr = def.expr();
                    self.with_gc_retry(move |factory| {
                        factory.create_def_cont(cont, lenv, denv, lvar.into())
                               .map(|cont| State::Eval { expr, lenv, denv, cont: cont.into() })
                    }, &mut [lvar.as_mut(), &mut expr, &mut lenv, &mut denv, &mut cont])
                } else {
                    unimplemented!()
                }
            }
            _ => Ok(State::Eval { expr: stmt, lenv, denv, cont })
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
                        .map(|cont| State::Exec {
                            stmt: cont.block().stmts()[index],
                            lenv, denv, cont: cont.into()
                        })
                    }, &mut [cont.as_mut()])
                }
            },
            ValueView::DefCont(cont) => {
                self.assign(cont.lenv(), cont.denv(), cont.var(), value)?;
                Ok(State::Continue { value /* won't be used anyway */, cont: cont.parent() })
            },
            _ => unimplemented!()
        }
    }

    // fn apply(&mut self, function: ValueRef, args: Vec<ValueRef>, denv: Gc<Env>, cont: Cont)
    //     -> Result<State, EvalError>
    // {
    //     unimplemented!()
    // }

    fn assign(&self, lenv: ValueRef, denv: ValueRef, var: ValueRef, value: ValueRef)
        -> Result<(), EvalError>
    {
        match var.view(&self.values) {
            ValueView::Lex(lvar) => if let ValueView::Env(lenv) = lenv.view(&self.values) {
                lenv.get(lvar.name(), &self.values).map_err(EvalError::from)
                    .and_then(|promise|
                        if let ValueView::Promise(mut promise) = promise.view(&self.values) {
                            promise.init(value, &self.values).map_err(EvalError::from)
                        } else {
                            Err(EvalError::Reassignment)
                        }
                    )
            } else {
                unimplemented!()
            },
            _ => unimplemented!()
        }
    }

    fn with_gc_retry<C, R>(&mut self, create: C, roots: &mut [&mut ValueRef])
        -> Result<R, EvalError>
        where C: FnMut(&mut ValueManager) -> Option<R>
    {
        self.values.with_gc_retry(create, roots).map_err(EvalError::from)
    }
}
