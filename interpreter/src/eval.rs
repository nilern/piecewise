use std::iter;
use std::fmt::{self, Formatter};

use interpreter::{Interpreter, Allocator};
use object_model::{DynamicDebug, ValueRef, ScalarValueRef, HeapValueRef};
use value::{Reinit, TypeRegistry, ValueView, Tuple};
use ast::Block;
use continuations::Halt;
use env::Unbound;

pub enum EvalError {
    OutOfMemory,
    Type,
    Unbound(Unbound),
    Uninitialized(ValueRef),
    Reassignment
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
            &EvalError::OutOfMemory => f.debug_tuple("OutOfMemory").finish(),
            &EvalError::Type => f.debug_tuple("Type").finish(),
            &EvalError::Unbound(ref unb) => unb.fmt(f, types),
            &EvalError::Uninitialized(v) => f.debug_tuple("Uninitialized")
                                             .field(&v.fmt_wrap(types))
                                             .finish(),
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
    },
    Apply {
        callee: ValueRef,
        args: HeapValueRef<Tuple>,
        denv: ValueRef,
        cont: ValueRef
    }
}

impl State {
    fn start(factory: &mut Allocator, program: HeapValueRef<Block>) -> Option<State> {
        factory.create_halt()
               .map(|cont| State::Eval {
                   expr: ValueRef::from(program),
                   lenv: ScalarValueRef::from(false).into(),
                   denv: ScalarValueRef::from(false).into(),
                   cont: cont.into()
               })
    }
}

impl Interpreter {
    /// Run `program`.
    pub fn run(&mut self, mut program: HeapValueRef<Block>) -> Result<ValueRef, EvalError> {
        let mut state = self.with_gc_retry(move |allocator| State::start(allocator, program),
                                    &mut [program.as_mut()])
                            .ok_or(EvalError::OutOfMemory)?;

        loop {
            state = match state {
                State::Eval { expr, lenv, denv, cont } => self.eval(expr, lenv, denv, cont)?,
                State::Exec { stmt, lenv, denv, cont } => self.exec(stmt, lenv, denv, cont)?,
                State::Continue { value, cont }
                    if cont.is_instance::<Interpreter, Halt>(self) => return Ok(value),
                State::Continue { value, cont } => self.invoke(cont, value)?,
                State::Apply { callee, args, denv, cont } => self.apply(callee, args, denv, cont)?
            }
        }
    }

    fn eval(&mut self, expr: ValueRef, mut lenv: ValueRef, mut denv: ValueRef, mut cont: ValueRef)
        -> Result<State, EvalError>
    {
        match expr.view(self) {
            ValueView::Function(mut f) =>
                self.with_gc_retry(move |factory| {
                    factory.create_closure(f, lenv)
                }, &mut [f.as_mut(), &mut lenv, &mut cont])
                .map(|closure| State::Continue { value: closure.into(), cont })
                .ok_or(EvalError::OutOfMemory),
            ValueView::Block(tvref) if tvref.stmts().is_empty() =>
                Ok(State::Eval { expr: tvref.expr, lenv, denv, cont }),
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
            }, &mut [block.as_mut(), &mut lenv, &mut denv, &mut cont])
            .ok_or(EvalError::OutOfMemory),
            ValueView::Call(mut call) =>
                self.with_gc_retry(move |factory| {
                    factory.create_callee_cont(cont, lenv, denv, call)
                }, &mut [call.as_mut(), &mut lenv, &mut denv, &mut cont])
                .map(|cont| State::Eval { expr: call.callee, lenv, denv, cont: cont.into() })
                .ok_or(EvalError::OutOfMemory),
            ValueView::Lex(lvar) => if let ValueView::Env(lenv) = lenv.view(self) {
                lenv.get(lvar.name, self)
                    .map(|value| State::Continue { value: value, cont })
                    .map_err(EvalError::from)
            } else {
                unimplemented!()
            },
            ValueView::Const(tvref) => Ok(State::Continue { value: tvref.value, cont }),
            _ => unimplemented!()
        }
    }

    fn exec(&mut self, stmt: ValueRef, mut lenv: ValueRef, mut denv: ValueRef, mut cont: ValueRef)
        -> Result<State, EvalError>
    {
        match stmt.view(self) {
            ValueView::Def(def) => {
                if let ValueView::Lex(mut lvar) = def.pattern.view(self) {
                    let mut expr = def.expr;
                    self.with_gc_retry(move |factory| {
                        factory.create_def_cont(cont, lenv, denv, lvar.into())
                               .map(|cont| State::Eval { expr, lenv, denv, cont: cont.into() })
                    }, &mut [lvar.as_mut(), &mut expr, &mut lenv, &mut denv, &mut cont])
                    .ok_or(EvalError::OutOfMemory)
                } else {
                    unimplemented!()
                }
            }
            _ => Ok(State::Eval { expr: stmt, lenv, denv, cont })
        }
    }

    fn invoke(&mut self, cont: ValueRef, mut value: ValueRef) -> Result<State, EvalError> {
        match cont.view(self) {
            ValueView::BlockCont(mut cont) => {
                let index = cont.index() + 1;
                if index == cont.block.stmts().len() {
                    Ok(State::Eval {
                        expr: cont.block.expr,
                        lenv: cont.lenv,
                        denv: cont.denv,
                        cont: cont.parent
                    })
                } else {
                    self.with_gc_retry(move |factory| {
                        let lenv = cont.lenv;
                        let denv = cont.denv;
                        let block = cont.block;
                        factory
                        .create_block_cont(cont.parent, lenv, denv, block, index)
                        .map(|cont| State::Exec {
                            stmt: cont.block.stmts()[index],
                            lenv, denv, cont: cont.into()
                        })
                    }, &mut [cont.as_mut()])
                    .ok_or(EvalError::OutOfMemory)
                }
            },
            ValueView::DefCont(cont) => {
                self.assign(cont.lenv, cont.denv, cont.var, value)?;
                Ok(State::Continue { value /* won't be used anyway */, cont: cont.parent })
            },
            ValueView::CalleeCont(mut cont) =>
                self.with_gc_retry(move |factory| {
                    let lenv = cont.lenv;
                    let denv = cont.denv;
                    let call = cont.call;
                    factory.create_arg_cont(cont.parent, lenv, denv, call, 0, value, &[])
                           .map(|cont| State::Eval {
                               expr: call.args()[0],
                               lenv, denv, cont: cont.into()
                           })
                }, &mut [cont.as_mut(), &mut value])
                .ok_or(EvalError::OutOfMemory),
            ValueView::ArgCont(mut cont) => {
                let index = cont.index() + 1;
                if index < cont.call.args().len() {
                    self.with_gc_retry(move |factory| {
                        let lenv = cont.lenv;
                        let denv = cont.denv;
                        let call = cont.call;
                        factory.create_arg_cont(cont.parent, lenv, denv, call,
                                                index, cont.callee, cont.args())
                               .map(|cont| State::Eval {
                                   expr: call.args()[index],
                                   lenv, denv, cont: cont.into()
                               })
                    }, &mut [cont.as_mut(), &mut value])
                    .ok_or(EvalError::OutOfMemory)
                } else {
                    self.with_gc_retry(move |factory| {
                        let args = cont.args();
                        factory.create_tuple(args.len() + 1,
                                             args.iter().cloned().chain(iter::once(value)))
                               .map(|args| State::Apply {
                                   callee: cont.callee,
                                   args,
                                   denv: cont.denv,
                                   cont: cont.parent
                               })
                    }, &mut [cont.as_mut(), &mut value])
                    .ok_or(EvalError::OutOfMemory)
                }
            },
            _ => unimplemented!()
        }
    }

    fn apply(&mut self, callee: ValueRef, mut args: HeapValueRef<Tuple>, mut denv: ValueRef,
             mut cont: ValueRef) -> Result<State, EvalError>
    {
        let callee = callee.force().ok_or(EvalError::Uninitialized(callee))?;
        if let ValueView::Closure(mut f) = callee.view(self) {
            let mut method = f.function.methods()[0];
            if let ValueView::Lex(mut param) = method.pattern.view(self) {
                self.with_gc_retry(move |factory| {
                    factory.create_method_lenv(f.lenv, param.name, args.values()[0])
                           .map(|lenv| State::Eval {
                               expr: method.body, lenv: lenv.into(), denv, cont
                           })
                }, &mut [f.as_mut(), method.as_mut(), param.as_mut(), args.as_mut(), &mut denv,
                         &mut cont])
                .ok_or(EvalError::OutOfMemory)
            } else {
                Err(EvalError::Type)
            }
        } else {
            Err(EvalError::Type)
        }
    }

    fn assign(&self, lenv: ValueRef, denv: ValueRef, var: ValueRef, value: ValueRef)
        -> Result<(), EvalError>
    {
        match var.view(self) {
            ValueView::Lex(lvar) => if let ValueView::Env(lenv) = lenv.view(self) {
                lenv.get(lvar.name, self).map_err(EvalError::from)
                    .and_then(|promise|
                        if let ValueView::Promise(mut promise) = promise.view(self) {
                            promise.init(value, self).map_err(EvalError::from)
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
}
