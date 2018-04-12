use std::mem::transmute;

use pcws_domain::Allocator;
use pcws_domain::object_model::{Unbox, ValueRef, ValueRefT};
use pcws_domain::values::Promise;
use ast::{Block, Call, Def, Lex, Dyn, Const};
use env::{self, Env};

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
    ($f:path, ($($args:expr),*), { $itp:ident, $($live_in:ident),* } ) => {{
        let mut heap = Allocator::instance();
        if let Some(v) = $f(&mut*heap, $($args),*) {
            Ok(v)
        } else {
            $itp.mark_roots(&mut heap);
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
    Unbound(env::Unbound),
    Type,
    OOM
}

impl From<env::Unbound> for EvalError {
    fn from(err: env::Unbound) -> EvalError { EvalError::Unbound(err) }
}

pub type EvalResult<T> = Result<T, EvalError>;

// ================================================================================================

pub fn interpret(program: ValueRef) -> EvalResult<ValueRef> {
    Interpreter::new(/* OPTIMIZE: */ 1000, program).run()
}

// ================================================================================================

#[derive(Debug)]
struct Interpreter {
    control: ValueRef,
    lenv: Option<ValueRefT<Env>>,
    denv: Option<ValueRefT<Env>>,
    stack: Vec<Option<ValueRef>>,
    fp: usize
}

#[derive(Debug)]
enum State {
    Eval,
    Exec,
    Continue(ValueRef),
    Halt(ValueRef)
}

// FIXME: Environment save/restore
impl Interpreter {
    // HACK: Manually tagged constants:
    const BLOCK: usize = 0x0001;
    const VAR: usize = 0x1001;

    fn new(stack_capacity: usize, program: ValueRef) -> Interpreter {
        Interpreter {
            control: program,
            lenv: None,
            denv: None,
            stack: Vec::with_capacity(stack_capacity),
            fp: 0
        }
    }

    fn run(&mut self) -> EvalResult<ValueRef> {
        let mut state = State::Eval;
        loop { // trampoline
            state = match state {
                State::Eval => self.eval()?,
                State::Exec => self.exec()?,
                State::Continue(value) => self.invoke(value)?,
                State::Halt(value) => return Ok(value)
            }
        }
    }

    fn eval(&mut self) -> EvalResult<State> {
        // println!("eval, fp = {}, sp = {}", self.fp, self.stack.len());
        typecase!(self.control, {
            mut block: Block =>
                if 0 < block.stmts().len() {
                    let index = 0;
                    let mut lexen = block.lex_binders();
                    if lexen.vals().len() > 0 {
                        self.lenv = Some(allocate!(Env::block, (self.lenv,
                                                               unsafe { transmute(lexen.vals()) }),
                                                   {self, lexen, block})?);
                    }

                    let mut dyns = block.dyn_binders();
                    if dyns.vals().len() > 0 {
                        self.denv = Some(allocate!(Env::block, (self.denv,
                                                               unsafe { transmute(dyns.vals()) }),
                                                   {self, dyns, block})?);
                    }
                    self.push_block_frame(block, index);
                    self.control = block.stmts()[index];
                    Ok(State::Exec)
                } else {
                    self.control = block.expr();
                    Ok(State::Eval)
                },
            lvar: Lex => {
                let val = self.lenv.unwrap().get(lvar.name())?.expect("uninitialized");
                Ok(State::Continue(val))
            },
            dvar: Dyn => {
                let val = self.denv.unwrap().get(dvar.name())?.expect("uninitialized");
                Ok(State::Continue(val))
            },
            c: Const => Ok(State::Continue(c.value())),
            _ => unimplemented!()
        })
    }

    fn exec(&mut self) -> EvalResult<State> {
        // println!("exec, fp = {}, sp = {}", self.fp, self.stack.len());
        typecase!(self.control, {
            def: Def => typecase!(def.pattern(), {
                lvar: Lex => {
                    self.push_var_frame(def.pattern());
                    self.control = def.expr();
                    Ok(State::Eval)
                },
                dvar: Dyn => {
                    self.push_var_frame(def.pattern());
                    self.control = def.expr();
                    Ok(State::Eval)
                },
                _ => unimplemented!()
            }),
            _ => Ok(State::Eval)
        })
    }

    fn invoke(&mut self, value: ValueRef) -> EvalResult<State> {
        // println!("invoke, fp = {}, sp = {}", self.fp, self.stack.len());
        if !self.stack.is_empty() {
            self.restore_envs();
            match unsafe { transmute(self.stack[self.fp + 3]) } {
                Self::BLOCK => {
                    let block: ValueRefT<Block> = unsafe { transmute(self.stack[self.fp + 4]) };
                    let index: ValueRefT<isize> = unsafe { transmute(self.stack[self.fp + 5]) };
                    let new_index: usize = index.unbox() as usize + 1;
                    if new_index < block.stmts().len() {
                        self.stack[self.fp + 5] = ValueRefT::from(new_index as isize).as_root();
                        self.control = block.stmts()[new_index];
                        Ok(State::Exec)
                    } else {
                        self.pop_frame();
                        self.control = block.expr();
                        Ok(State::Eval)
                    }
                },
                Self::VAR => {
                    let (name, env) = typecase!(self.stack[self.fp + 4].unwrap(), {
                        lvar: Lex => (lvar.name(), self.lenv),
                        dvar: Dyn => (dvar.name(), self.denv),
                        _ => unreachable!()
                    });
                    let promise = env.unwrap().get(name)?.unwrap();
                    if let Some(mut promise) = promise.try_downcast::<Promise>() {
                        promise.init(value);
                        self.pop_frame();
                        Ok(State::Continue(value))
                    } else {
                        return Err(EvalError::Type);
                    }
                },
                _ => unimplemented!()
            }
        } else {
            Ok(State::Halt(value))
        }
    }

    fn restore_envs(&mut self) {
        self.lenv = unsafe { transmute(self.stack[self.fp + 1]) };
        self.denv = unsafe { transmute(self.stack[self.fp + 2]) };
    }

    fn pop_frame(&mut self) {
        let new_fp: ValueRefT<isize> = unsafe { self.stack[self.fp].unwrap().downcast() };
        self.stack.truncate(self.fp);
        self.fp = new_fp.unbox() as _;
    }

    fn push_frame<F>(&mut self, push_specific: F) where F: Fn(&mut Self) {
        let old_fp = self.fp;
        self.fp = self.stack.len();
        self.stack.push(ValueRefT::from(old_fp as isize).as_root());
        self.stack.push(self.lenv.as_root());
        self.stack.push(self.denv.as_root());
        push_specific(self);
    }

    fn push_block_frame(&mut self, block: ValueRefT<Block>, index: usize) {
        self.push_frame(|itp| {
            itp.stack.push(unsafe { transmute(Self::BLOCK) });
            itp.stack.push(block.as_root());
            itp.stack.push(ValueRefT::from(index as isize).as_root())
        })
    }

    fn push_var_frame(&mut self, var: ValueRef) {
        self.push_frame(|itp| {
            itp.stack.push(unsafe { transmute(Self::VAR) });
            itp.stack.push(Some(var));
        })
    }

    fn mark_roots(&mut self, heap: &mut Allocator) {
        unsafe {
            self.control = transmute(heap.mark_ref(self.control.into()));
            self.lenv = transmute(heap.mark_ref(self.control.into()));
            self.denv = transmute(heap.mark_ref(self.control.into()));
        }
        for slot in self.stack.iter_mut() {
            *slot = heap.mark_ref(*slot);
        }
    }
}
