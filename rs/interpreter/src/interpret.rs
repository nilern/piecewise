use std::mem::transmute;

use pcws_domain::object_model::{Unbox, ValueRef, ValueRefT};
use pcws_domain::Allocator;
use ast::{Block, Call, Def, Const};
use env::Env;

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
        let mut heap = Allocator::instance_mut();
        if let Some(v) = $f(&mut*heap, $($args),*) {
            Ok(v)
        } else {
            itp.mark_roots(&mut heap);
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
    OOM
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
    const BLOCK: usize = 0x0001; // HACK

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
        typecase!(self.control, {
            mut block: Block =>
                if 0 < block.stmts().len() {
                    let index = 0;
                    self.push_block_frame(block, index);
                    // TODO: self.lenv = ...; self.denv = ...;
                    self.control = block.stmts()[index];
                    Ok(State::Exec)
                } else {
                    self.control = block.expr();
                    Ok(State::Eval)
                },
            c: Const => Ok(State::Continue(c.value())),
            _ => unimplemented!()
        })
    }

    fn exec(&mut self) -> EvalResult<State> {
        typecase!(self.control, {
            def: Def => unimplemented!(),
            _ => Ok(State::Eval)
        })
    }

    fn invoke(&mut self, value: ValueRef) -> EvalResult<State> {
        if !self.stack.is_empty() {
            match unsafe { transmute(self.stack[self.fp]) } {
                Self::BLOCK => {
                    let block: ValueRefT<Block> = unsafe { transmute(self.stack[self.fp + 1]) };
                    let index: ValueRefT<isize> = unsafe { transmute(self.stack[self.fp + 2]) };
                    let new_index: usize = index.unbox() as usize + 1;
                    if new_index < block.stmts().len() {
                        self.stack[self.fp + 2] = ValueRefT::from(new_index as isize).as_root();
                        self.control = block.stmts()[new_index];
                        Ok(State::Exec)
                    } else {
                        self.pop_frame();
                        self.control = block.expr();
                        Ok(State::Eval)
                    }
                },
                _ => unimplemented!()
            }
        } else {
            Ok(State::Halt(value))
        }
    }

    unsafe fn push_base_frame(&mut self) {
        self.stack.push(self.lenv.as_root());
        self.stack.push(self.denv.as_root());
        let offset = self.stack.len() + 1 - self.fp;
        self.stack.push(ValueRefT::from(offset as isize).as_root());
        self.fp = self.stack.len();
    }

    fn push_block_frame(&mut self, block: ValueRefT<Block>, index: usize) {
        unsafe { self.push_base_frame(); }
        self.stack.push(unsafe { transmute(Self::BLOCK) });
        self.stack.push(block.as_root());
        self.stack.push(ValueRefT::from(index as isize).as_root());
    }

    fn pop_frame(&mut self) {
        let offset: ValueRefT<isize> = unsafe { transmute(self.stack[self.fp - 1]) };
        let offset: usize = offset.unbox() as _;
        self.denv = unsafe { transmute(self.stack[self.fp - 2]) };
        self.lenv = unsafe { transmute(self.stack[self.fp - 3]) };
        self.stack.truncate(self.fp - 3);
        self.fp -= offset;
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
