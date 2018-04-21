use std::mem::transmute;
use std::slice;
use std::iter;

use pcws_gc::GSize;
use pcws_domain::Allocator;
use pcws_domain::object_model::{Unbox, ValueRef, ValueRefT};
use pcws_domain::values::{Tuple, Slice};
use ast::{Block, Def, Lex, Dyn, Const};
use env::{self, Env, EnvBuffer};

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
    lenv_buf: Option<ValueRefT<EnvBuffer>>,
    denv_buf: Option<ValueRefT<EnvBuffer>>,
    stack: Vec<Option<ValueRef>>,
    fp: usize
}

#[derive(Debug)]
enum State {
    Eval,
    Exec,
    Parse(ValueRefT<Slice>),
    Continue(ValueRef),
    Halt(ValueRef)
}

trait SubFrame { const TAG: usize; }

#[repr(C)]
struct BlockFrame {
    block: ValueRefT<Block>,
    index: ValueRefT<isize>
}

impl SubFrame for BlockFrame { const TAG: usize = 0b1; }

#[repr(C)]
struct DefFrame { def: ValueRefT<Def> }

impl SubFrame for DefFrame { const TAG: usize = 0b1001; }

#[derive(Clone, Copy)]
#[repr(C)]
struct CommitFrame;

impl SubFrame for CommitFrame { const TAG: usize = 0b10001; }

// FIXME: Environment save/restore
impl Interpreter {
    fn new(stack_capacity: usize, program: ValueRef) -> Interpreter {
        Interpreter {
            control: program,
            lenv: None,
            denv: None,
            lenv_buf: None,
            denv_buf: None,
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
                State::Parse(seq) => self.parse(seq)?,
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

                    self.push_frame(BlockFrame { block, index: (index as isize).into() });
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
            def: Def => {
                self.push_frame(DefFrame { def });
                self.control = def.expr();
                Ok(State::Eval)
            },
            // typecase!(def.pattern(), {
            //     lvar: Lex => {
            //         self.push_frame(VarFrame(def.pattern()));
            //         self.control = def.expr();
            //         Ok(State::Eval)
            //     },
            //     dvar: Dyn => {
            //         self.push_frame(VarFrame(def.pattern()));
            //         self.control = def.expr();
            //         Ok(State::Eval)
            //     },
            //     _ => unimplemented!()
            // }),
            _ => Ok(State::Eval)
        })
    }

    fn parse(&mut self, seq: ValueRefT<Slice>) -> EvalResult<State> {
        typecase!(self.control, {
            lex: Lex => {
                // TODO
                unimplemented!()
            },
            _ => unimplemented!()
        })
    }

    fn invoke(&mut self, mut value: ValueRef) -> EvalResult<State> {
        // println!("invoke, fp = {}, sp = {}", self.fp, self.stack.len());
        if !self.stack.is_empty() {
            self.restore_envs();
            match self.top_frame_tag() {
                BlockFrame::TAG => {
                    let &BlockFrame { block, index } = self.top_frame();
                    let new_index: usize = index.unbox() as usize + 1;
                    if new_index < block.stmts().len() {
                        self.top_frame_mut::<BlockFrame>().index = (new_index as isize).into();
                        self.control = block.stmts()[new_index];
                        Ok(State::Exec)
                    } else {
                        self.pop_frame();
                        self.control = block.expr();
                        Ok(State::Eval)
                    }
                },
                DefFrame::TAG => {
                    let &DefFrame { mut def } = self.top_frame();
                    self.lenv_buf =
                        Some(allocate!(EnvBuffer::with_capacity, (def.lex_defs().vals().len()),
                                       {self, value, def})?);
                    self.denv_buf =
                        Some(allocate!(EnvBuffer::with_capacity, (def.dyn_defs().vals().len()),
                                       {self, value, def})?);
                    self.push_frame(CommitFrame);
                    self.control = def.pattern();
                    let mut tuple = allocate!(Tuple::new, (1, iter::once(value)), {self, value})?;
                    let seq = allocate!(Slice::new, (tuple, 0, 1), {self, tuple})?;
                    Ok(State::Parse(seq))
                },
                // {
                //     let (name, env) = typecase!(self.top_frame::<VarFrame>().0, {
                //         lvar: Lex => (lvar.name(), self.lenv),
                //         dvar: Dyn => (dvar.name(), self.denv),
                //         _ => unreachable!()
                //     });
                //     let promise = env.unwrap().get(name)?.unwrap();
                //     if let Some(mut promise) = promise.try_downcast::<Promise>() {
                //         promise.init(value);
                //         self.pop_frame();
                //         Ok(State::Continue(value))
                //     } else {
                //         return Err(EvalError::Type);
                //     }
                // },
                _ => unimplemented!()
            }
        } else {
            Ok(State::Halt(value))
        }
    }

    fn restore_envs(&mut self) {
        self.lenv = unsafe { transmute(self.stack[self.fp + 1]) };
        self.denv = unsafe { transmute(self.stack[self.fp + 2]) };
        self.lenv_buf = unsafe { transmute(self.stack[self.fp + 3]) };
        self.denv_buf = unsafe { transmute(self.stack[self.fp + 4]) };
    }

    fn top_frame_tag(&self) -> usize {
        unsafe { transmute(self.stack[self.fp + 5]) }
    }

    fn top_frame<T: SubFrame>(&self) -> &T {
        unsafe { transmute::<_, &T>(&self.stack[self.fp + 6]) }
    }

    fn top_frame_mut<T: SubFrame>(&mut self) -> &mut T {
        unsafe { transmute::<_, &mut T>(&mut self.stack[self.fp + 6]) }
    }

    fn pop_frame(&mut self) {
        let new_fp: ValueRefT<isize> = unsafe { self.stack[self.fp].unwrap().downcast() };
        self.stack.truncate(self.fp);
        self.fp = new_fp.unbox() as _;
    }

    fn push_frame<T: SubFrame>(&mut self, subframe: T) {
        let old_fp = self.fp;
        self.fp = self.stack.len();
        self.stack.push(ValueRefT::from(old_fp as isize).as_root());
        self.stack.push(self.lenv.as_root());
        self.stack.push(self.denv.as_root());
        self.stack.push(self.lenv_buf.as_root());
        self.stack.push(self.denv_buf.as_root());

        self.stack.push(unsafe { transmute(T::TAG) });
        let ptr = &subframe as *const T as *const Option<ValueRef>;
        let fields = unsafe { slice::from_raw_parts(ptr, GSize::of::<T>().into()) };
        for field in fields {
            self.stack.push(*field);
        }
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
