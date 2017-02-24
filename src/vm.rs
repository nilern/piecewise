use std::fmt;
use std::fmt::Display;
use std::mem;

use value::RawRef;

const OPERAND_SHIFT: u8 = 2;
const OPERAND_MASK: u8 = 0b11;
const LOCAL_TAG: u8 = 0b00;
const CONST_TAG: u8 = 0b01;

/// Unpacked representation for complex operands of virtual instructions
#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Local(u8),
    Const(u8)
}

impl Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Operand::*;
        match self {
            &Local(i) => write!(f, "l{}", i),
            &Const(i) => write!(f, "c{}", i)
        }
    }
}

impl From<u8> for Operand {
    fn from(byte: u8) -> Operand {
        use self::Operand::*;
        match byte & OPERAND_MASK {
            LOCAL_TAG => Local(byte >> OPERAND_SHIFT),
            CONST_TAG => Const(byte >> OPERAND_SHIFT),
            2 => unimplemented!(),
            3 => unimplemented!(),
            _ => unreachable!()
        }
    }
}

impl From<Operand> for u8 {
    fn from(op: Operand) -> u8 {
        use self::Operand::*;
        match op {
            Local(i) => i << OPERAND_SHIFT | LOCAL_TAG,
            Const(i) => i << OPERAND_SHIFT | CONST_TAG
        }
    }
}

/// A virtual instruction.
#[derive(Debug, Clone, Copy)]
pub enum Instr {
    Mov(u8, u8),

    Fun(u8, u16),

    IAdd(u8, u8, u8),
    ISub(u8, u8, u8),
    IMul(u8, u8, u8),

    ILt(u8, u8),

    Br(u16),
    Call(u16),
    Ret(u8),

    Halt
}

impl Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Instr::*;
        match self {
            &Mov(dest, src) => write!(f, "mov  l{}, {}", dest, Operand::from(src)),
            &Fun(d, i) => write!(f, "fun  l{}, {}", d, i),
            &IAdd(d, l, r) => write!(f, "iadd l{}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &ISub(d, l, r) => write!(f, "isub l{}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &IMul(d, l, r) => write!(f, "imul l{}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &ILt(l, r) => write!(f, "ilt  {}, {}", Operand::from(l), Operand::from(r)),
            &Br(offset) => write!(f, "br   {}", offset),
            &Call(offset) => write!(f, "call {}", offset),
            &Ret(v) => write!(f, "ret  {}", Operand::from(v)),
            &Halt => write!(f, "halt")
        }
    }
}

/// A temporary shim for code objects
#[derive(Debug, Clone)]
pub struct CodeObject {
    pub code: Vec<Instr>,
    pub consts: Vec<RawRef>,
    pub reg_req: usize,
    pub cobs: Vec<CodeObject>
}

/// A temporary shim for closures.
#[derive(Debug)]
pub struct Closure {
    pub cob: CodeObject
}

/// Proff virtual machine
#[derive(Debug)]
pub struct VM {
    cl: Box<Closure>,
    ip: usize,
    fp: usize,
    stack: Vec<RawRef>
}

impl VM {
    /// Create a new VM.
    pub fn new(fun: CodeObject) -> VM {
        let stacksize = fun.reg_req;
        VM {
            cl: Box::new(Closure { cob: fun }),
            ip: 0,
            fp: 0,
            stack: vec![RawRef(0); stacksize]
        }
    }

    /// Start the VM.
    pub fn run(&mut self) {
        use self::Instr::*;

        loop {
            let instr = self.cl.cob.code[self.ip];
            println!("{} [{}]: {}", self.stack.len(), self.ip, instr);
            self.ip += 1;

            match instr {
                Mov(di, si) => {
                    let s = self.decode_operand(si);
                    self.set_reg(di, s);
                },

                Fun(di, ci) => {
                    let cob = self.cl.cob.cobs[ci as usize].clone();
                    self.set_reg(di, unsafe { mem::transmute(Box::new(Closure {cob: cob })) });
                },

                IAdd(di, li, ri) => {
                    let l = self.decode_operand(li);
                    let r = self.decode_operand(ri);
                    self.set_reg(di, RawRef(l.0 + r.0));
                },
                ISub(di, li, ri) => {
                    let l = self.decode_operand(li);
                    let r = self.decode_operand(ri);
                    self.set_reg(di, RawRef(l.0 - r.0));
                },
                IMul(di, li, ri) => {
                    let l = self.decode_operand(li);
                    let r = self.decode_operand(ri);
                    self.set_reg(di, RawRef(l.0 * r.0));
                },

                ILt(li, ri) => {
                    let l = self.decode_operand(li);
                    let r = self.decode_operand(ri);
                    if l.0 < r.0 {
                        self.ip += 1;
                    }
                },

                Br(offset) => {
                    self.ip += offset as usize;
                },
                // TODO: `Call` is a bit messy. Also need `TCall`. Maybe `Call` can be broken into
                // `PushK` and `TCall` to solve both?
                Call(fp_offset) => {
                    let newfp = self.fp + fp_offset as usize;
                    self.stack[newfp - 3] = RawRef(self.fp);
                    self.stack[newfp - 2] = RawRef(self.ip);
                    let mut cl = unsafe { mem::transmute(self.stack[newfp]) };
                    mem::swap(&mut cl, &mut self.cl);
                    self.stack[newfp - 1] = unsafe { mem::transmute(cl) };
                    self.ip = 0;
                    self.fp = newfp;
                    self.stack.resize(self.fp + self.cl.cob.reg_req, RawRef(0));
                },
                // TODO: Estimate register quantity better, null out registers above return value.
                Ret(vi) => {
                    let newfp = self.stack[self.fp - 3];
                    self.stack[self.fp - 3] = self.decode_operand(vi);
                    self.ip = self.stack[self.fp - 2].0;
                    self.cl = unsafe { mem::transmute(self.stack[self.fp - 1]) };
                    self.fp = newfp.0;
                    self.stack.resize(self.fp + self.cl.cob.reg_req, RawRef(0));
                },

                Halt => break
            }
        }
    }

    fn decode_operand(&self, operand: u8) -> RawRef {
        match From::from(operand) {
            Operand::Local(li) => self.stack[self.fp + li as usize],
            Operand::Const(ci) => self.cl.cob.consts[ci as usize]
        }
    }

    fn set_reg(&mut self, reg_index: u8, val: RawRef) {
        self.stack[self.fp + reg_index as usize] = val;
    }
}
