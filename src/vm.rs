use std::fmt;
use std::fmt::Display;
use std::convert::TryFrom;
use std::mem;

use value::{RawRef, TypeError};
use util::ProffError;

// FIXME: Box<Closure> transmutes probably leak memory

/// Unpacked representation for complex operands of virtual instructions
#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Local(u8),
    Const(u8)
}

impl Operand {
    const SHIFT: u8 = 2;
    const MASK: u8 = 0b11;
    const LOCAL_TAG: u8 = 0b00;
    const CONST_TAG: u8 = 0b01;
}

impl Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &self::Operand::Local(i) => write!(f, "l{}", i),
            &self::Operand::Const(i) => write!(f, "c{}", i)
        }
    }
}

impl From<u8> for Operand {
    fn from(byte: u8) -> Operand {
        match byte & Operand::MASK {
            Operand::LOCAL_TAG => Operand::Local(byte >> Operand::SHIFT),
            Operand::CONST_TAG => Operand::Const(byte >> Operand::SHIFT),
            2 => unimplemented!(),
            3 => unimplemented!(),
            _ => unreachable!()
        }
    }
}

impl From<Operand> for u8 {
    fn from(op: Operand) -> u8 {
        match op {
            Operand::Local(i) => i << Operand::SHIFT | Operand::LOCAL_TAG,
            Operand::Const(i) => i << Operand::SHIFT | Operand::CONST_TAG
        }
    }
}

/// A virtual instruction.
#[derive(Debug, Clone, Copy)]
pub enum Instr {
    Mov(u8, u8),
    SvK(u16),

    Fun(u8, u16),

    IAdd(u8, u8, u8),
    ISub(u8, u8, u8),
    IMul(u8, u8, u8),

    ILt(u8, u8),

    Br(u16),
    Call(u16),
    Ret(u8),

    Halt(u8)
}

impl Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Instr::*;
        match self {
            &Mov(dest, src) => write!(f, "mov  {}, {}", dest, Operand::from(src)),
            &SvK(fp_offset) => write!(f, "svk  {}", fp_offset),
            &Fun(d, i) => write!(f, "fun  {}, {}", d, i),
            &IAdd(d, l, r) => write!(f, "iadd {}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &ISub(d, l, r) => write!(f, "isub {}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &IMul(d, l, r) => write!(f, "imul {}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &ILt(l, r) => write!(f, "ilt  {}, {}", Operand::from(l), Operand::from(r)),
            &Br(offset) => write!(f, "br   {}", offset),
            &Call(argc) => write!(f, "call {}", argc),
            &Ret(v) => write!(f, "ret  {}", Operand::from(v)),
            &Halt(ri) => write!(f, "halt {}", Operand::from(ri))
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
#[derive(Debug, Clone)]
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
            stack: vec![From::from(0); stacksize]
        }
    }

    /// Start the VM.
    pub fn run(&mut self) -> Result<RawRef, ProffError> {
        use self::Instr::*;

        loop {
            let instr = self.cl.cob.code[self.ip];
            //println!("{} [{}]: {}", self.stack.len(), self.ip, instr);
            self.ip += 1;

            match instr {
                Mov(di, si) => {
                    let s = self.decode_operand(si);
                    self.set_reg(di, s);
                },
                SvK(fp_offset) => {
                    let newfp = self.fp + fp_offset as usize;
                    self.stack[newfp - 3] = From::from(self.fp as isize);
                    self.stack[newfp - 2] = From::from((self.ip + 1) as isize);
                    self.stack[newfp - 1] = unsafe {
                        mem::transmute(Box::new((*self.cl).clone()))
                    };
                    self.fp = newfp;
                },

                Fun(di, ci) => {
                    let cob = self.cl.cob.cobs[ci as usize].clone();
                    self.set_reg(di, unsafe { mem::transmute(Box::new(Closure {cob: cob })) });
                },

                IAdd(di, li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li))?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri))?;
                    self.set_reg(di, From::from(l + r));
                },
                ISub(di, li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li))?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri))?;
                    self.set_reg(di, From::from(l - r));
                },
                IMul(di, li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li))?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri))?;
                    self.set_reg(di, From::from(l * r));
                },

                ILt(li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li))?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri))?;
                    if l < r {
                        self.ip += 1;
                    }
                },

                Br(offset) => {
                    self.ip += offset as usize;
                },
                Call(argc) => {
                    self.cl = unsafe { mem::transmute(self.stack[self.fp]) };
                    self.ip = 0;
                    let keep = self.fp + argc as usize;
                    let total = self.fp + self.cl.cob.reg_req;
                    self.resize_stack(keep, total);
                },
                Ret(vi) => {
                    let oldfp = self.fp;
                    let newfp = self.load_usize(oldfp - 3)?;
                    self.stack[oldfp - 3] = self.decode_operand(vi);
                    self.fp = newfp;
                    self.ip = self.load_usize(oldfp - 2)?;
                    self.cl = unsafe { mem::transmute(self.stack[oldfp - 1]) };
                    let keep = oldfp - 2;
                    let total = self.fp + self.cl.cob.reg_req; // TODO: better estimate
                    self.resize_stack(keep, total);
                },

                Halt(ri) => return Ok(self.decode_operand(ri))
            }
        }
    }

    fn decode_operand(&self, operand: u8) -> RawRef {
        match From::from(operand) {
            Operand::Local(li) => self.stack[self.fp + li as usize],
            Operand::Const(ci) => self.cl.cob.consts[ci as usize]
        }
    }

    fn load_usize(&self, i: usize) -> Result<usize, TypeError> {
        <isize as TryFrom<RawRef>>::try_from(self.stack[i]).map(|n| n as usize)
    }

    fn set_reg(&mut self, reg_index: u8, val: RawRef) {
        self.stack[self.fp + reg_index as usize] = val;
    }

    fn resize_stack(&mut self, keep: usize, total: usize) {
        self.stack.truncate(keep);
        self.stack.resize(total, From::from(0));
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use super::{VM, CodeObject};
    use super::Instr::*;
    use super::Operand::*;
    use value::RawRef;

    // TODO: use globals

    #[test]
    fn fact() {
        let mut vm = VM::new(CodeObject {
            code: vec![
                Fun(3, 0),
                Mov(4, From::from(Const(0))),
                SvK(3),
                Call(2),
                Halt(From::from(Local(0)))
            ],
            consts: vec![From::from(5)],
            reg_req: 5,
            cobs: vec![
                CodeObject {
                    code: vec![
                        ILt(From::from(Local(1)), From::from(Const(0))),
                        Br(1),
                        Ret(From::from(Const(1))),
                        ISub(2, From::from(Local(1)), From::from(Const(1))),
                        Mov(4, From::from(Local(0))),
                        Mov(0, From::from(Local(1))),
                        Mov(5, From::from(Local(2))),
                        SvK(4),
                        Call(2),
                        IMul(2, From::from(Local(0)), From::from(Local(1))),
                        Ret(From::from(Local(2)))
                    ],
                    consts: vec![From::from(2), From::from(1)],
                    reg_req: 6,
                    cobs: vec![]
                }
            ]
        });
        assert_eq!(<isize as TryFrom<RawRef>>::try_from(vm.run().unwrap()).unwrap(), 120isize);
    }

    #[test]
    fn tailfact() {
        let mut vm = VM::new(CodeObject {
            code: vec![
                Fun(3, 0),
                Mov(4, From::from(Const(0))),
                Mov(5, From::from(Const(1))),
                SvK(3),
                Call(3),
                Halt(From::from(Local(0)))
            ],
            consts: vec![From::from(5), From::from(1)],
            reg_req: 6,
            cobs: vec![
                CodeObject {
                    code: vec![
                        ILt(From::from(Local(1)), From::from(Const(0))),
                        Br(1),
                        Ret(From::from(Local(2))),
                        ISub(3, From::from(Local(1)), From::from(Const(1))),
                        IMul(4, From::from(Local(1)), From::from(Local(2))),
                        Mov(1, From::from(Local(3))),
                        Mov(2, From::from(Local(4))),
                        Call(3)
                    ],
                    consts: vec![From::from(2), From::from(1)],
                    reg_req: 5,
                    cobs: vec![]
                }
            ]
        });
        assert_eq!(<isize as TryFrom<RawRef>>::try_from(vm.run().unwrap()).unwrap(), 120isize);
    }
}
