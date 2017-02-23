use std::fmt;
use std::fmt::Display;

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
    Regs(u8),

    IAdd(u8, u8, u8),
    IMul(u8, u8, u8),

    Halt
}

impl Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Instr::*;
        match self {
            &Regs(n) => write!(f, "regs {}", n),
            &IAdd(d, l, r) => write!(f, "iadd l{}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &IMul(d, l, r) => write!(f, "imul l{}, {}, {}", d, Operand::from(l), Operand::from(r)),
            &Halt => write!(f, "halt")
        }
    }
}

/// A temporary shim for closures.
#[derive(Debug)]
pub struct Closure {
    pub code: Vec<Instr>,
    pub consts: Vec<RawRef>
}

/// Proff virtual machine
#[derive(Debug)]
pub struct VM {
    cl: Closure,
    ip: usize,
    fp: usize,
    stack: Vec<RawRef>
}

impl VM {
    /// Create a new VM.
    pub fn new(fun: Closure) -> VM {
        VM {
            cl: fun,
            ip: 0,
            fp: 0,
            stack: Vec::new()
        }
    }

    /// Start the VM.
    pub fn run(&mut self) {
        use self::Instr::*;

        loop {
            let instr = self.cl.code[self.ip];
            println!("{}: {}", self.ip, instr);
            self.ip += 1;

            match instr {
                Regs(n) => {
                    let slen = self.stack.len();
                    self.stack.resize(slen + n as usize, RawRef(0));
                },

                IAdd(di, li, ri) => {
                    let l = self.decode_operand(li);
                    let r = self.decode_operand(ri);
                    self.set_reg(di, RawRef(l.0 + r.0));
                },
                IMul(di, li, ri) => {
                    let l = self.decode_operand(li);
                    let r = self.decode_operand(ri);
                    self.set_reg(di, RawRef(l.0 * r.0));
                },

                Halt => break
            }
        }
    }

    fn decode_operand(&self, operand: u8) -> RawRef {
        match From::from(operand) {
            Operand::Local(li) => self.stack[self.fp + li as usize],
            Operand::Const(ci) => self.cl.consts[ci as usize]
        }
    }

    fn set_reg(&mut self, reg_index: u8, val: RawRef) {
        self.stack[self.fp + reg_index as usize] = val;
    }
}
