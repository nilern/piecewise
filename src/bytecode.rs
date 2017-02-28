use std::fmt;
use std::fmt::Display;
use std::cmp::max;

use ast::ConstVal;
use gc::Allocator;
use value::{Header, RawRef, TypedRef, CodeObject, ByteArray, Tuple};

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

    fn reg_req(&self) -> Option<u8> {
        match self {
            &Operand::Local(i) => Some(i + 1),
            &Operand::Const(..) => None
        }
    }
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

/// Unpacked virtual instruction
#[derive(Debug, Clone, Copy)]
pub enum Instr {
    Mov(u8, Operand),
    SvK(u16),

    Fun(u8, u16),

    IAdd(u8, Operand, Operand),
    ISub(u8, Operand, Operand),
    IMul(u8, Operand, Operand),

    ILt(Operand, Operand),

    Br(u16),
    Call(u16),
    Ret(Operand),

    Halt(Operand)
}

impl Instr {
    fn reg_req(&self) -> Option<u8> {
        use self::Instr::*;
        match self {
            &SvK(..) | &Fun(..) | &Br(..) | &Call(..) => None,

            &Mov(dest, si) => Some(max(si.reg_req().unwrap_or(0), dest)),

            &IAdd(di, li, ri) | &ISub(di, li, ri) | &IMul(di, li, ri) =>
                Some(max(max(li.reg_req().unwrap_or(0),
                             ri.reg_req().unwrap_or(0)),
                         di)),

            &ILt(li, ri) =>
                li.reg_req().or_else(|| ri.reg_req()),

            &Ret(ri) | &Halt(ri) => ri.reg_req()
        }
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Instr::*;
        match self {
            &Mov(dest, src) => write!(f, "mov  l{}, {}", dest, src),
            &SvK(fp_offset) => write!(f, "svk  {}", fp_offset),
            &Fun(d, i) => write!(f, "fun  l{}, {}", d, i),
            &IAdd(d, l, r) => write!(f, "iadd l{}, {}, {}", d, l, r),
            &ISub(d, l, r) => write!(f, "isub l{}, {}, {}", d, l, r),
            &IMul(d, l, r) => write!(f, "imul l{}, {}, {}", d, l, r),
            &ILt(l, r) => write!(f, "ilt  {}, {}", l, r),
            &Br(offset) => write!(f, "br   {}", offset),
            &Call(argc) => write!(f, "call {}", argc),
            &Ret(v) => write!(f, "ret  {}", v),
            &Halt(ri) => write!(f, "halt {}", ri)
        }
    }
}

// ------------------------------------------------------------------------------------------------

/// A packed instruction.
#[derive(Debug, Clone, Copy)]
pub enum PackedInstr {
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

impl From<Instr> for PackedInstr {
    fn from(instr: self::Instr) -> PackedInstr {
        use self::Instr::*;
        match instr {
            Mov(dest, src) => PackedInstr::Mov(dest, From::from(src)),
            SvK(fp_offset) => PackedInstr::SvK(fp_offset),

            Fun(dest, src) => PackedInstr::Fun(dest, src),

            IAdd(dest, l, r) => PackedInstr::IAdd(dest, From::from(l), From::from(r)),
            ISub(dest, l, r) => PackedInstr::ISub(dest, From::from(l), From::from(r)),
            IMul(dest, l, r) => PackedInstr::IMul(dest, From::from(l), From::from(r)),

            ILt(l, r) => PackedInstr::ILt(From::from(l), From::from(r)),

            Br(offset) => PackedInstr::Br(offset),
            Call(argc) => PackedInstr::Call(argc),
            Ret(src) => PackedInstr::Ret(From::from(src)),

            Halt(src) => PackedInstr::Ret(From::from(src)),
        }
    }
}

impl From<PackedInstr> for Instr {
    fn from(instr: PackedInstr) -> Instr {
        use self::PackedInstr::*;
        match instr {
            Mov(dest, src) => Instr::Mov(dest, From::from(src)),
            SvK(fp_offset) => Instr::SvK(fp_offset),

            Fun(dest, src) => Instr::Fun(dest, src),

            IAdd(dest, l, r) => Instr::IAdd(dest, From::from(l), From::from(r)),
            ISub(dest, l, r) => Instr::ISub(dest, From::from(l), From::from(r)),
            IMul(dest, l, r) => Instr::IMul(dest, From::from(l), From::from(r)),

            ILt(l, r) => Instr::ILt(From::from(l), From::from(r)),

            Br(offset) => Instr::Br(offset),
            Call(argc) => Instr::Call(argc),
            Ret(src) => Instr::Ret(From::from(src)),

            Halt(src) => Instr::Ret(From::from(src)),
        }
    }
}

// ------------------------------------------------------------------------------------------------

pub struct Assembler {
    code: Vec<Instr>,
    consts: Vec<ConstVal>,
    cobs: Vec<Assembler>,
    reg_req: u8
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler {
            code: Vec::new(),
            consts: Vec::new(),
            cobs: Vec::new(),
            reg_req: 0
        }
    }

    pub fn push_instr(&mut self, instr: Instr) {
        self.reg_req = max(self.reg_req, instr.reg_req().unwrap_or(0));
        self.code.push(instr);
    }

    pub fn extend_code<I>(&mut self, instrs: I) where I: Iterator<Item=Instr> {
        self.code.extend(instrs);
    }

    pub fn push_const(&mut self, c: ConstVal) {
        self.consts.push(c);
    }

    pub fn push_child(&mut self, child: Assembler) {
        self.cobs.push(child);
    }

    pub fn assemble<A>(self, heap: &mut A) -> TypedRef<CodeObject>
        where A: Allocator<Header=Header, Slot=RawRef>
    {
        let code = unsafe { ByteArray::from_iter(heap, self.code.into_iter()) };
        let consts = unsafe {
            let crefs: Vec<RawRef> = self.consts.into_iter().map(|c| RawRef::from_const(heap, c)).collect();
            Tuple::from_iter(heap, crefs.into_iter())
        };
        let cobs = unsafe {
            let crefs: Vec<RawRef> = self.cobs.into_iter()
                                              .map(|cob| From::from(cob.assemble(heap)))
                                              .collect();
            Tuple::from_iter(heap, crefs.into_iter())
        };
        let reg_req: TypedRef<isize> = From::from(self.reg_req as isize);
        unsafe {
            <TypedRef<CodeObject> as From<RawRef>>::from(heap.alloc_sized_pointy(CodeObject {
                code: code,
                consts: consts,
                cobs: cobs,
                reg_req: reg_req
            }))
        }
    }
}
