use std::convert::TryFrom;
use std::mem;

use gc::{SimpleCollector, Allocator};
use value::{Header, RawRef, TypedRef, Closure, CodeObject, TypeError, BoundsError};
use util::ProffError;
use bytecode;
use bytecode::{PackedInstr, Operand};

/// Proff virtual machine
#[derive(Debug)]
pub struct VM {
    cl: TypedRef<Closure>,
    ip: usize,
    fp: usize,
    stack: Vec<RawRef>,
    heap: SimpleCollector<Header, RawRef>
}

impl VM{
    /// Create a new VM.
    pub fn new(mut mem: SimpleCollector<Header, RawRef>, fun: TypedRef<CodeObject>) -> VM {
        let stacksize: isize = From::from(fun.reg_req);
        VM {
            cl: From::from(mem.alloc_sized_pointy(Closure { cob: fun })),
            ip: 0,
            fp: 0,
            stack: vec![Default::default(); stacksize as usize],
            heap: SimpleCollector::new(1024, 1024)
        }
    }

    /// Start the VM.
    pub fn run(&mut self) -> Result<RawRef, ProffError> {
        use bytecode::PackedInstr::*;

        loop {
            let instr: PackedInstr = unsafe { self.cl.cob.code.get(self.ip)? };
            println!("{} [{}]: {}", self.stack.len(), self.ip, bytecode::Instr::from(instr));
            self.ip += 1;

            match instr {
                Mov(di, si) => {
                    let s = self.decode_operand(si)?;
                    self.set_reg(di, s);
                },
                SvK(fp_offset) => {
                    let newfp = self.fp + fp_offset as usize;
                    self.stack[newfp - 3] = From::from(self.fp as isize);
                    self.stack[newfp - 2] = From::from((self.ip + 1) as isize);
                    self.stack[newfp - 1] = From::from(self.cl.clone());
                    self.fp = newfp;
                },

                Fun(di, ci) => {
                    let cob = From::from(unsafe { self.cl.cob.cobs.get(ci as usize)?.clone() });
                    let cl = self.heap.alloc_sized_pointy(Closure {cob: cob });
                    self.set_reg(di, cl);
                },

                IAdd(di, li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li)?)?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri)?)?;
                    self.set_reg(di, From::from(l + r));
                },
                ISub(di, li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li)?)?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri)?)?;
                    self.set_reg(di, From::from(l - r));
                },
                IMul(di, li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li)?)?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri)?)?;
                    self.set_reg(di, From::from(l * r));
                },

                ILt(li, ri) => {
                    let l: isize = TryFrom::try_from(self.decode_operand(li)?)?;
                    let r: isize = TryFrom::try_from(self.decode_operand(ri)?)?;
                    if l < r {
                        self.ip += 1;
                    }
                },

                Br(offset) => {
                    self.ip += offset as usize;
                },
                Call(argc) => {
                    self.cl = From::from(self.stack[self.fp]);
                    self.ip = 0;
                    let keep = self.fp + argc as usize;
                    let total = self.fp +
                        <isize as From<TypedRef<isize>>>::from(self.cl.cob.reg_req) as usize;
                    self.resize_stack(keep, total);
                },
                Ret(vi) => {
                    let oldfp = self.fp;
                    let newfp = self.load_usize(oldfp - 3)?;
                    self.stack[oldfp - 3] = self.decode_operand(vi)?;
                    self.fp = newfp;
                    self.ip = self.load_usize(oldfp - 2)?;
                    self.cl = unsafe { mem::transmute(self.stack[oldfp - 1]) };
                    let keep = oldfp - 2;
                    let total = self.fp +  // TODO: better estimate
                        <isize as From<TypedRef<isize>>>::from(self.cl.cob.reg_req) as usize;
                    self.resize_stack(keep, total);
                },

                Halt(ri) => return Ok(self.decode_operand(ri)?)
            }
        }
    }

    fn decode_operand(&self, operand: u8) -> Result<RawRef, BoundsError> {
        match From::from(operand) {
            Operand::Local(li) => Ok(self.stack[self.fp + li as usize]),
            Operand::Const(ci) => unsafe { self.cl.cob.consts.get(ci as usize) }
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

// ------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use super::VM;
    use gc::SimpleCollector;
    use ast::ConstVal;
    use bytecode::Assembler;
    use bytecode::Instr::*;
    use bytecode::Operand::*;
    use value::{Header, RawRef};

    // TODO: use globals

    #[test]
    fn fact() {
        let mut fact_asm = Assembler::new();
        fact_asm.extend_code(vec![
            ILt(Local(1), Const(0)),
            Br(1),
            Ret(Const(1)),
            ISub(2, Local(1), Const(1)),
            Mov(4, Local(0)),
            Mov(0, Local(1)),
            Mov(5, Local(2)),
            SvK(4),
            Call(2),
            IMul(2, Local(0), Local(1)),
            Ret(Local(2))
        ].into_iter());
        fact_asm.push_const(ConstVal::Int(2));
        fact_asm.push_const(ConstVal::Int(1));

        let mut main_asm = Assembler::new();
        main_asm.extend_code(vec![
            Fun(3, 0),
            Mov(4, Const(0)),
            SvK(3),
            Call(2),
            Halt(Local(0))
        ].into_iter());
        main_asm.push_const(ConstVal::Int(5));
        main_asm.push_child(fact_asm);

        let mut heap = SimpleCollector::<Header, RawRef>::new(1024, 1024);
        let main = main_asm.assemble(&mut heap);
        // let mut vm = VM::new(heap, main);
        //
        // assert_eq!(<isize as TryFrom<RawRef>>::try_from(vm.run().unwrap()).unwrap(), 120isize);
    }

    #[test]
    fn tailfact() {
        let mut fact_asm = Assembler::new();
        fact_asm.extend_code(vec![
            ILt(Local(1), Const(0)),
            Br(1),
            Ret(Local(2)),
            ISub(3, Local(1), Const(1)),
            IMul(4, Local(1), Local(2)),
            Mov(1, Local(3)),
            Mov(2, Local(4)),
            Call(3)
        ].into_iter());
        fact_asm.push_const(ConstVal::Int(2));
        fact_asm.push_const(ConstVal::Int(1));

        let mut main_asm = Assembler::new();
        main_asm.extend_code(vec![
            Fun(3, 0),
            Mov(4, Const(0)),
            Mov(5, Const(1)),
            SvK(3),
            Call(3),
            Halt(Local(0))
        ].into_iter());
        main_asm.push_const(ConstVal::Int(5));
        main_asm.push_const(ConstVal::Int(1));
        main_asm.push_child(fact_asm);

        // let mut heap = SimpleCollector::new(1024, 1024);
        // let main = main_asm.assemble(&mut heap);
        // let mut vm = VM::new(heap, main);
        //
        // assert_eq!(<isize as TryFrom<RawRef>>::try_from(vm.run().unwrap()).unwrap(), 120isize);
    }
}
