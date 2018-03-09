use std::mem::transmute;
use std::hash::{Hash, Hasher};
use std::iter;
use std::collections::HashMap;

use pcws_domain::object_model::ValueRef;
use pcws_syntax::cst::{PrimOp, Const};
use anf::{Program, Function, Stmt, Expr, Triv};
use registers::Reg;
use vm::CodeObject;

// ================================================================================================

struct ConstBits(Const);

impl PartialEq for ConstBits {
    fn eq(&self, other: &ConstBits) -> bool {
        use self::Const::*;

        match (&self.0, &other.0) {
            (&Int(a), &Int(b)) => a == b,
            (&Float(a), &Float(b)) => unsafe {
                transmute::<_, isize>(a) == transmute::<_, isize>(b)
            },
            (&Char(a), &Char(b)) => a == b,
            (&Bool(a), &Bool(b)) => a == b,
            (&String(ref a), &String(ref b)) => a == b,
            (&Symbol(ref a), &String(ref b)) => a == b,
            _ => false
        }
    }
}

impl Eq for ConstBits {}

impl Hash for ConstBits {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        use self::Const::*;

        match self.0 {
            Int(n) => {
                0.hash(hasher);
                n.hash(hasher);
            },
            Float(n) => {
                1.hash(hasher);
                unsafe { transmute::<_, isize>(n) }.hash(hasher);
            },
            Char(c) => {
                2.hash(hasher);
                c.hash(hasher);
            },
            Bool(b) => {
                3.hash(hasher);
                b.hash(hasher);
            },
            String(ref s) => {
                4.hash(hasher);
                s.hash(hasher);
            },
            Symbol(ref s) => {
                5.hash(hasher);
                s.hash(hasher);
            }
        }
    }
}

// ================================================================================================

struct Assembler {
    indexed_consts: Vec<ValueRef>,
    const_indices: HashMap<ConstBits, usize>
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            indexed_consts: Vec::new(),
            const_indices: HashMap::new()
        }
    }

    fn emit<I: IntoIterator<Item=Triv<Reg>>>(&mut self, op: PrimOp, args: I) {
        unimplemented!()
    }

    fn stmt(&mut self, stmt: Stmt<Reg>) {
        match stmt {
            Stmt::Def(dest, Expr::PrimCall(_, op, args)) =>
                self.emit(op, iter::once(dest.into()).chain(args)),
            Stmt::Def(dest, Expr::Triv(_, t)) =>
                self.emit(PrimOp::Mov, iter::once(dest.into()).chain(iter::once(t))),
            Stmt::Expr(Expr::PrimCall(_, op, args)) => self.emit(op, args),
            Stmt::Expr(Expr::Triv(..)) => { /* dead code */ },
            _ => unreachable!()
        }
    }

    fn transfer(&mut self, expr: Expr<Reg>) {
        use self::Expr::*;

        match expr {
            Function(..) | Call(..) => unreachable!(),
            PrimCall(_, op, args) => {
                let dest: Reg = 0.into();
                self.emit(op, iter::once(dest.into()).chain(args));
                self.emit(PrimOp::Ret, iter::once(dest.into()));
            },
            Triv(_, t) => self.emit(PrimOp::Ret, iter::once(t))
        }
    }

    fn assemble(self) -> CodeObject {
        unimplemented!()
    }
}

// ================================================================================================¨

impl From<Function<Reg>> for CodeObject {
    fn from(f: Function<Reg>) -> CodeObject {
        let Function { pos: _, params: _, free_vars: _, stmts, expr } = f;

        let mut assembler = Assembler::new();

        for stmt in stmts { assembler.stmt(stmt) }
        assembler.transfer(expr);

        assembler.assemble()
    }
}

pub fn assemble(program: Program<Reg>) -> (Vec<CodeObject>, usize) {
    let Program { fns, entry } = program;

    let mut entry_index = 0;

    let cobs = fns.into_iter().enumerate()
                  .map(|(i, (name, f))| {
                      if name == entry { entry_index = i; }
                      f.into()
                  })
                  .collect();

    (cobs, entry_index)
}
