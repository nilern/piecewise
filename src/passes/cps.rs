use std::fmt;
use std::fmt::Display;
use std::collections::HashMap;
use std::iter::Peekable;

use util::{Sourced, Name, SrcPos, IndexSrc, Either};
use ast;
use ast::{Var, VarRef, Const, ConstVal};
use passes::flatten;
use passes::flatten::{FAST, Closure};

// TODO: Differentiate trivial/serious exprs on the type level.

#[derive(Debug)]
pub struct CPS {
    procs: HashMap<Name, Fun>,
    body: ContMap
}

impl CPS {
    pub fn from_fast(fast: FAST, temp_counter: &mut IndexSrc,
        label_counter: &mut Peekable<IndexSrc>)
        -> CPS
    {
        CPS {
            procs: fast.procs.into_iter().map(|(name, f)|
                (name, f.to_cps(temp_counter, label_counter))
            ).collect(),
            body: ContMap::new(fast.expr, ContRef::Halt, temp_counter, label_counter)
        }
    }
}

impl Display for CPS {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut procvec: Vec<(&Name, &Fun)> = self.procs.iter().collect();
        procvec.sort_by_key(|&(n, _)| n);

        for (name, p) in procvec {
            write!(f, "{} {}\n\n", name, p)?;
        }
        write!(f, "{}", self.body)
    }
}

#[derive(Debug)]
pub enum Expr {
    App(App),
    Closure(Closure),

    Var(Var),
    Const(Const)
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Expr::App(ref app) => app.fmt(f),
            &Expr::Closure(ref cl) => cl.fmt(f),
            &Expr::Var(ref v) => v.fmt(f),
            &Expr::Const(ref c) => c.fmt(f)
        }
    }
}

pub type App = ast::App<Expr>;

pub type Fun = flatten::Fun<Clause>;

#[derive(Debug)]
pub struct Clause {
    pub pos: SrcPos,
    pub params: Name, // TODO: Multiple params
    pub cond: ContMap,
    pub body: ContMap,
}

impl Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "{} | {} =>", self.params, self.cond)?;
        self.body.fmt(f)
    }
}

/// Continuation.
#[derive(Debug)]
pub struct Cont {
    pub param: Option<Name>,
    pub expr: Expr,
    pub next: ContRef
}

impl Display for Cont {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for param in self.param.iter() {
            write!(f, "{} ", param)?;
        }
        write!(f, "= {} -> {}", self.expr, self.next)
    }
}

/// Continuation reference (similar to variable reference)
#[derive(Debug, Clone, Copy)]
pub enum ContRef {
    Local(usize),
    Ret,
    Halt
}

impl Display for ContRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ContRef::Local(i) => write!(f, "k[{}]", i),
            &ContRef::Ret => write!(f, "ret"),
            &ContRef::Halt => write!(f, "halt")
        }
    }
}

/// A mapping from labels to continuations.
#[derive(Debug)]
pub struct ContMap {
    entry: usize,
    conts: HashMap<usize, Cont>
}

impl ContMap {
    fn new(expr: flatten::Expr, cont: ContRef, temp_counter: &mut IndexSrc,
        label_counter: &mut Peekable<IndexSrc>) -> ContMap
    {
        let mut res = ContMap {
            entry: *label_counter.peek().unwrap(),
            conts: HashMap::new()
        };
        let _ = res.convert_nontrivially(expr, Some(cont), None, temp_counter, label_counter);
        res
    }

    fn insert(&mut self, label_counter: &mut Peekable<IndexSrc>, tempname: Option<Name>,
              expr: Expr, cont: Option<ContRef>)
    {
        let label = label_counter.next().unwrap();
        let ki = *label_counter.peek().unwrap();
        self.conts.insert(label, Cont {
            param: tempname,
            expr: expr,
            next: cont.unwrap_or(ContRef::Local(ki))
        });
    }

    fn convert_step(&mut self, expr: flatten::Expr, mut tempname: Option<Name>,
        cont: Option<ContRef>, temp_counter: &mut IndexSrc, label_counter: &mut Peekable<IndexSrc>)
        -> Either<(Expr, Option<Name>), bool>
    {
        use util::Either::*;
        match expr {
            flatten::Expr::Block(flatten::Block { pos, mut stmts }) => {
                // TODO: Declare variables at start of block.
                if stmts.len() > 0 {
                    let lasti = stmts.len() - 1;
                    for stmt in stmts.drain(0..lasti) {
                        match stmt {
                            ast::Stmt::Def { name, val } => { // Nontail definition:
                                let _ = self.convert_nontrivially(val, None, tempname,
                                                                  temp_counter, label_counter);
                                tempname = Some(name);
                            },
                            ast::Stmt::Expr(expr) => // Nontail expression:
                                tempname = match self.convert_step(expr, tempname, None,
                                                                   temp_counter, label_counter)
                                {
                                    Left((_, n)) => n, // treat as dead code
                                    Right(_) => None
                                }
                        }
                    }
                    // TODO: What to do with `Stmt::Def`:s as last statements?
                    Right(self.convert_nontrivially(stmts.pop().unwrap().into_expr(), cont,
                                                    tempname, temp_counter, label_counter))
                } else { // Empty block:
                    Left((Expr::Const(Const {
                        pos: pos,
                        val: ConstVal::Bool(false), // TODO: Return `()`
                    }), tempname))
                }
            },
            flatten::Expr::App(flatten::App { pos, box op, args }) => {
                let (cop, mut tempname) = self.convert_subexpr(op, tempname, temp_counter,
                                                               label_counter);
                let mut cargs = Vec::new();
                for arg in args {
                    let (carg, tn) = self.convert_subexpr(arg, tempname.take(), temp_counter,
                                                          label_counter);
                    tempname = tn;
                    cargs.push(carg);
                }

                self.insert(label_counter, tempname, Expr::App(App {
                    pos: pos,
                    op: Box::new(cop),
                    args: cargs
                }), cont);
                Right(true)
            },
            flatten::Expr::Closure(cl) => {
                self.insert(label_counter, tempname, Expr::Closure(cl), cont);
                Right(true)
            },
            flatten::Expr::Var(v) => Left((Expr::Var(v), tempname)),
            flatten::Expr::Const(c) => Left((Expr::Const(c), tempname))
        }
    }

    /// Like `convert_step` but always returns a trivial term even if `expr` was serious.
    fn convert_subexpr(&mut self, expr: flatten::Expr, tempname: Option<Name>,
        temp_counter: &mut IndexSrc, label_counter: &mut Peekable<IndexSrc>)
        -> (Expr, Option<Name>)
    {
        use util::Either::*;
        let pos = expr.pos();
        match self.convert_step(expr, tempname, None, temp_counter, label_counter) {
            Left(tn) => tn,
            Right(true) => {
                let name = Name::unique(String::from("v"), temp_counter);
                (Expr::Var(Var {
                     pos: pos,
                     name: VarRef::Local(name.clone()),
                 }), Some(name))
            },
            Right(false) =>
                (Expr::Const(Const {
                     pos: pos,
                     val: ConstVal::Bool(false) // TODO: return `()`
                 }), None)
        }
    }

    /// Like `convert_step` but force pushing of `Cont` instead of returning trivial term.
    fn convert_nontrivially(&mut self, expr: flatten::Expr, cont: Option<ContRef>,
        tempname: Option<Name>, temp_counter: &mut IndexSrc,
        label_counter: &mut Peekable<IndexSrc>) -> bool
    {
        use util::Either::*;
        match self.convert_step(expr, tempname, cont, temp_counter, label_counter) {
            Left((t, n)) => {
                self.insert(label_counter, n, t, cont);
                true
            }
            Right(b) => b
        }
    }
}

impl Display for ContMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut contv: Vec<(&usize, &Cont)> = self.conts.iter().collect();
        contv.sort_by_key(|&(&l, _)| l);

        for (label, cont) in contv {
            writeln!(f, "k[{}] {}", label, cont)?;
        }
        Ok(())
    }
}

// ------------------------------------------------------------------------------------------------

impl FAST {
    pub fn to_cps(self, temp_counter: &mut IndexSrc, label_counter: &mut Peekable<IndexSrc>)
        -> CPS
    {
        CPS::from_fast(self, temp_counter, label_counter)
    }
}

impl flatten::Fun<flatten::Clause> {
    fn to_cps(self, temp_counter: &mut IndexSrc, label_counter: &mut Peekable<IndexSrc>)
        -> Fun
    {
        Fun {
            pos: self.pos,
            freevars: self.freevars,
            clauses: self.clauses.into_iter().map(|clause|
                clause.to_cps(temp_counter, label_counter)).collect()
        }
    }
}

impl flatten::Clause {
    fn to_cps(self, temp_counter: &mut IndexSrc, label_counter: &mut Peekable<IndexSrc>)
        -> Clause
    {
        Clause {
            pos: self.pos,
            params: self.params,
            cond: ContMap::new(self.cond, ContRef::Ret, temp_counter, label_counter),
            body: ContMap::new(flatten::Expr::Block(ast::Block {
                pos: self.body[0].pos(),
                stmts: self.body
            }), ContRef::Ret, temp_counter, label_counter)
        }
    }
}
