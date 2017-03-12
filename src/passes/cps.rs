use std::fmt;
use std::fmt::Display;
use std::collections::HashMap;

use util::{Sourced, Name, SrcPos, Either, fresh_label, push_label};
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

impl From<FAST> for CPS {
    fn from(fast: FAST) -> CPS {
        CPS {
            procs: fast.procs.into_iter()
                             .map(|(name, f)| (name, Fun::from(f)))
                             .collect(),
            body: ContMap::new(fast.expr, ContRef::Halt)
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
    fn new(expr: flatten::Expr, cont: ContRef) -> ContMap {
        let k = fresh_label();
        let mut res = ContMap {
            entry: k,
            conts: HashMap::new()
        };
        let _ = res.convert_nontrivially(k, None, expr, cont);
        res
    }

    fn insert(&mut self, label: usize, tempname: Option<Name>, expr: Expr, cont: ContRef) {
        self.conts.insert(label, Cont {
            param: tempname,
            expr: expr,
            next: cont
        });
    }

    fn convert_step(&mut self, label: usize, mut tempname: Option<Name>, expr: flatten::Expr,
                    cont: ContRef) -> Either<(Expr, Option<Name>), bool> {
        use util::Either::*;
        match expr {
            flatten::Expr::Block(flatten::Block { pos, mut stmts }) => {
                // TODO: Declare variables at start of block.
                if stmts.len() > 0 {
                    let mut oldk = label;
                    let mut k = None;
                    let lasti = stmts.len() - 1;
                    for stmt in stmts.drain(0..lasti) {
                        k = Some(fresh_label());
                        match stmt {
                            ast::Stmt::Def { name, val } => { // Nontail definition:
                                let _ = self.convert_nontrivially(oldk, tempname, val, ContRef::Local(k.unwrap()));
                                oldk = k.unwrap();
                                tempname = Some(name);
                            },
                            ast::Stmt::Expr(expr) => // Nontail expression:
                                tempname = match self.convert_step(oldk, tempname, expr, ContRef::Local(k.unwrap()))
                                {
                                    Left((_, n)) => {
                                        push_label(k.unwrap());
                                        k = None;
                                        n // treat as dead code
                                    },
                                    Right(_) => {
                                        oldk = k.unwrap();
                                        None
                                    }
                                }
                        }
                    }
                    // TODO: What to do with `Stmt::Def`:s as last statements?
                    Right(self.convert_nontrivially(oldk, tempname, stmts.pop().unwrap().into_expr(), cont))
                } else { // Empty block:
                    Left((Expr::Const(Const {
                        pos: pos,
                        val: ConstVal::Bool(false), // TODO: Return `()`
                    }), tempname))
                }
            },
            flatten::Expr::App(flatten::App { pos, box op, args }) => {
                let mut oldk = label;
                let mut k = Some(fresh_label());
                let (cop, lused, mut tempname) = self.convert_subexpr(oldk, tempname, op, ContRef::Local(k.unwrap()));
                if lused {
                    oldk = k.unwrap();
                } else {
                    push_label(k.unwrap());
                    k = None;
                }
                let mut cargs = Vec::new();
                for arg in args {
                    k = Some(fresh_label());
                    let (carg, lused, tn) = self.convert_subexpr(oldk, tempname.take(), arg, ContRef::Local(k.unwrap()));
                    if lused {
                        oldk = k.unwrap();
                    } else {
                        push_label(k.unwrap());
                        k = None;
                    }
                    tempname = tn;
                    cargs.push(carg);
                }

                self.insert(oldk, tempname, Expr::App(App {
                    pos: pos,
                    op: Box::new(cop),
                    args: cargs
                }), cont);
                Right(true)
            },
            flatten::Expr::Closure(cl) => {
                self.insert(label, tempname, Expr::Closure(cl), cont);
                Right(true)
            },
            flatten::Expr::Var(v) => Left((Expr::Var(v), tempname)),
            flatten::Expr::Const(c) => Left((Expr::Const(c), tempname))
        }
    }

    /// Like `convert_step` but always returns a trivial term even if `expr` was serious.
    fn convert_subexpr(&mut self, label: usize, tempname: Option<Name>, expr: flatten::Expr,
                       cont: ContRef) -> (Expr, bool, Option<Name>) {
        use util::Either::*;
        let pos = expr.pos();
        match self.convert_step(label, tempname, expr, cont) {
            Left((t, n)) => (t, false, n),
            Right(true) => {
                let name = Name::fresh(String::from("v"));
                (Expr::Var(Var {
                     pos: pos,
                     name: VarRef::Local(name.clone()),
                 }), true, Some(name))
            },
            Right(false) =>
                (Expr::Const(Const {
                     pos: pos,
                     val: ConstVal::Bool(false) // TODO: return `()`
                 }), true, None)
        }
    }

    /// Like `convert_step` but force pushing of `Cont` instead of returning trivial term.
    fn convert_nontrivially(&mut self, label: usize, tempname: Option<Name>, expr: flatten::Expr,
                            cont: ContRef) -> bool {
        use util::Either::*;
        match self.convert_step(label, tempname, expr, cont) {
            Left((t, n)) => {
                self.insert(label, n, t, cont);
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
    pub fn to_cps(self) -> CPS {
        CPS::from(self)
    }
}

impl From<flatten::Fun<flatten::Clause>> for Fun {
    fn from(fun: flatten::Fun<flatten::Clause>) -> Fun {
        Fun {
            pos: fun.pos,
            freevars: fun.freevars,
            clauses: fun.clauses.into_iter()
                                .map(|clause| Clause::from(clause))
                                .collect()
        }
    }
}

impl From<flatten::Clause> for Clause {
    fn from(clause: flatten::Clause) -> Clause {
        Clause {
            pos: clause.pos,
            params: clause.params,
            cond: ContMap::new(clause.cond, ContRef::Ret),
            body: ContMap::new(flatten::Expr::Block(ast::Block {
                pos: clause.body[0].pos(),
                stmts: clause.body
            }), ContRef::Ret)
        }
    }
}
