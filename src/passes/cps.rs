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
    App(App, ContRef),
    Next(App),
    If(Triv, ContRef, ContRef),
    Closure(Closure, ContRef),
    Triv(Triv, ContRef)
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Expr::App(ref app, k) => write!(f, "{} -> {}", app, k),
            &Expr::Next(ref app) => write!(f, "{} -> ret", app),
            &Expr::If(ref app, k, l) => write!(f, "{} -> {} | {}", app, k, l),
            &Expr::Closure(ref cl, k) => write!(f, "{} -> {}", cl, k),
            &Expr::Triv(ref t, k) => write!(f, "{} -> {}", t, k)
        }
    }
}

#[derive(Debug)]
pub enum Triv {
    Var(Var),
    Const(Const)
}

impl Display for Triv {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Triv::Var(ref v) => v.fmt(f),
            &Triv::Const(ref c) => c.fmt(f)
        }
    }
}

pub type App = ast::App<Triv>;

pub type Fun = flatten::Fun<Clause>;

#[derive(Debug)]
pub struct Clause {
    pub pos: SrcPos,
    pub params: Name, // TODO: Multiple params
    pub body: ContMap,
}

impl Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} =>\n{}", self.params, self.body)
    }
}

/// Continuation.
#[derive(Debug)]
pub struct Cont {
    pub param: Option<Name>,
    pub expr: Expr
}

impl Display for Cont {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for param in self.param.iter() {
            write!(f, "{} ", param)?;
        }
        write!(f, "= {}", self.expr)
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

    fn insert(&mut self, label: usize, tempname: Option<Name>, expr: Expr) {
        self.conts.insert(label, Cont {
            param: tempname,
            expr: expr
        });
    }

    fn convert_step(&mut self, label: usize, mut tempname: Option<Name>, expr: flatten::Expr,
                    cont: ContRef) -> Either<(Triv, Option<Name>), bool> {
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
                    Left((Triv::Const(Const {
                        pos: pos,
                        val: ConstVal::Bool(false) // TODO: Return `()`
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
                }, cont));
                Right(true)
            },
            flatten::Expr::Closure(cl) => {
                self.insert(label, tempname, Expr::Closure(cl, cont));
                Right(true)
            },
            flatten::Expr::Var(v) => Left((Triv::Var(v), tempname)),
            flatten::Expr::Const(c) => Left((Triv::Const(c), tempname))
        }
    }

    /// Like `convert_step` but always returns a trivial term even if `expr` was serious.
    fn convert_subexpr(&mut self, label: usize, tempname: Option<Name>, expr: flatten::Expr,
                       cont: ContRef) -> (Triv, bool, Option<Name>) {
        use util::Either::*;
        let pos = expr.pos();
        match self.convert_step(label, tempname, expr, cont) {
            Left((t, n)) => (t, false, n),
            Right(true) => {
                let name = Name::fresh(String::from("v"));
                (Triv::Var(Var {
                     pos: pos,
                     name: VarRef::Local(name.clone()),
                 }), true, Some(name))
            },
            Right(false) =>
                (Triv::Const(Const {
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
                self.insert(label, n, Expr::Triv(t, cont));
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
    fn from(flatten::Clause { pos, params, cond, body}: flatten::Clause) -> Clause {
        fn convert_body(conts: &mut ContMap, stmts: Vec<flatten::Stmt>) -> usize {
            let bk = fresh_label();
            conts.convert_nontrivially(bk, None, flatten::Expr::Block(flatten::Block {
                pos: stmts[0].pos(),
                stmts: stmts
            }), ContRef::Ret);
            bk
        }

        fn convert_next(conts: &mut ContMap, pos: SrcPos, params: Name) -> usize {
            let k = fresh_label();
            conts.insert(k, None, Expr::Next(App {
                pos: pos,
                op: Box::new(Triv::Var(Var {
                    pos: pos,
                    name: VarRef::Local(Name::fresh(String::from("self"))) // FIXME
                })),
                args: vec![Triv::Var(Var {
                    pos: pos,
                    name: VarRef::Local(params)
                })]
            }));
            k
        }

        let entry = fresh_label();
        let mut cbody = ContMap {
            entry: entry,
            conts: HashMap::new()
        };

        match cond {
            flatten::Expr::Const(Const { val: ConstVal::Bool(true), pos: _ }) => {
                cbody.entry = convert_body(&mut cbody, body);
            },
            flatten::Expr::Const(Const { val: ConstVal::Bool(false), pos }) => {
                cbody.entry = convert_next(&mut cbody, pos, params.clone());
            },
            _ => {
                let ck = fresh_label();
                let (ccond, lused, tempname) =
                    cbody.convert_subexpr(entry, None, cond, ContRef::Local(ck));
                if !lused {
                    push_label(entry);
                    cbody.entry = ck;
                }
                let bk = convert_body(&mut cbody, body);
                let nk = convert_next(&mut cbody, pos, params.clone());
                cbody.insert(ck, tempname, Expr::If(ccond, ContRef::Local(bk), ContRef::Local(nk)));
            }
        }

        Clause {
            pos: pos,
            params: params,
            body: cbody
        }
    }
}
