use std::fmt;
use std::fmt::Display;
use std::collections::HashMap;
use std::iter::Peekable;

use util::{Sourced, Name, SrcPos, IndexSrc};
use ast;
use ast::{Var, VarRef, Const, ConstVal};
use flatten;
use flatten::{FAST, Closure};

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
        for (name, p) in self.procs.iter() {
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
        write!(f, "{} -> {}", self.expr, self.next)
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
            &ContRef::Ret => write!(f, "%ret"),
            &ContRef::Halt => write!(f, "%halt")
        }
    }
}

/// A mapping from labels to continuations.
#[derive(Debug)]
pub struct ContMap(HashMap<usize, Cont>);

impl ContMap {
    fn new(expr: flatten::Expr, cont: ContRef, temp_counter: &mut IndexSrc,
        label_counter: &mut Peekable<IndexSrc>) -> ContMap
    {
        let mut res = ContMap(HashMap::new());
        if let Some(t) = res.convert_step(expr, None, Some(cont), temp_counter, label_counter) {
            res.insert(label_counter.next().unwrap(), Cont {
                param: None,
                expr: t,
                next: cont
            });
        }
        res
    }

    fn insert(&mut self, label: usize, cont: Cont) {
        self.0.insert(label, cont);
    }

    /// Conversion step. Converts `expr` and returns the converted version if it is trivial or
    /// pushes continuations to self and returns `None` if it was not. `tempname` is the name that
    /// was given to the previous nontrivial expression (if required) and `cont` the tail
    /// continuation (`ContRef::Ret` or `ContRef::Halt`) if we are in tail position.
    fn convert_step(&mut self, expr: flatten::Expr, mut tempname: Option<Name>,
        cont: Option<ContRef>, temp_counter: &mut IndexSrc, label_counter: &mut Peekable<IndexSrc>)
        -> Option<Expr>
    {
        match expr {
            flatten::Expr::Block(flatten::Block { pos, mut stmts }) => {
                // TODO: Declare variables at start of block.
                if stmts.len() > 0 {
                    let lasti = stmts.len() - 1;
                    for stmt in stmts.drain(0..lasti) {
                        match stmt {
                            ast::Stmt::Def { name, val } => { // Nontail definition:
                                self.convert_nontrivially(val, None, &mut tempname, temp_counter,
                                                          label_counter);
                                tempname = Some(name);
                            },
                            ast::Stmt::Expr(expr) => // Nontail expression:
                                if self.convert_step(expr, tempname.clone(), None, temp_counter,
                                                          label_counter).is_none() {
                                    tempname = None;
                                }
                        }
                    }
                    // TODO: What to do with `Stmt::Def`:s as last statements?
                    self.convert_nontrivially(stmts.pop().unwrap().into_expr(), cont,
                                              &mut tempname, temp_counter, label_counter);
                    None
                } else { // Empty block:
                    Some(Expr::Const(Const {
                        pos: pos,
                        val: ConstVal::Bool(false), // TODO: Return `()`
                    }))
                }
            },
            flatten::Expr::App(flatten::App { pos, box op, args }) => {
                let cop = self.convert_subexpr(op, &mut tempname, temp_counter, label_counter);
                let cargs = args.into_iter()
                                .map(|arg|
                                    self.convert_subexpr(
                                        arg, &mut tempname, temp_counter, label_counter))
                                .collect();

                let label = label_counter.next().unwrap();
                let ki = *label_counter.peek().unwrap();
                self.insert(label, Cont {
                    param: tempname,
                    expr: Expr::App(App {
                        pos: pos,
                        op: Box::new(cop),
                        args: cargs
                    }),
                    next: cont.unwrap_or(ContRef::Local(ki))
                });

                None
            },
            flatten::Expr::Closure(cl) => {
                let label = label_counter.next().unwrap();
                let ki = *label_counter.peek().unwrap();
                self.insert(label, Cont {
                    param: tempname,
                    expr: Expr::Closure(cl),
                    next: cont.unwrap_or(ContRef::Local(ki))
                });

                None
            },
            flatten::Expr::Var(v) => Some(Expr::Var(v)),
            flatten::Expr::Const(c) => Some(Expr::Const(c))
        }
    }

    /// Like `convert_step` but always returns a trivial term even if `expr` was serious.
    fn convert_subexpr(&mut self, expr: flatten::Expr, tempname: &mut Option<Name>,
        temp_counter: &mut IndexSrc, label_counter: &mut Peekable<IndexSrc>)
        -> Expr
    {
        let pos = expr.pos();
        match self.convert_step(expr, tempname.clone(), None, temp_counter, label_counter) {
            Some(t) => t,
            None => {
                *tempname = Some(Name::unique(String::from("v"), temp_counter));
                Expr::Var(Var {
                    pos: pos,
                    name: VarRef::Local(tempname.clone().unwrap())
                })
            }
        }
    }

    /// Like `convert_step` but force pushing of `Cont` instead of returning trivial term.
    fn convert_nontrivially(&mut self, expr: flatten::Expr, cont: Option<ContRef>,
        tempname: &mut Option<Name>, temp_counter: &mut IndexSrc,
        label_counter: &mut Peekable<IndexSrc>)
    {
        if let Some(t) = self.convert_step(expr, tempname.clone(), cont, temp_counter,
                                           label_counter) {
            let label = label_counter.next().unwrap();
            let ki = *label_counter.peek().unwrap();
            self.insert(label, Cont {
                param: tempname.clone(),
                expr: t,
                next: cont.unwrap_or(ContRef::Local(ki))
            });
        }
    }
}

impl Display for ContMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (label, cont) in self.0.iter() {
            writeln!(f, "{}: {}", label, cont)?;
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
