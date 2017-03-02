use std::fmt;
use std::fmt::Display;

use util::{SrcPos, Sourced};
use ast;
use ast::{AST, Block, Stmt, Var, VarRef, Const, ConstVal, Name};

// TODO: Use proper counters for temp name and continuation label generation. For temp vars use the
//       same one as the alphatization pass to guarantee global uniqueness.
// TODO: Make `Display` output readable (with indentation, probably)
// TODO: Differentiate trivial/serious exprs on the type level.

/// Continuation Passing AST
#[derive(Debug)]
pub enum CPS {
    Fn(Fn),
    App(App),
    Continue(Continue),

    Var(Var),
    Const(Const)
}

impl Display for CPS {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &CPS::Fn(ref fun) => fun.fmt(f),
            &CPS::App(ref app) => app.fmt(f),
            &CPS::Continue(ref c) => c.fmt(f),

            &CPS::Var(ref v) => v.fmt(f),
            &CPS::Const(ref c) => c.fmt(f)
        }
    }
}

/// Function expression
#[derive(Debug)]
pub struct Fn {
    pub pos: SrcPos,
    pub clauses: Vec<Clause>
}

impl Display for Fn {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "{{"));
        let mut it = self.clauses.iter();
        if let Some(arg) = it.next() {
            try!(write!(f, "{}", arg));
        }
        for arg in it {
            try!(write!(f, "; {}", arg));
        }
        write!(f, "}}")
    }
}

impl From<ast::Fn> for Fn {
    fn from(ast::Fn { pos, clauses }: ast::Fn) -> Fn {
        Fn {
            pos: pos,
            clauses: clauses.into_iter().map(From::from).collect()
        }
    }
}

/// Function application
#[derive(Debug)]
pub struct App {
    pub pos: SrcPos,
    pub op: Box<CPS>,
    pub args: Vec<CPS>,
    pub cont: Box<ContRef>
}

impl Display for App {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "({} ", self.op));
        let mut it = self.args.iter();
        if let Some(arg) = it.next() {
            try!(write!(f, "{}", arg));
        }
        for arg in it {
            try!(write!(f, " {}", arg));
        }
        write!(f, " {})", self.cont)
    }
}

/// Continuation application
#[derive(Debug)]
pub struct Continue {
    cont: ContRef,
    args: Box<CPS> // TODO: Vec<CPS>
}

impl Display for Continue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {}", self.cont, self.args)
    }
}

/// Function clause
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

impl From<ast::Clause> for Clause {
    fn from(ast::Clause { pos, params, cond, body }: ast::Clause) -> Clause {
        Clause {
            pos: pos,
            params: params,
            cond: ContMap::new(cond, ContRef::Ret),
            body: ContMap::new(AST::Block(ast::Block { pos: pos, stmts: body }), ContRef::Ret)
        }
    }
}

/// A mapping from labels to continuations.
#[derive(Debug)]
pub struct ContMap(Vec<Cont>);

impl ContMap {
    /// Create a new `ContMap` that ultimately continues with `cont` via conversion from `expr`.
    pub fn new(expr: AST, cont: ContRef) -> ContMap {
        let mut res = ContMap(Vec::new());
        if let Some(t) = res.convert_step(expr, None, Some(cont)) {
            res.0.push(Cont {
                param: None,
                expr: CPS::Continue(Continue {
                    cont: cont,
                    args: Box::new(t)
                })
            })
        }
        res
    }

    /// What will be the next label?
    fn peek_label(&self) -> usize {
        self.0.len() + 1
    }

    /// Conversion step. Converts `expr` and returns the converted version if it is trivial or
    /// pushes continuations and returns `None` if it was not. `tempname` is the name that was
    /// given to the previous nontrivial expression (if required) and `cont` the tail continuation
    /// (`ContRef::Ret` or `ContRef::Halt`) if we are in tail position.
    fn convert_step(&mut self, expr: AST, mut tempname: Option<String>, cont: Option<ContRef>)
        -> Option<CPS>
    {
        match expr {
            AST::Block(Block { pos, mut stmts }) => {
                // TODO: Declare variables at start of block.
                if stmts.len() > 0 {
                    let lasti = stmts.len() - 1;
                    for stmt in stmts.drain(0..lasti) {
                        match stmt {
                            Stmt::Def { name, val } => { // Nontail definition:
                                self.convert_nontrivially(val, None, &mut tempname);
                                tempname = Some(name);
                            },
                            Stmt::Expr(expr) => // Nontail expression:
                                if self.convert_step(expr, tempname.clone(), None).is_none() {
                                    tempname = None;
                                }
                        }
                    }
                    // TODO: What to do with `Stmt::Def`:s as last statements?
                    self.convert_nontrivially(stmts.pop().unwrap().into_expr(), cont,
                                              &mut tempname);
                    None
                } else { // Empty block:
                    Some(CPS::Const(Const {
                        pos: pos,
                        val: ConstVal::Bool(false), // TODO: Return `()`
                    }))
                }
            },
            AST::App(ast::App { pos, box op, args }) => {
                let cop = self.convert_subexpr(op, &mut tempname);
                let cargs = args.into_iter()
                                .map(|arg| self.convert_subexpr(arg, &mut tempname))
                                .collect();

                let ki = self.peek_label();
                self.0.push(Cont {
                    param: tempname,
                    expr: CPS::App(App {
                        pos: pos,
                        op: Box::new(cop),
                        args: cargs,
                        cont: Box::new(cont.unwrap_or(ContRef::Local(ki)))
                    })
                });

                None
            }

            AST::Fn(fun @ ast::Fn { .. }) => Some(CPS::Fn(Fn::from(fun))),
            AST::Var(v) => Some(CPS::Var(v)),
            AST::Const(c) => Some(CPS::Const(c))
        }
    }

    /// Like `convert_step` but always returns a trivial term even if `expr` was serious.
    fn convert_subexpr(&mut self, expr: AST, tempname: &mut Option<String>) -> CPS {
        let pos = expr.pos();
        match self.convert_step(expr, tempname.clone(), None) {
            Some(t) => t,
            None => {
                *tempname = Some("__tmp".to_string());
                CPS::Var(Var {
                    pos: pos,
                    name: VarRef::Local(tempname.clone().unwrap())
                })
            }
        }
    }

    /// Like `convert_step` but force pushing of `Cont` instead of returning trivial term.
    fn convert_nontrivially(&mut self, expr: AST, cont: Option<ContRef>,
                            tempname: &mut Option<String>)
    {
        if let Some(t) = self.convert_step(expr, tempname.clone(), cont) {
            let ki = self.peek_label();
            self.0.push(Cont {
                param: tempname.clone(),
                expr: CPS::Continue(Continue {
                    cont: cont.unwrap_or(ContRef::Local(ki)),
                    args: Box::new(t)
                })
            });
        }
    }
}

impl Display for ContMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (label, cont) in self.0.iter().enumerate() {
            writeln!(f, "{}: {}", label, cont)?;
        }
        Ok(())
    }
}

/// Continuation.
#[derive(Debug)]
pub struct Cont {
    pub param: Option<Name>,
    pub expr: CPS
}

impl Display for Cont {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for param in self.param.iter() {
            write!(f, "{} ", param)?;
        }
        write!(f, "-> {}", self.expr)
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
