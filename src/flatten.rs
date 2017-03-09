use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::fmt;
use std::fmt::Display;
use std::ops;

use util::{Sourced, SrcPos, Name, IndexSrc};
use ast;
use ast::{AST, Var, VarRef, Const, CtxMapping};

// FIXME: Block binding frames should not cause params to be taken as clovers.

#[derive(Debug)]
pub struct FAST {
    pub procs: HashMap<Name, Fn>,
    pub expr: Expr
}

impl FAST {
    pub fn new(procs: HashMap<Name, Fn>, expr: Expr) -> FAST {
        FAST {
            procs: procs,
            expr: expr
        }
    }
}

impl Display for FAST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (name, p) in self.procs.iter() {
            write!(f, "{} {}\n\n", name, p)?;
        }
        write!(f, "{}", self.expr)
    }
}

#[derive(Debug)]
pub struct Fun<C> {
    pub pos: SrcPos,
    pub freevars: Vec<Name>,
    pub clauses: Vec<C>
}

pub type Fn = Fun<Clause>;

impl<C> Display for Fun<C> where C: Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "[")?;
        for v in self.freevars.iter() {
            write!(f, "{}, ", v)?;
        }
        try!(write!(f, "] {{"));
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

pub type Clause = ast::Clause<Expr>;

#[derive(Debug)]
pub enum Expr {
    Block(Block),
    Closure(Closure),
    App(App),

    Var(Var),
    Const(Const)
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Expr::Block(ref block) => block.fmt(f),
            &Expr::Closure(ref close) => close.fmt(f),
            &Expr::App(ref app) => app.fmt(f),
            &Expr::Var(ref v) => v.fmt(f),
            &Expr::Const(ref c) => c.fmt(f)
        }
    }
}

impl Sourced for Expr {
    fn pos(&self) -> SrcPos {
        match self {
            &Expr::Block(ref block) => block.pos(),
            &Expr::Closure(ref close) => close.pos(),
            &Expr::App(ref app) => app.pos(),
            &Expr::Var(ref v) => v.pos(),
            &Expr::Const(ref c) => c.pos(),
        }
    }
}

pub type Block = ast::Block<Stmt>;

pub type App = ast::App<Expr>;

#[derive(Debug)]
pub struct Closure {
    pub pos: SrcPos,
    pub fun: Name,
    pub freevars: Vec<Name>
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "(%fun {} ", self.fun)?;
        let mut it = self.freevars.iter();
        if let Some(arg) = it.next() {
            write!(f, "{}", arg)?;
        }
        for arg in it {
            write!(f, " {}", arg)?;
        }
        write!(f, ")")
    }
}

impl Sourced for Closure {
    fn pos(&self) -> SrcPos {
        self.pos
    }
}

pub type Stmt = ast::Stmt<Expr>;

// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
struct Env {
    bindings: HashMap<Name, Name>,
    parent: Option<Rc<Env>>
}

impl Env {
    fn new(parent: Option<Rc<Env>>, bindings: HashMap<Name, Name>) -> Env {
        Env {
            bindings: bindings,
            parent: parent
        }
    }

    fn resolve(&self, name: &Name) -> VarRef {
        self.bindings.get(name)
                     .map(|name| VarRef::Local(name.clone()))
                     .or_else(||
                         self.parent.clone()
                                    .and_then(|parent| parent.resolve_str(name))
                                    .map(|name| VarRef::Clover(name.clone())))
                     .unwrap_or(VarRef::Global(name.clone()))
    }

    fn resolve_str(&self, name: &Name) -> Option<Name> {
        self.bindings.get(name)
                     .map(Clone::clone)
                     .or_else(||
                         self.parent.clone().and_then(|parent| parent.resolve_str(name)))
    }
}

// ------------------------------------------------------------------------------------------------

struct Flatten {
    counter: IndexSrc,
    procs: HashMap<Name, Fn>
}

impl Flatten {
    fn new(counter: IndexSrc) -> Flatten {
        Flatten {
            counter: counter,
            procs: HashMap::new()
        }
    }

    fn rename(&mut self, name: &Name) -> Name {
        name.as_unique(&mut self.counter)
    }

    fn add_proc(&mut self, fun: Fn) -> Name {
        let name = Name::unique(String::from("f"), &mut self.counter);
        self.procs.insert(name.clone(), fun);
        name
    }

    fn block_bindings<'a, I>(&mut self, stmts: I) -> HashMap<Name, Name>
        where I: Iterator<Item=&'a ast::Stmt<AST>>
    {
        let mut bindings = HashMap::new();
        for stmt in stmts {
            match stmt {
                &ast::Stmt::Def { ref name, .. } => {
                    bindings.insert(name.clone(), self.rename(name));
                },
                &ast::Stmt::Expr(..) => ()
            }
        }
        bindings
    }

    fn param_bindings<'a>(&mut self, params: &Name) -> HashMap<Name, Name> {
        let mut bindings = HashMap::new();
        bindings.insert(params.clone(), self.rename(params));
        bindings
    }

    fn flat_map_to<A, B, F, I>(&mut self, f: F, asts: I, env: Option<Rc<Env>>,
                            adest: &mut Vec<B>, vdest: &mut HashSet<Name>)
        where F: ops::Fn(&mut Self, A, Option<Rc<Env>>) -> (B, HashSet<Name>),
              I: IntoIterator<Item=A>
    {
        for a in asts {
            let (a, fs) = f(self, a, env.clone());
            adest.push(a);
            vdest.extend(fs);
        }
    }

    fn flat_map<A, B, F, I>(&mut self, f: F, asts: I, env: Option<Rc<Env>>)
        -> (Vec<B>, HashSet<Name>)
        where F: ops::Fn(&mut Self, A, Option<Rc<Env>>) -> (B, HashSet<Name>),
              I: IntoIterator<Item=A>
    {
        let mut fas = Vec::new();
        let mut freevars = HashSet::new();
        self.flat_map_to(f, asts, env, &mut fas, &mut freevars);
        (fas, freevars)
    }

    fn remove_bindings(freevars: &mut HashSet<Name>, bindings: &HashMap<Name, Name>) {
        for name in bindings.values() {
            freevars.remove(name);
        }
    }
}

impl CtxMapping for Flatten {
    type Ctx = Option<Rc<Env>>;
    type ASTRes = (Expr, HashSet<Name>);
    type StmtRes = (Stmt, HashSet<Name>);
    type ClauseRes = (Clause, HashSet<Name>);

    fn map_block(&mut self, ast::Block { pos, stmts }: ast::Block<ast::Stmt<AST>>, env: Option<Rc<Env>>)
        -> Self::ASTRes
    {
        let bindings = self.block_bindings(stmts.iter());
        let env = Some(Rc::new(Env::new(env, bindings.clone())));

        let (fstmts, mut freevars) = self.flat_map(Flatten::map_stmt, stmts, env.clone());

        Flatten::remove_bindings(&mut freevars, &bindings);

        (Expr::Block(Block { pos: pos, stmts: fstmts }), freevars)
    }

    fn map_fn(&mut self, ast::Fn { pos, clauses }: ast::Fn, env: Option<Rc<Env>>) -> Self::ASTRes {
        let (fclauses, freevars) = self.flat_map(Flatten::map_clause, clauses, env.clone());

        let freevec: Vec<Name> = freevars.iter().cloned().collect();
        let name = self.add_proc(Fn {
            pos: pos,
            freevars:
            freevec.clone(), clauses: fclauses
        });

        (Expr::Closure(Closure {
            pos: pos,
            fun: name,
            freevars: freevec
        }), freevars)
    }

    fn map_app(&mut self, ast::App { pos, op, args }: ast::App<AST>, env: Option<Rc<Env>>)
        -> Self::ASTRes
    {
        let (op, mut freevars) = op.accept_ctx(self, env.clone());

        let mut fargs = Vec::new();
        self.flat_map_to(|f, arg, env| arg.accept_ctx(f, env),
                         args, env.clone(), &mut fargs, &mut freevars);

        (Expr::App(App { pos: pos, op: Box::new(op), args: fargs }), freevars)
    }

    fn map_var(&mut self, Var { pos, name }: Var, env: Option<Rc<Env>>) ->  Self::ASTRes {
        let vref = env.map(|env| env.resolve(&name.name())).unwrap_or(name);
        let mut freevars = HashSet::new();
        if let VarRef::Clover(ref name) = vref {
            freevars.insert(name.clone());
        }
        (Expr::Var(Var { pos: pos, name: vref }), freevars)
    }

    fn map_const(&mut self, c: Const, _: Option<Rc<Env>>) ->  Self::ASTRes {
        (Expr::Const(c), HashSet::new())
    }

    fn map_stmt(&mut self, stmt: ast::Stmt<AST>, env: Option<Rc<Env>>) -> Self::StmtRes {
        match stmt {
            ast::Stmt::Def { name, val } => {
                let (expr, freevars) = val.accept_ctx(self, env.clone());
                (ast::Stmt::Def {
                    name: env.and_then(|env| env.resolve_str(&name)).unwrap_or(name),
                    val: expr
                 }, freevars)
            }
            ast::Stmt::Expr(e) => {
                let (expr, freevars) = e.accept_ctx(self, env);
                (ast::Stmt::Expr(expr), freevars)
            }
        }
    }

    fn map_clause(&mut self, ast::Clause { pos, params, cond, body }: ast::Clause<AST>,
                  env: Option<Rc<Env>>) -> Self::ClauseRes
    {
        let param_bindings = self.param_bindings(&params);
        let param_env = Some(Rc::new(Env::new(env.clone(), param_bindings.clone())));

        let (cond, mut freevars) = cond.accept_ctx(self, param_env.clone());

        Flatten::remove_bindings(&mut freevars, &param_bindings);

        let mut bindings = param_bindings;
        bindings.extend(self.block_bindings(body.iter()));
        let env = Some(Rc::new(Env::new(env.clone(), bindings.clone())));

        let mut fstmts = Vec::new();
        self.flat_map_to(Flatten::map_stmt, body, env.clone(), &mut fstmts, &mut freevars);

        Flatten::remove_bindings(&mut freevars, &bindings);

        (Clause {
            pos: pos,
            params: param_env.and_then(|env| env.resolve_str(&params)).unwrap(), // unwrap is OK
            cond: cond,
            body: fstmts
         }, freevars)
    }
}

// ------------------------------------------------------------------------------------------------

impl AST {
    pub fn flatten(self, counter: IndexSrc) -> FAST {
        let mut flattener = Flatten::new(counter);
        let (expr, _) = self.accept_ctx(&mut flattener, None);
        FAST::new(flattener.procs, expr)
    }
}
