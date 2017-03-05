use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::fmt;
use std::fmt::Display;

use util::{SrcPos, Name, IndexSrc};
use ast;
use ast::{AST, Var, VarRef, Const, CtxMapping};

#[derive(Debug)]
pub struct FAST {
    procs: HashMap<Name, Fn>,
    expr: Expr
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
pub struct Fn {
    pub pos: SrcPos,
    pub freevars: Vec<Name>,
    pub clauses: Vec<Clause>
}

impl Display for Fn {
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

#[derive(Debug)]
pub struct Clause {
    pub pos: SrcPos,
    pub params: Name, // TODO: Vec<AST>
    pub cond: Expr,
    pub body: Vec<Stmt>
}

impl Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} | {} => ", self.params, self.cond)?;
        let mut it = self.body.iter();
        if let Some(stmt) = it.next() {
            write!(f, "{}", stmt)?;
        }
        for stmt in it {
            write!(f, "; {}", stmt)?;
        }
        Ok(())
    }
}

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

#[derive(Debug)]
pub struct Block {
    pub pos: SrcPos,
    pub stmts: Vec<Stmt>
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{")?;
        let mut it = self.stmts.iter();
        if let Some(arg) = it.next() {
            write!(f, "{}", arg)?;
        }
        for arg in it {
            write!(f, "; {}", arg)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct App {
    pub pos: SrcPos,
    pub op: Box<Expr>,
    pub args: Vec<Expr>
}

impl Display for App {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "({} ", self.op)?;
        let mut it = self.args.iter();
        if let Some(arg) = it.next() {
            write!(f, "{}", arg)?;
        }
        for arg in it {
            write!(f, " {}", arg)?;
        }
        write!(f, ")")
    }
}

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

#[derive(Debug)]
pub enum Stmt {
    Def { name: Name, val: Expr },
    Expr(Expr)
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Stmt::Def { ref name, ref val, ..} => write!(f, "{} = {}", name, val),
            &Stmt::Expr(ref expr) => expr.fmt(f)
        }
    }
}

// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Env {
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

pub struct Flatten {
    counter: IndexSrc
}

impl Flatten {
    fn new(counter: IndexSrc) -> Flatten {
        Flatten { counter: counter }
    }

    fn rename(&mut self, name: &Name) -> Name {
        name.as_unique(&mut self.counter)
    }

    fn block_bindings<'a, I>(&mut self, bindings: &mut HashMap<Name, Name>, stmts: I)
        where I: Iterator<Item=&'a ast::Stmt>
    {
        for stmt in stmts {
            match stmt {
                &ast::Stmt::Def { ref name, .. } => {
                    bindings.insert(name.clone(), self.rename(name));
                },
                &ast::Stmt::Expr(..) => ()
            }
        }
    }

    fn param_bindings<'a>(&mut self, bindings: &mut HashMap<Name, Name>, params: &Name) {
        bindings.insert(params.clone(), self.rename(params));
    }
}

impl CtxMapping for Flatten {
    type Ctx = Option<Rc<Env>>;
    type ASTRes = ((HashMap<Name, Fn>, Expr), HashSet<Name>);
    type StmtRes = ((HashMap<Name, Fn>, Stmt), HashSet<Name>);
    type ClauseRes = ((HashMap<Name, Fn>, Clause), HashSet<Name>);

    fn map_block(&mut self, block: ast::Block, env: Option<Rc<Env>>) -> Self::ASTRes {
        let mut bindings = HashMap::new();
        self.block_bindings(&mut bindings, block.stmts.iter());
        let env = Some(Rc::new(Env::new(env, bindings)));

        let (procexprs, freesets): (Vec<(HashMap<Name, Fn>, Stmt)>, Vec<HashSet<Name>>) =
            block.stmts.into_iter()
                       .map(|stmt| self.map_stmt(stmt, env.clone()))
                       .unzip();
        let (procs, stmts): (Vec<HashMap<Name, Fn>>, Vec<Stmt>) = procexprs.into_iter().unzip();
        ((procs.into_iter().flat_map(|procs| procs).collect(),
          Expr::Block(Block { pos: block.pos, stmts: stmts })),
         freesets.into_iter().flat_map(|frees| frees).collect())
    }

    fn map_fn(&mut self, ast::Fn { pos, clauses }: ast::Fn, env: Option<Rc<Env>>) -> Self::ASTRes {
        let name = Name::unique(String::from("f"), &mut self.counter);

        let (cprocexprs, freesets): (Vec<(HashMap<Name, Fn>, Clause)>, Vec<HashSet<Name>>) =
            clauses.into_iter().map(|clause| self.map_clause(clause, env.clone())).unzip();
        let (procmaps, clauses): (Vec<HashMap<Name, Fn>>, Vec<Clause>) =
            cprocexprs.into_iter().unzip();
        let freeset: HashSet<Name> = freesets.into_iter().flat_map(|frees| frees).collect();
        let mut freevec = Vec::new();
        for var in freeset.iter() {
            freevec.push(var.clone())
        }
        let mut procs: HashMap<Name, Fn> = procmaps.into_iter().flat_map(|procs| procs).collect();
        procs.insert(name.clone(), Fn { pos: pos, freevars: freevec.clone(), clauses: clauses});
        ((procs, Expr::Closure(Closure { pos: pos, fun: name, freevars: freevec })), freeset)
    }

    fn map_app(&mut self, ast::App { pos, op, args }: ast::App, env: Option<Rc<Env>>)
        -> Self::ASTRes
    {
        let ((mut procs, op), mut freevars) = op.accept_ctx(self, env.clone());
        let (fargs, argfrees): (Vec<(HashMap<Name, Fn>, Expr)>, Vec<HashSet<Name>>) =
            args.into_iter()
                .map(|arg| arg.accept_ctx(self, env.clone()))
                .unzip();
        let (aprocs, args): (Vec<HashMap<Name, Fn>>, Vec<Expr>) = fargs.into_iter().unzip();
        for aprocset in aprocs {
            procs.extend(aprocset);
        }
        for frees in argfrees {
            freevars.extend(frees);
        }
        ((procs, Expr::App(App { pos: pos, op: Box::new(op), args: args })), freevars)
    }

    fn map_var(&mut self, Var { pos, name }: Var, env: Option<Rc<Env>>) ->  Self::ASTRes {
        let vref = env.map(|env| env.resolve(&name.name())).unwrap_or(name);
        let mut freevars = HashSet::new();
        if let VarRef::Clover(ref name) = vref {
            freevars.insert(name.clone());
        }
        ((HashMap::new(), Expr::Var(Var { pos: pos, name: vref })), freevars)
    }

    fn map_const(&mut self, c: Const, _: Option<Rc<Env>>) ->  Self::ASTRes {
        ((HashMap::new(), Expr::Const(c)), HashSet::new())
    }

    fn map_stmt(&mut self, stmt: ast::Stmt, env: Option<Rc<Env>>) -> Self::StmtRes {
        match stmt {
            ast::Stmt::Def { name, val } => {
                let ((procs, expr), freevars) = val.accept_ctx(self, env.clone());
                ((procs, Stmt::Def {
                    name: env.and_then(|env| env.resolve_str(&name)).unwrap_or(name),
                    val: expr
                 }), freevars)
            }
            ast::Stmt::Expr(e) => {
                let ((procs, expr), freevars) = e.accept_ctx(self, env);
                ((procs, Stmt::Expr(expr)), freevars)
            }
        }
    }

    fn map_clause(&mut self, ast::Clause { pos, params, cond, body }: ast::Clause,
                  env: Option<Rc<Env>>) -> Self::ClauseRes
    {
        let mut param_bindings = HashMap::new();
        self.param_bindings(&mut param_bindings, &params);
        let param_env = Some(Rc::new(Env::new(env.clone(), param_bindings.clone())));

        let mut bindings = param_bindings;
        self.block_bindings(&mut bindings, body.iter());
        let env = Some(Rc::new(Env::new(env.clone(), bindings)));

        let ((mut procs, cond), mut freevars) = cond.accept_ctx(self, param_env.clone());
        let (bprocexprs, bfreesets): (Vec<(HashMap<Name, Fn>, Stmt)>, Vec<HashSet<Name>>) =
            body.into_iter()
                .map(|stmt| self.map_stmt(stmt, env.clone()))
                .unzip();
        let (bprocs, bstmts): (Vec<HashMap<Name, Fn>>, Vec<Stmt>) = bprocexprs.into_iter().unzip();
        for freeset in bfreesets {
            freevars.extend(freeset);
        }
        for procmap in bprocs {
            procs.extend(procmap);
        }
        ((procs, Clause {
            pos: pos,
            params: param_env.and_then(|env| env.resolve_str(&params)).unwrap(), // unwrap is OK
            cond: cond,
            body: bstmts
         }), freevars)
    }
}

// ------------------------------------------------------------------------------------------------

impl AST {
    pub fn flatten(self, counter: IndexSrc) -> FAST {
        let ((procs, expr), _) = self.accept_ctx(&mut Flatten::new(counter), None);
        FAST::new(procs, expr)
    }
}
