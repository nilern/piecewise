use util::{Sourced, SrcPos};

use std::fmt;
use std::fmt::Display;

/// A `NodeMapping` is essentially a piecewise function that consumes a node
/// and produces a new one (or an error).
pub trait NodeMapping {
    /// The type to put in Result::Err(_).
    type Err;

    fn map_block(&mut self, node: Block) -> Result<AST, Self::Err> { Ok(AST::Block(node)) }
    fn map_fn(&mut self, node: Fn) -> Result<AST, Self::Err> { Ok(AST::Fn(node)) }
    fn map_app(&mut self, node: App) -> Result<AST, Self::Err> { Ok(AST::App(node)) }
    fn map_var(&mut self, node: Var) -> Result<AST, Self::Err> { Ok(AST::Var(node)) }
    fn map_const(&mut self, node: Const) -> Result<AST, Self::Err> { Ok(AST::Const(node)) }

    fn map_stmt(&mut self, node: Stmt) -> Result<Stmt, Self::Err> { Ok(node) }
    fn map_clause(&mut self, node: Clause) -> Result<Clause, Self::Err> { Ok(node) }
}

/// A `CtxMapping` is essentially a piecewise function that consumes a node and a context value
/// and produces a new one (or an error).
pub trait CtxMapping {
    /// The type of the context (usually an inherited attribute).
    type Ctx;
    /// The type to put in Result::Err(_).
    type Err;

    fn map_block(&mut self, node: Block, _: Self::Ctx) -> Result<AST, Self::Err> {
        Ok(AST::Block(node))
    }
    fn map_fn(&mut self, node: Fn, _: Self::Ctx) -> Result<AST, Self::Err> { Ok(AST::Fn(node)) }
    fn map_app(&mut self, node: App, _: Self::Ctx) -> Result<AST, Self::Err> {
        Ok(AST::App(node))
    }
    fn map_var(&mut self, node: Var, _: Self::Ctx) -> Result<AST, Self::Err> {
        Ok(AST::Var(node))
    }
    fn map_const(&mut self, node: Const, _: Self::Ctx) -> Result<AST, Self::Err> {
        Ok(AST::Const(node))
    }

    fn map_stmt(&mut self, node: Stmt, _: Self::Ctx) -> Result<Stmt, Self::Err> { Ok(node) }
    fn map_clause(&mut self, node: Clause, _: Self::Ctx) -> Result<Clause, Self::Err> { Ok(node) }
}

/// A `FunctorNode` knows how to apply a NodeMapping to each of its children.
pub trait FunctorNode: Sized {

    /// Apply the `NodeMapping` `f` to the children of this node.
    fn map<F>(self, f: &mut F) -> Result<Self, F::Err> where F: NodeMapping;
}

/// A type for variable names.
pub type Name = String;

/// Abstract Syntax Tree
#[derive(Debug)]
pub enum AST {
    Block(Block),
    Fn(Fn),
    App(App),

    Var(Var),
    Const(Const)
}

impl AST {
    /// Create a new AST that is equivalent to an `@if` expression.
    pub fn new_if(pos: SrcPos, cond: AST, then: AST, els: AST) -> AST {
        use self::AST::*;

        App(self::App {
            pos: pos,
            op: Box::new(Fn(self::Fn {
                pos: pos,
                clauses: vec![
                    Clause {
                        pos: then.pos(),
                        params: String::from("_"), // HACK
                        cond: Var(self::Var {
                            pos: pos,
                            name: VarRef::Global(String::from("_")) // HACK
                        }),
                        body: vec![Stmt::Expr(then)]
                    },
                    Clause {
                        pos: els.pos(),
                        params: String::from("_"), // HACK
                        cond: Const(self::Const { pos: pos, val: ConstVal::Bool(true) }),
                        body: vec![Stmt::Expr(els)]
                    }
                ]
            })),
            args: vec![cond]
        })
    }

    /// Apply `f` to this node and produce a new one.
    pub fn accept<F>(self, f: &mut F) -> Result<AST, F::Err> where F: NodeMapping {
        use self::AST::*;

        match self {
            Block(block) => f.map_block(block),
            Fn(fun) => f.map_fn(fun),
            App(app) => f.map_app(app),
            Var(v) => f.map_var(v),
            Const(c) => f.map_const(c)
        }
    }

    /// Apply `f` to this node and produce a new one.
    pub fn accept_ctx<F>(self, f: &mut F, ctx: F::Ctx) -> Result<AST, F::Err> where F: CtxMapping {
        use self::AST::*;

        match self {
            Block(block) => f.map_block(block, ctx),
            Fn(fun) => f.map_fn(fun, ctx),
            App(app) => f.map_app(app, ctx),
            Var(v) => f.map_var(v, ctx),
            Const(c) => f.map_const(c, ctx)
        }
    }

    /// Traverse the tree in pre-order with `f`.
    pub fn prewalk<F>(self, f: F) -> Result<AST, F::Err> where F: NodeMapping {
        self.accept(&mut PreWalker(f))
    }
}

impl FunctorNode for AST {
    fn map<F>(self, f: &mut F) -> Result<AST, F::Err> where F: NodeMapping {
        use self::AST::*;

        match self {
            Block(block) => block.map(f).map(|block| Block(block)),
            Fn(fun) => fun.map(f).map(|fun| Fn(fun)),
            App(app) => app.map(f).map(|app| App(app)),
            v @ Var(_) => Ok(v),
            c @ Const(_) => Ok(c)
        }
    }
}

impl Sourced for AST {
    fn pos(&self) -> SrcPos {
        match self {
            &AST::Block(ref block) => block.pos(),
            &AST::Fn(ref f) => f.pos(),
            &AST::App(ref app) => app.pos(),
            &AST::Var(ref v) => v.pos(),
            &AST::Const(ref c) => c.pos()
        }
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &AST::Block(ref block) => block.fmt(f),
            &AST::Fn(ref fun) => fun.fmt(f),
            &AST::App(ref app) => app.fmt(f),
            &AST::Var(ref v) => v.fmt(f),
            &AST::Const(ref c) => c.fmt(f)
        }
    }
}

/// A block.
#[derive(Debug)]
pub struct Block {
    pub pos: SrcPos,
    //decls: Vec<Name>
    pub stmts: Vec<Stmt>
}

impl Sourced for Block {
    fn pos(&self) -> SrcPos {
        self.pos
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "{{"));
        let mut it = self.stmts.iter();
        if let Some(arg) = it.next() {
            try!(write!(f, "{}", arg));
        }
        for arg in it {
            try!(write!(f, "; {}", arg));
        }
        write!(f, "}}")
    }
}

impl FunctorNode for Block {
    fn map<F>(self, f: &mut F) -> Result<Block, F::Err> where F: NodeMapping {
        Ok(Block {
            pos: self.pos,
            stmts: self.stmts.into_iter()
                             .map(|stmt| f.map_stmt(stmt))
                             .collect::<Result<Vec<Stmt>, F::Err>>()?
        })
    }
}

/// A function expression.
#[derive(Debug)]
pub struct Fn {
    pub pos: SrcPos,
    pub clauses: Vec<Clause>
}

impl Sourced for Fn {
    fn pos(&self) -> SrcPos { self.pos }
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

impl FunctorNode for Fn {
    fn map<F>(self, f: &mut F) -> Result<Fn, F::Err> where F: NodeMapping {
        Ok(Fn {
            pos: self.pos,
            clauses: self.clauses.into_iter()
                                 .map(|clause| f.map_clause(clause))
                                 .collect::<Result<Vec<Clause>, F::Err>>()?
        })
    }
}

/// A function application.
#[derive(Debug)]
pub struct App {
    pub pos: SrcPos,
    pub op: Box<AST>,
    pub args: Vec<AST>
}

impl Sourced for App {
    fn pos(&self) -> SrcPos { self.pos }
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
        write!(f, ")")
    }
}

impl FunctorNode for App {
    fn map<F>(self, f: &mut F) -> Result<App, F::Err> where F: NodeMapping {
        Ok(App {
            pos: self.pos,
            op: Box::new(self.op.accept(f)?),
            args: self.args.into_iter()
                           .map(|arg| arg.accept(f))
                           .collect::<Result<Vec<AST>, F::Err>>()?
        })
    }
}

/// A variable reference with `SrcPos` information,
#[derive(Debug)]
pub struct Var {
    pub pos: SrcPos,
    pub name: VarRef
}

impl Var {
    pub fn name(&self) -> &str {
        self.name.name()
    }
}

impl Sourced for Var {
    fn pos(&self) -> SrcPos { self.pos }
}

impl Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> { self.name.fmt(f) }
}

/// A variable reference.
#[derive(Debug)]
pub enum VarRef {
    Local(Name),
    Clover(Name),
    Global(Name)
}

impl VarRef {
    pub fn name(&self) -> &str {
        match self {
            &VarRef::Local(ref name) => name,
            &VarRef::Clover(ref name) => name,
            &VarRef::Global(ref name) => name
        }
    }
}

impl Display for VarRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &VarRef::Local(ref name) => name.fmt(f),
            &VarRef::Clover(ref name) => write!(f, "€{}", name),
            &VarRef::Global(ref name) => write!(f, "${}", name)
        }
    }
}

/// Statement (for `Block`s).
#[derive(Debug)]
pub enum Stmt {
    Def { name: String, val: AST },
    Expr(AST)
}

impl Stmt {
    pub fn into_expr(self) -> AST {
        match self {
            Stmt::Def { val, .. } => val,
            Stmt::Expr(expr) => expr
        }
    }
}

impl Sourced for Stmt {
    fn pos(&self) -> SrcPos {
        match self {
            &Stmt::Def { ref val, .. } => val.pos(),
            &Stmt::Expr(ref expr) => expr.pos()
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Stmt::Def { ref name, ref val, ..} => write!(f, "{} = {}", name, val),
            &Stmt::Expr(ref expr) => expr.fmt(f)
        }
    }
}

impl FunctorNode for Stmt {
    fn map<F>(self, f: &mut F) -> Result<Stmt, F::Err> where F: NodeMapping {
        match self {
            Stmt::Def { name, val } => Ok(Stmt::Def { name: name, val: val.accept(f)? }),
            Stmt::Expr(e) => Ok(Stmt::Expr(e.accept(f)?))
        }
    }
}

/// Function clause.
#[derive(Debug)]
pub struct Clause {
    pub pos: SrcPos,
    pub params: Name, // TODO: Vec<AST>
    pub cond: AST,
    pub body: Vec<Stmt>
}

impl Clause {
    fn push(&mut self, stmt: Stmt) {
        self.body.push(stmt);
    }
}

impl Sourced for Clause {
    fn pos(&self) -> SrcPos { self.pos }
}

impl Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "{} | {} => ", self.params, self.cond));
        let mut it = self.body.iter();
        if let Some(stmt) = it.next() {
            try!(write!(f, "{}", stmt));
        }
        for stmt in it {
            try!(write!(f, "; {}", stmt));
        }
        Ok(())
    }
}

impl FunctorNode for Clause {
    fn map<F>(self, f: &mut F) -> Result<Clause, F::Err> where F: NodeMapping {
        Ok(Clause {
            pos: self.pos,
            params: self.params,
            cond: self.cond.accept(f)?,
            body: self.body.into_iter()
                           .map(|stmt| f.map_stmt(stmt))
                           .collect::<Result<Vec<Stmt>, F::Err>>()?
        })
    }
}

/// Source constants.
#[derive(Debug)]
pub struct Const {
    pub pos: SrcPos,
    pub val: ConstVal
}

impl Sourced for Const {
    fn pos(&self) -> SrcPos { self.pos }
}

impl Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.val.fmt(f)
    }
}

/// Actual constant values.
#[derive(Debug)]
pub enum ConstVal {
    Int(isize),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool)
}

impl Display for ConstVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::ConstVal::*;

        match self {
            &Int(i) => i.fmt(f),
            &Float(n) => n.fmt(f),
            &Char(c) => fmt::Debug::fmt(&c, f),
            &String(ref cs) => write!(f, "\"{}\"", cs),
            &Bool(true) => write!(f, "True"),
            &Bool(false) => write!(f, "False")
        }
    }
}

/// A block item. This only exists for `parse_block`.
#[derive(Debug)]
pub enum BlockItem {
    Stmt(Stmt),
    Clause(Clause)
}

impl BlockItem {
    fn stmt(self) -> Option<Stmt> {
        match self {
            BlockItem::Stmt(stmt) => Some(stmt),
            BlockItem::Clause(_) => None
        }
    }
}

// TODO: Option -> Result for better error reporting
/// Try to convert a sequence of `BlockItem`s into either an `AST::Block` or an `AST::Fn`.
pub fn parse_block(pos: SrcPos, items: Vec<BlockItem>) -> Option<AST> {
    fn parse_stmt_block(pos: SrcPos, items: Vec<BlockItem>) -> Option<AST> {
        let mut stmts = Vec::new();

        for item in items.into_iter() {
            match item {
                BlockItem::Clause(_) => return None,
                BlockItem::Stmt(stmt) => stmts.push(stmt)
            }
        }

        Some(AST::Block(Block {
            pos: pos,
            stmts: stmts
        }))
    }

    fn parse_fn_block(pos: SrcPos, items: Vec<BlockItem>) -> Option<AST> {
        let mut it = items.into_iter().peekable();
        let mut clauses = Vec::new();

        loop {
            match it.next() {
                Some(BlockItem::Clause(mut clause)) => {
                    while let Some(&BlockItem::Stmt(_)) = it.peek() {
                        clause.push(it.next().unwrap().stmt().unwrap());
                    }
                    clauses.push(clause);
                },
                Some(BlockItem::Stmt(_)) => return None,
                None => return Some(AST::Fn(Fn { pos: pos, clauses: clauses}))
            }
        }
    }

    match items.first() {
        Some(&BlockItem::Clause(_)) => parse_fn_block(pos, items),
        Some(&BlockItem::Stmt(_)) => parse_stmt_block(pos, items),
        None => Some(AST::Block(Block { pos: pos, stmts: vec![] }))
    }
}

/// A `NodeMapping` that wraps another mapping and uses it to perform a pre-order traversal.
struct PreWalker<F>(F) where F: NodeMapping;

impl<F> NodeMapping for PreWalker<F> where F: NodeMapping {
    type Err = F::Err;

    fn map_block(&mut self, node: Block) -> Result<AST, Self::Err> {
        self.0.map_block(node).and_then(|node| node.map(self))
    }

    fn map_fn(&mut self, node: Fn) -> Result<AST, Self::Err> {
        self.0.map_fn(node).and_then(|node| node.map(self))
    }

    fn map_app(&mut self, node: App) -> Result<AST, Self::Err> {
        self.0.map_app(node).and_then(|node| node.map(self))
    }

    fn map_var(&mut self, node: Var) -> Result<AST, Self::Err> {
        self.0.map_var(node).and_then(|node| node.map(self))
    }

    fn map_const(&mut self, node: Const) -> Result<AST, Self::Err> {
        self.0.map_const(node).and_then(|node| node.map(self))
    }

    fn map_stmt(&mut self, node: Stmt) -> Result<Stmt, Self::Err> {
        self.0.map_stmt(node).and_then(|node| node.map(self))
    }

    fn map_clause(&mut self, node: Clause) -> Result<Clause, Self::Err> {
        self.0.map_clause(node).and_then(|node| node.map(self))
    }
}
