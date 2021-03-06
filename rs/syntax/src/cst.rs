use std::ptr::NonNull;
use std::convert::TryFrom;
use std::str::FromStr;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display, Formatter};
use std::iter;
use pretty::{self, Doc, DocAllocator, DocBuilder};

use lexer::Lexer;
use parser::{self, ParseError};

// ================================================================================================

impl FromStr for Expr {
    type Err = ParseError;

    fn from_str(source: &str) -> Result<Expr, ParseError> {
        parser::program(&mut Lexer::new(source), &RefCell::new(IdFactory::new()))
    }
}

// ================================================================================================

pub type DefRef = Rc<RefCell<Def>>;

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub uses: HashSet<NonNull<Expr>>
}

impl Def {
    pub fn new<S: Into<String>>(name: S) -> DefRef {
        Rc::new(RefCell::new(Def {
            name: name.into(),
            uses: HashSet::new()
        }))
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum PrimOp {
    Tuple,
    TupleLen,
    TupleGet,
    TupleSlice,

    SliceLen,
    SliceGetP,
    SliceSubP,

    IAdd,

    SymbolFresh,

    Promise,
    Redirect,

    Eq,
    Type,

    DenvEmpty,
    Denv,
    DenvGet,

    Prompt,

    AssertP
}

impl Display for PrimOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut debug_string = format!("__{:?}", self);
        unsafe {
            let bytes = debug_string.as_bytes_mut();
            bytes[2] = char::from(bytes[2]).to_ascii_lowercase() as _;
        }
        debug_string.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Function(Pos, Vec<DefRef>, Box<Expr>),
    Block(Pos, Vec<Stmt>, Box<Expr>),
    Match(Pos, Box<Expr>, Vec<Case>, Box<Expr>),
    Call(Pos, Box<Expr>, Vec<Expr>),
    PrimCall(Pos, PrimOp, Vec<Expr>),
    Lex(Pos, DefRef),
    Dyn(Pos, String),
    Const(Pos, Const)
}

impl Expr {
    pub fn is_trivial(&self) -> bool {
        match *self {
            Expr::Lex(..) | Expr::Dyn(..) | Expr::Const(..) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Call(Pos, Expr, Vec<Pattern>),
    PrimCall(Pos, PrimOp, Vec<Pattern>),
    Lex(Pos, DefRef),
    Dyn(Pos, String),
    Const(Pos, Const)
}

#[derive(Debug)]
pub struct IllegalPattern;

impl TryFrom<Expr> for Pattern {
    type Error = IllegalPattern;

    fn try_from(expr: Expr) -> Result<Pattern, IllegalPattern> {
        Ok(match expr {
            Expr::Call(pos, callee, args) => // FIXME: Deal with `apply`:s from CstFactory
                Pattern::Call(pos, *callee, args.into_iter()
                                                .map(TryFrom::try_from)
                                                .collect::<Result<Vec<_>, _>>()?),
            Expr::PrimCall(pos, op, args) =>
                Pattern::PrimCall(pos, op, args.into_iter()
                                               .map(TryFrom::try_from)
                                               .collect::<Result<Vec<_>, _>>()?),
            Expr::Lex(pos, def) => Pattern::Lex(pos, def),
            Expr::Dyn(pos, name) => Pattern::Dyn(pos, name),
            Expr::Const(pos, c) => Pattern::Const(pos, c),
            _ => return Err(IllegalPattern)
        })
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Def(Pattern, Expr),
    Expr(Expr)
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Stmt { Stmt::Expr(expr) }
}

#[derive(Debug, Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub guard: Expr,
    pub body: Expr
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Symbol(String)
}

impl From<isize> for Const {
    fn from(n: isize) -> Const { Const::Int(n) }
}

impl Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Const::*;

        match self {
            &Int(n) => n.fmt(f),
            &Float(n) => n.fmt(f),
            &Char(c) => write!(f, "'{}'", c),
            &Bool(true) => "__true".fmt(f),
            &Bool(false) => "__false".fmt(f),
            &String(ref s) => write!(f, "\"{}\"", s),
            &Symbol(ref s) => write!(f, ":{}", s)
        }
    }
}

// ================================================================================================

#[derive(Debug)]
pub struct IdFactory {
    defs: HashMap<String, DefRef>
}

impl IdFactory {
    pub fn new() -> IdFactory {
        IdFactory { defs: HashMap::new() }
    }

    pub fn usage(&mut self, name: &str) -> DefRef {
        self.defs.get(name).map(Clone::clone).unwrap_or_else(|| {
            let def = Def::new(name.to_string());
            self.insert(name, def.clone());
            def
        })
    }

    pub fn insert<S: Into<String>>(&mut self, name: S, def: DefRef) {
        self.defs.insert(name.into(), def);
    }
}

// ================================================================================================

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pub file: Rc<String>, // OPTIMIZE: Handle this some other way so that Pos: Copy
    pub index: usize,
    pub line: usize,
    pub col: usize
}

impl Default for Pos {
    fn default() -> Pos {
        Pos {
            file: Rc::new(String::new()),
            index: 0,
            line: 1,
            col: 1
        }
    }
}

pub trait Positioned {
    fn pos(&self) -> &Pos;
}

impl Positioned for Expr {
    fn pos(&self) -> &Pos {
        use self::Expr::*;

        match *self {
            Function(ref pos, ..) => pos,
            Match(ref pos, ..) => pos,
            Block(ref pos, ..) => pos,
            Call(ref pos, ..) => pos,
            PrimCall(ref pos, ..) => pos,
            Lex(ref pos, ..) => pos,
            Dyn(ref pos, ..) => pos,
            Const(ref pos, ..) => pos
        }
    }
}

impl Positioned for Pattern {
    fn pos(&self) -> &Pos {
        use self::Pattern::*;

        match *self {
            Call(ref pos, ..) => pos,
            PrimCall(ref pos, ..) => pos,
            Lex(ref pos, ..) => pos,
            Dyn(ref pos, ..) => pos,
            Const(ref pos, ..) => pos
        }
    }
}

impl Positioned for Stmt {
    fn pos(&self) -> &Pos {
        match *self {
            Stmt::Def(ref pattern, ..) => pattern.pos(),
            Stmt::Expr(ref expr, ..) => expr.pos()
        }
    }
}

impl Positioned for Case {
    fn pos(&self) -> &Pos { self.pattern.pos() }
}

// ================================================================================================

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let allocator = pretty::Arena::new();
        <DocBuilder<_> as Into<Doc<_>>>::into(self.pretty(&allocator)).render_fmt(80, f)
    }
}

impl Display for Def {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}@{:p}", self.name, self)
    }
}

impl Expr {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        use self::Expr::*;

        fn pretty_block<'a, A>(stmts: &'a [Stmt], expr: &'a Expr, allocator: &'a A)
            -> DocBuilder<'a, A> where A: DocAllocator<'a>
        {
            allocator.intersperse(stmts.iter()
                                       .map(|stmt| stmt.pretty(allocator))
                                       .chain(iter::once(expr.pretty(allocator))),
                                  allocator.text(";").append(allocator.newline()))
        }

        match self {
            &Function(_, ref params, ref body) =>
                allocator.text("@fn (")
                         .append(allocator.intersperse(
                                    params.iter().map(|param| allocator.as_string(param.borrow())),
                                    " "))
                         .append(") {")
                         .append(allocator.newline()
                                          .append(body.pretty(allocator))
                                          .nest(2))
                         .append(allocator.newline())
                         .append("}"),
            &Block(_, ref stmts, ref expr) =>
                allocator.text("{")
                         .append(allocator.newline()
                                          .append(pretty_block(stmts, expr, allocator))
                                          .nest(2))
                         .append(allocator.newline())
                         .append("}"),
            &Match(_, ref matchee, ref cases, ref default) =>
                allocator.text("@match ").append(matchee.pretty(allocator)).append(" {")
                         .append(allocator.newline()
                                          .append(allocator.intersperse(
                                                      cases.iter()
                                                           .map(|case| case.pretty(allocator))
                                                           .chain(iter::once(
                                                                      default.pretty(allocator))),
                                                      allocator.text(";")
                                                               .append(allocator.newline())))
                                          .nest(2))
                         .append(allocator.newline())
                         .append("}"),
            &Call(_, ref callee, ref args) =>
                allocator.text("(")
                         .append(allocator.intersperse(
                                     iter::once(callee.pretty(allocator))
                                          .chain(args.iter().map(|arg| arg.pretty(allocator))),
                                     " "))
                         .append(")"),
            &PrimCall(_, op, ref args) =>
                allocator.text("(")
                         .append(allocator.intersperse(
                                     iter::once(allocator.as_string(op))
                                          .chain(args.iter().map(|arg| arg.pretty(allocator))),
                                     " "))
                         .append(")"),
            &Lex(_, ref def) => allocator.as_string(def.borrow()),
            &Dyn(_, ref name) => allocator.text("$").append(name.as_ref()),
            &Const(_, ref c) => allocator.as_string(c)
        }
    }
}

impl Pattern {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        use self::Pattern::*;

        match self {
            &Call(_, ref callee, ref args) =>
                allocator.text("(")
                         .append(allocator.intersperse(
                                     iter::once(callee.pretty(allocator))
                                          .chain(args.iter().map(|arg| arg.pretty(allocator))),
                                     " "))
                         .append(")"),
            &PrimCall(_, op, ref args) =>
                allocator.text("(")
                         .append(allocator.intersperse(
                                     iter::once(allocator.as_string(op))
                                          .chain(args.iter().map(|arg| arg.pretty(allocator))),
                                     " "))
                         .append(")"),
            &Lex(_, ref def) => allocator.as_string(def.borrow()),
            &Dyn(_, ref name) => allocator.text("$").append(allocator.text(name.as_ref())),
            &Const(_, ref c) => allocator.as_string(c)
        }
    }
}

impl Stmt {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        match self {
            &Stmt::Def(ref pat, ref val) =>
                pat.pretty(allocator).append(" = ").append(val.pretty(allocator)),
            &Stmt::Expr(ref expr) => expr.pretty(allocator)
        }
    }
}

impl Case {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        let &Case { ref pattern, ref guard, ref body } = self;

        pattern.pretty(allocator)
               .append(" => {")
               .append(allocator.newline()
                                .append(allocator.intersperse(
                                            iter::once(allocator.text("@guard ")
                                                                .append(guard.pretty(allocator)))
                                                 .chain(iter::once(body.pretty(allocator))),
                                            allocator.text(";").append(allocator.newline())))
                                .nest(2))
               .append(allocator.newline())
               .append("}")
    }
}

// ================================================================================================

#[derive(Debug)]
pub struct CstFactory {
    pos: Pos
}

impl CstFactory {
    pub fn new(pos: Pos) -> CstFactory {
        CstFactory { pos }
    }

    pub fn lex_def(&self, def: &DefRef) -> Pattern {
        Pattern::Lex(self.pos.clone(), def.clone())
    }

    pub fn def(&self, pattern: Pattern, expr: Expr) -> Stmt {
        Stmt::Def(pattern, expr)
    }

    pub fn block(&self, stmts: Vec<Stmt>, expr: Expr) -> Expr {
        Expr::Block(self.pos.clone(), stmts, Box::new(expr))
    }

    pub fn call(&self, callee: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call(self.pos.clone(), Box::new(callee), args)
    }

    pub fn primcall(&self, op: PrimOp, args: Vec<Expr>) -> Expr {
        Expr::PrimCall(self.pos.clone(), op, args)
    }

    pub fn lex_use(&self, def: &DefRef) -> Expr {
        Expr::Lex(self.pos.clone(), def.clone())
    }

    pub fn constant<V: Into<Const>>(&self, value: V) -> Expr {
        Expr::Const(self.pos.clone(), value.into())
    }
}
