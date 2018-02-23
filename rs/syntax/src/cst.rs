use std::ptr::NonNull;
use std::convert::TryFrom;
use std::str::FromStr;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::fmt::{self, Display};
use combine::Parser;

use lexer::Lexer;
use parser;

// ================================================================================================

#[derive(Debug)]
pub struct Program<S> {
    pub cst: Expr,
    state: PhantomData<S>
}

impl<S> Program<S> {
    pub fn new(cst: Expr) -> Program<S> {
        Program { cst, state: PhantomData }
    }
}

#[derive(Debug)]
pub enum Parsed {}

#[derive(Debug)]
pub struct ParseError(pub String);

impl FromStr for Program<Parsed> {
    type Err = ParseError;

    fn from_str(source: &str) -> Result<Self, ParseError> {
        let id_factory = RefCell::new(IdFactory::new());

        match parser::program(&id_factory).parse(Lexer::new(source)) {
            Ok((cst, _)) => Ok(Program::new(cst)),
            Err(err) => Err(ParseError(format!("{}", err))) // HACK
        }
    }
}

// ================================================================================================

pub type DefRef = Rc<RefCell<Def>>;

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub uses: HashSet<NonNull<Use>>
}

impl Def {
    pub fn new<S: Into<String>>(name: S) -> DefRef {
        Rc::new(RefCell::new(Def {
            name: name.into(),
            uses: HashSet::new()
        }))
    }
}

#[derive(Debug, Clone)]
pub struct Use {
    pub def: DefRef
}

impl Use {
    pub fn new(def: DefRef) -> Use { Use { def } }
}

#[derive(Debug)]
pub enum Expr {
    Function(Pos, Vec<DefRef>, Box<Expr>),
    Block(Pos, Vec<Stmt>, Box<Expr>),
    Match(Pos, Vec<Case>, Box<Case>),
    Call(Pos, Box<Expr>, Vec<Expr>),
    Lex(Pos, Use),
    Dyn(Pos, String),
    Const(Pos, Const)
}

#[derive(Debug)]
pub enum Pattern {
    Call(Pos, Expr, Vec<Pattern>),
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
            Expr::Call(pos, callee, args) =>
                Pattern::Call(pos, *callee, args.into_iter()
                                                .map(TryFrom::try_from)
                                                .collect::<Result<Vec<_>, _>>()?),
            Expr::Lex(pos, usage) => Pattern::Lex(pos, usage.def),
            Expr::Dyn(pos, name) => Pattern::Dyn(pos, name),
            Expr::Const(pos, c) => Pattern::Const(pos, c),
            _ => return Err(IllegalPattern)
        })
    }
}

#[derive(Debug)]
pub enum Stmt {
    Def(Pattern, Expr),
    Expr(Expr)
}

#[derive(Debug)]
pub struct Case {
    pub patterns: Vec<Pattern>,
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

impl Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Const::*;

        match self {
            &Int(n) => n.fmt(f),
            &Float(n) => n.fmt(f),
            &Char(c) => write!(f, "'{}'", c),
            &Bool(true) => "True".fmt(f),
            &Bool(false) => "False".fmt(f),
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

    pub fn usage(&mut self, name: &str) -> Use {
        Use::new(self.defs.get(name).map(Clone::clone).unwrap_or_else(|| {
            let def = Def::new(name.to_string());
            self.insert(name, def.clone());
            def
        }))
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
    fn pos(&self) -> &Pos {
        self.patterns.get(0).map(Pattern::pos)
            .unwrap_or_else(|| self.guard.pos())
    }
}
