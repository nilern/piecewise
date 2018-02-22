use std::convert::TryFrom;
use std::str::FromStr;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::fmt::{self, Display};
use combine::Parser;

use lexer::Lexer;
use parser;

// ================================================================================================

#[derive(Debug)]
pub struct Program<S> {
    pub cst: Expr,
    pub ids: IdTable,
    state: PhantomData<S>
}

impl<S> Program<S> {
    pub fn new(cst: Expr, ids: IdTable) -> Program<S> {
        Program { cst, ids, state: PhantomData }
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
            Ok((cst, _)) => Ok(Program::new(cst, id_factory.into_inner().build())),
            Err(err) => Err(ParseError(format!("{}", err))) // HACK
        }
    }
}

// ================================================================================================

#[derive(Debug)]
pub enum Expr {
    Function(Pos, Vec<Id>, Box<Expr>),
    Block(Pos, Vec<Stmt>, Box<Expr>),
    Match(Pos, Vec<Case>, Box<Case>),
    Call(Pos, Box<Expr>, Vec<Expr>),
    Lex(Pos, Id),
    Dyn(Pos, String),
    Const(Pos, Const)
}

#[derive(Debug)]
pub enum Pattern {
    Call(Pos, Expr, Vec<Pattern>),
    Lex(Pos, Id),
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
            Expr::Lex(pos, id) => Pattern::Lex(pos, id),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

#[derive(Debug)]
pub struct IdTable {
    names: HashMap<Id, String>
}

impl IdTable {
    pub fn get_name(&self, id: Id) -> Option<&str> {
        self.names.get(&id).map(AsRef::as_ref)
    }
}

#[derive(Debug)]
pub struct IdFactory {
    counter: usize,
    id_names: HashMap<Id, String>,
    name_ids: HashMap<String, Id>
}

impl IdFactory {
    pub fn new() -> IdFactory {
        IdFactory {
            counter: 0,
            id_names: HashMap::new(),
            name_ids: HashMap::new()
        }
    }

    pub fn get(&mut self, name: &str) -> Id {
        self.name_ids.get(name).map(|&id| id)
                     .unwrap_or_else(|| self.fresh(name))
    }

    pub fn fresh(&mut self, name: &str) -> Id {
        let id = Id(self.counter);
        self.counter += 1;
        self.id_names.insert(id, name.to_string());
        self.name_ids.insert(name.to_string(), id);
        id
    }

    pub fn build(self) -> IdTable { IdTable { names: self.id_names } }
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
