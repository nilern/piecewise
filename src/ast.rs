use lexer::SrcPos;

use std::fmt;
use std::fmt::Display;

/// Values that originated in source code, IR trees and suchlike.
trait Sourced {
    /// Return the source position.
    fn pos(&self) -> SrcPos;
}

/// A type for variable names.
pub type Name = String;

/// Source constants.
pub enum Const {
    Int(isize),
    Float(f64),
    Char(char),
    String(String)
}

/// Concrete Syntax Tree.
#[derive(Debug)]
pub enum CST {
    Block(Vec<CST>),          // {foo; bar}
    Def(Box<CST>, Box<CST>),   // foo = bar
    Params(Box<CST>, Box<CST>), // foo bar => baz
    App(Box<CST>, Vec<CST>),   // foo bar

    Tuple(Vec<CST>),           // (,)
    Array(Vec<CST>),           // [,]
    // Set(Vec<CST>),             // {,}
    // Map(Vec<(CST, CST)>),      // {:,}

    Symbol(String),            // foo
    Int(isize)                 // 123
}

impl Display for CST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &CST::App(ref op, ref args) => {
                try!(write!(f, "({} ", op));
                let mut it = args.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for arg in it {
                    try!(write!(f, " {}", arg));
                }
                write!(f, ")")
            },
            &CST::Block(ref stmts) => {
                try!(write!(f, "{{"));
                let mut it = stmts.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for arg in it {
                    try!(write!(f, " {}", arg));
                }
                write!(f, "}}")
            },
            &CST::Def(ref pat, ref val) => write!(f, "{} = {}", pat, val),
            &CST::Params(ref pat, ref val) => write!(f, "{} => {}", pat, val),
            &CST::Tuple(ref vals) => {
                try!(write!(f, "("));
                let mut it = vals.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for v in it {
                    try!(write!(f, ", {}", v));
                }
                write!(f, ")")
            },
            &CST::Array(ref vals) => {
                try!(write!(f, "["));
                let mut it = vals.iter();
                if let Some(v) = it.next() {
                    try!(write!(f, "{}", v));
                }
                for v in it {
                    try!(write!(f, ", {}", v));
                }
                write!(f, "]")
            },
            &CST::Symbol(ref chars) => write!(f, "{}", chars),
            &CST::Int(i) => write!(f, "{}", i)
        }
    }
}

/// Abstract Syntax Tree
pub enum AST {
    Block { pos: SrcPos, decls: Vec<String>, stmts: Vec<Stmt> },
    Fn { pos: SrcPos, clauses: Vec<Clause> },
    App { pos: SrcPos, op: Box<AST>, args: Vec<AST> },

    Var { pos: SrcPos, name: String },
    Const { pos: SrcPos, val: Const }
}

impl Sourced for AST {
    fn pos(&self) -> SrcPos {
        match self {
            &AST::Block { pos, .. } => pos,
            &AST::Fn { pos, .. } => pos,
            &AST::App { pos, .. } => pos,
            &AST::Var { pos, .. } => pos,
            &AST::Const { pos, .. } => pos
        }
    }
}

/// Statement (for `Block`s).
pub enum Stmt {
    Def { pos: SrcPos, name: String, val: AST },
    Expr { pos: SrcPos, expr: AST}
}

impl Sourced for Stmt {
    fn pos(&self) -> SrcPos {
        match self {
            &Stmt::Def { pos, .. } => pos,
            &Stmt::Expr { pos, .. } => pos
        }
    }
}

/// Function clause.
pub struct Clause {
    pos: SrcPos,
    params: Vec<AST>,
    cond: AST,
    body: AST
}

impl Sourced for Clause {
    fn pos(&self) -> SrcPos { self.pos }
}
