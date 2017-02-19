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
#[derive(Debug)]
pub enum Const {
    Int(isize),
    Float(f64),
    Char(char),
    String(String)
}

impl Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Const::*;

        match self {
            &Int(i) => write!(f, "{}", i),
            &Float(n) => write!(f, "{}", n),
            &Char(c) => write!(f, "{:?}", c),
            &String(ref cs) => write!(f, "\"{}\"", cs)
        }
    }
}

/// Abstract Syntax Tree
#[derive(Debug)]
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

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &AST::Block { ref stmts, .. } => {
                try!(write!(f, "{{"));
                let mut it = stmts.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for arg in it {
                    try!(write!(f, "; {}", arg));
                }
                write!(f, "}}")
            },
            &AST::Fn { ref clauses, .. } => {
                try!(write!(f, "{{"));
                let mut it = clauses.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for arg in it {
                    try!(write!(f, "; {}", arg));
                }
                write!(f, "}}")
            },
            &AST::App { ref op, ref args, .. } => {
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
            &AST::Var { ref name, .. } => write!(f, "{}", name),
            &AST::Const { ref val, .. } => write!(f, "{}", val)
        }
    }
}

/// Statement (for `Block`s).
#[derive(Debug)]
pub enum Stmt {
    Def { pos: SrcPos, name: String, val: AST },
    Expr { pos: SrcPos, expr: AST }
}

impl Sourced for Stmt {
    fn pos(&self) -> SrcPos {
        match self {
            &Stmt::Def { pos, .. } => pos,
            &Stmt::Expr { pos, .. } => pos
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Stmt::Def { ref name, ref val, ..} => write!(f, "{} = {}", name, val),
            &Stmt::Expr { ref expr, .. } => write!(f, "{}", expr)
        }
    }
}

/// Function clause.
#[derive(Debug)]
pub struct Clause {
    pub pos: SrcPos,
    pub params: AST, // TODO: Vec<AST>
    //pub cond: AST,
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
        // for param in self.params.iter() {
        //     try!(write!(f, "{} ", param));
        // }
        try!(write!(f, "{} => ", self.params));
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

        Some(AST::Block {
            pos: pos,
            decls: vec![],
            stmts: stmts
        })
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
                None => return Some(AST::Fn { pos: pos, clauses: clauses})
            }
        }
    }

    match items.first() {
        Some(&BlockItem::Clause(_)) => parse_fn_block(pos, items),
        Some(&BlockItem::Stmt(_)) => parse_stmt_block(pos, items),
        None => Some(AST::Block { pos: pos, decls: vec![], stmts: vec![] })
    }
}
