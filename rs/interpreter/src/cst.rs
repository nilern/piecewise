use std::rc::Rc;
use std::collections::HashMap;

// ================================================================================================

#[derive(Debug)]
pub enum Expr {
    Function(Pos, Vec<Method>),
    Block(Pos, Vec<Stmt>, Box<Expr>),
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
pub enum Stmt {
    Def(Pattern, Expr),
    Expr(Expr)
}

#[derive(Debug)]
pub struct Method {
    pub params: Vec<Pattern>,
    pub guard: Expr,
    pub body: Expr
}

#[derive(Debug)]
pub enum Const {
    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Symbol(String)
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

    pub fn create(&mut self, name: &str) -> Id {
        self.name_ids.get(name).map(|&id| id)
                     .unwrap_or_else(|| {
                         let id = Id(self.counter);
                         self.counter += 1;
                         self.id_names.insert(id, name.to_string());
                         self.name_ids.insert(name.to_string(), id);
                         id
                     })
    }

    pub fn build(self) -> IdTable { IdTable { names: self.id_names } }
}

// ================================================================================================

#[derive(Debug)]
pub struct Pos {
    pub file: Rc<String>,
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

impl Positioned for Method {
    fn pos(&self) -> &Pos {
        self.params.get(0).map(Pattern::pos)
            .unwrap_or_else(|| self.guard.pos())
    }
}
