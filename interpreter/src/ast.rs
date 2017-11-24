use std::fmt::Debug;
use gc::{Gc, Trace, Finalize};

use value::Value;

#[derive(Debug, Trace, Finalize)]
pub enum Pattern {
    Call(Call<Pattern>),
    Name(Name),
    Const(Gc<Value>)
}

#[derive(Debug, Trace, Finalize)]
pub enum Expr {
    Block(Block),
    Function(Gc<Function>),
    Call(Call<Expr>),
    PrimCall(PrimCall),
    
    NameRef(Name),
    Const(Gc<Value>)
}

#[derive(Debug, Trace, Finalize)]
pub enum Stmt {
    Def(Def),
    Aug(Aug),
    Expr(Expr)
}

#[derive(Debug, Trace, Finalize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Gc<Expr>
}

#[derive(Debug, Trace, Finalize)]
pub struct Function(pub Vec<Method>);

#[derive(Debug, Trace, Finalize)]
pub struct Method {
    pub params: Vec<Pattern>,
    pub guard: Expr,
    pub body: Expr
}

#[derive(Debug, Trace, Finalize)]
pub struct Call<T> where T: 'static + Debug + Trace + Finalize {
    callee: Gc<T>,
    args: Vec<T>
}

#[derive(Debug, Trace, Finalize)]
pub struct PrimCall {
    pub op: Primop,
    pub args: Vec<Expr>
}

#[derive(Debug, Trace, Finalize)]
pub enum Name {
    Lex(String),
    Dyn(String)
}

#[derive(Debug, Trace, Finalize)]
pub struct Def {
    pattern: Pattern,
    guard: Expr,
    value: Expr
}

#[derive(Debug, Trace, Finalize)]
pub struct Aug {
    pattern: Pattern,
    guard: Expr,
    value: Expr
}

#[derive(Debug, Trace, Finalize)]
pub enum Primop {
    IntAdd, IntSub, IntMul, IntDiv, IntRem
}
