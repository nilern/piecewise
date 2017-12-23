use std::convert::TryFrom;
use std::fmt::Debug;
use std::iter::Peekable;
use gc::{Gc, Trace, Finalize};

use value::Value;

#[derive(Debug)]
pub struct PatternError;

#[derive(Debug)]
pub struct ClauseError;

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Pattern {
    Call(Call<Pattern>),
    Name(Name),
    Const(Gc<Value>)
}

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Expr {
    Block(Block),
    Function(Gc<Function>),
    Call(Call<Expr>),
    PrimCall(PrimCall),

    NameRef(Name),
    Const(Gc<Value>)
}

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Stmt {
    Def(Def),
    Aug(Aug),
    Expr(Expr)
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Gc<Expr>
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct Function(pub Vec<Method>);

#[derive(Debug, Clone, Trace, Finalize)]
pub struct Method {
    pub pattern: Pattern,
    pub guard: Expr,
    pub body: Expr
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct Call<T> where T: Debug + Trace + Finalize {
    pub callee: Gc<Expr>,
    pub args: Vec<T>
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct PrimCall {
    pub op: Primop,
    pub args: Vec<Expr>
}

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Name {
    Lex(String),
    Dyn(String)
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct Def {
    pub pattern: Pattern,
    pub guard: Expr,
    pub value: Expr
}

#[derive(Debug, Clone, Trace, Finalize)]
pub struct Aug {
    pub pattern: Pattern,
    pub guard: Expr,
    pub value: Expr
}

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Primop {
    IntAdd, IntSub, IntMul, IntDiv, IntRem
}

// -------------------------------------------------------------------------------------------------

impl<'a> TryFrom<&'a Expr> for Pattern {
    type Error = PatternError;

    fn try_from(expr: &Expr) -> Result<Pattern, PatternError> {
        match expr {
            &Expr::Call(Call { ref callee, ref args }) => Ok(Pattern::Call(Call {
                callee: callee.clone(),
                args: args.iter().map(Pattern::try_from).collect::<Result<Vec<_>, _>>()?
            })),
            &Expr::NameRef(ref name) => Ok(Pattern::Name(name.clone())),
            &Expr::Const(ref c) => Ok(Pattern::Const(c.clone())),
            &Expr::Block(_) | &Expr::Function(_) | &Expr::PrimCall(_) => Err(PatternError)
        }
    }
}

// -------------------------------------------------------------------------------------------------

pub enum Clause {
    Method {
        pattern: Pattern,
        guard: Expr,
        body: Stmt
    },
    Stmt(Stmt)
}

pub fn analyze_clauses(clauses: Vec<Clause>) -> Result<Expr, ClauseError> {
    fn cparse_body<I>(mut stmts: Vec<Stmt>, clauses: &mut Peekable<I>) -> Result<Block, ClauseError>
        where I: Iterator<Item=Clause>
    {
        while let Some(&Clause::Stmt(_)) = clauses.peek() {
            if let Some(Clause::Stmt(ref stmt)) = clauses.next() {
                stmts.push(stmt.clone());
            } else {
                unreachable!()
            }
        }

        if let Some(Stmt::Expr(ref expr)) = stmts.pop() {
            Ok(Block { stmts, expr: Gc::new(expr.clone()) })
        } else {
            Err(ClauseError)
        }
    }

    fn cparse_function<I>(clauses: &mut Peekable<I>) -> Result<Function, ClauseError>
        where I: Iterator<Item=Clause>
    {
        let mut methods = Vec::new();

        loop {
            let (pattern, guard, stmt) = match clauses.next() {
                Some(Clause::Method { pattern, guard, body }) => (pattern, guard, body),
                Some(_) => return Err(ClauseError),
                None => break
            };
            let body = cparse_body(vec![stmt], clauses)?;
            methods.push(Method { pattern, guard, body: Expr::Block(body) });
        }

        if methods.len() > 0 {
            Ok(Function(methods))
        } else {
            Err(ClauseError)
        }
    }

    let mut clauses = clauses.into_iter().peekable();
    match clauses.peek() {
        Some(&Clause::Method { .. }) =>
            cparse_function(&mut clauses).map(|f| Expr::Function(Gc::new(f))),
        Some(&Clause::Stmt(_)) => {
            let body = cparse_body(Vec::new(), &mut clauses)?;
            if clauses.peek().is_none() {
                Ok(Expr::Block(body))
            } else {
                Err(ClauseError)
            }
        },
        None => Err(ClauseError)
    }
}
