use std::cell::RefCell;
use std::convert::TryFrom;
use combine::stream::{StreamOnce, Resetable};
use combine::error::StringStreamError;

use lexer::{Token, Lexer};
use cst::{Expr, Stmt, Pattern, Case, PrimOp, Def, Const,
          IllegalPattern, IdFactory, Pos, Positioned};

// ================================================================================================

#[derive(Debug)]
struct CstFactory {
    pos: Pos
}

impl CstFactory {
    fn new(pos: Pos) -> CstFactory {
        CstFactory { pos }
    }

    fn pos(&self) -> Pos { self.pos.clone() }

    fn function(&self, methods: Vec<Case>) -> Expr {
        let closure = Def::new("self");
        let method_i = Def::new("m");
        let args = Def::new("args");
        Expr::Function(self.pos(), vec![closure.clone(), method_i.clone(), args.clone()],
            Box::new(Expr::Match(
                self.pos(),
                Box::new(Expr::Lex(self.pos(), args.clone())),
                methods,
                Box::new(Expr::Call(self.pos(),
                                    Box::new(Expr::Lex(self.pos(), closure.clone())),
                                    vec![Expr::Lex(self.pos(), closure.clone()),
                                         Expr::PrimCall(self.pos(), PrimOp::IAdd,
                                                        vec![Expr::Lex(self.pos(), method_i),
                                                             Expr::Const(self.pos(),
                                                                         Const::Int(1))]),
                                         Expr::Lex(self.pos(), args)])
                )
            ))
        )
    }

    fn block(&self, stmts: Vec<Stmt>, expr: Expr) -> Expr {
        if stmts.is_empty() {
            expr
        } else {
            Expr::Block(self.pos(), stmts, Box::new(expr))
        }
    }

    fn call(&self, callee: Expr, args: Vec<Expr>) -> Expr {
        let apply = Expr::Lex(self.pos(), Def::new("apply"));
        let arg_tup = Expr::PrimCall(self.pos(), PrimOp::Tuple, args);
        let args = vec![apply.clone(),
                        Expr::Const(self.pos(), Const::Int(0)),
                        Expr::PrimCall(self.pos(), PrimOp::Tuple, vec![callee, arg_tup])];
        Expr::Call(self.pos(), Box::new(apply), args)
    }
}

// ================================================================================================

#[derive(Debug)]
pub enum ParseError {
    Lex(StringStreamError),
    Token {
        expected: Token,
        received: Token
    },
    Expr,
    Pattern(IllegalPattern)
}

impl From<StringStreamError> for ParseError {
    fn from(err: StringStreamError) -> ParseError { ParseError::Lex(err) }
}

impl From<IllegalPattern> for ParseError {
    fn from(err: IllegalPattern) -> ParseError { ParseError::Pattern(err) }
}

pub type ParseResult<T> = Result<T, ParseError>;

// ================================================================================================

fn try_parse<T, F>(lexer: &mut Lexer, f: F) -> ParseResult<T>
    where F: FnOnce(&mut Lexer) -> ParseResult<T>
{
    let checkpoint = lexer.checkpoint();
    f(lexer).map_err(|err| {
        lexer.reset(checkpoint);
        err
    })
}

fn token(lexer: &mut Lexer, expected: Token) -> ParseResult<Token> {
    try_parse(lexer, |lexer| {
        let tok = lexer.uncons()?;
        if tok == expected {
            Ok(tok)
        } else {
            Err(ParseError::Token { expected, received: tok })
        }
    })
}

fn optional<T, F>(lexer: &mut Lexer, f: F) -> ParseResult<Option<T>>
    where F: Fn(&mut Lexer) -> ParseResult<T>
{
    try_parse(lexer, f).map(Some).or(Ok(None))
}

fn many<T, F>(lexer: &mut Lexer, f: F) -> ParseResult<Vec<T>>
    where F: Fn(&mut Lexer) -> ParseResult<T>
{
    let mut vals = Vec::new();
    while let Ok(v) = f(lexer) {
        vals.push(v);
    }
    Ok(vals)
}

fn many1<T, F>(lexer: &mut Lexer, f: F) -> ParseResult<Vec<T>>
    where F: Fn(&mut Lexer) -> ParseResult<T>
{
    let mut vals = Vec::new();

    vals.push(try_parse(lexer, |lexer| f(lexer))?);
    while let Ok(v) = f(lexer) {
        vals.push(v);
    }

    Ok(vals)
}

fn sep1<T, S, F, G>(lexer: &mut Lexer, f: F, separator: G) -> ParseResult<Vec<T>>
    where F: Fn(&mut Lexer) -> ParseResult<T>,
          G: Fn(&mut Lexer) -> ParseResult<S>
{
    let mut vals = Vec::new();
    vals.push(f(lexer)?);
    while let Ok(v) = try_parse(lexer, |lexer| separator(lexer).and_then(|_| f(lexer))) {
        vals.push(v);
    }
    Ok(vals)
}

// ================================================================================================

pub fn program(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<Expr> {
    body(lexer, ids)
}

fn body(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<Expr> {
    fn body_parts(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<(Vec<Stmt>, Expr)> {
        let s = stmt(lexer, ids)?;
        token(lexer, Token::Semicolon)?;
        let (mut stmts, e) = body_work(lexer, ids)?;
        stmts.push(s);
        Ok((stmts, e))
    }

    fn body_work(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<(Vec<Stmt>, Expr)> {
        try_parse(lexer, |lexer| body_parts(lexer, ids))
            .or_else(|_| Ok((Vec::new(), expr(lexer, ids)?)))
    }

    body_work(lexer, ids).map(|(mut stmts, e)| {
        stmts.reverse();
        Expr::Block(Pos::default(), stmts, Box::new(e))
    })
}

fn stmt(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<Stmt> {
    try_parse(lexer, |lexer| {
        let pattern = Pattern::try_from(expr(lexer, ids)?)?;
        token(lexer, Token::Eq)?;
        let value = expr(lexer, ids)?;
        Ok(Stmt::Def(pattern, value))
    })
    .or_else(|_| expr(lexer, ids).map(Stmt::Expr))
}

fn expr(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<Expr> {
    call(lexer, ids)
}

fn call(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<Expr> {
    let callee = simple(lexer, ids)?;
    let args = many(lexer, |lexer| simple(lexer, ids))?;
    Ok(if !args.is_empty() {
        CstFactory::new(Pos::default()).call(callee, args)
    } else {
        callee
    })
}

fn simple(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<Expr> {
    try_parse(lexer, |lexer| match lexer.uncons()? {
        Token::LBrace =>
            try_parse(lexer, |lexer| {
                let methods = sep1(lexer, |lexer| method(lexer, ids),
                                          |lexer| token(lexer, Token::Semicolon))?;
                token(lexer, Token::RBrace)?;
                Ok(CstFactory::new(Pos::default()).function(methods))
            })
            .or_else(|_| {
                let res = body(lexer, ids)?;
                token(lexer, Token::RBrace)?;
                Ok(res)
            }),
        Token::LBracket => {
            let body = body(lexer, ids)?;
            token(lexer, Token::RBracket)?;
            Ok(Expr::Function(Pos::default(), /* FIXME: */ vec![], Box::new(body)))
        }
        Token::LParen => {
            let res = expr(lexer, ids)?;
            token(lexer, Token::RParen)?;
            Ok(res)
        },
        Token::Lex(name) => Ok(Expr::Lex(Pos::default(), ids.borrow_mut().usage(&name))),
        Token::Dyn(name) => Ok(Expr::Dyn(Pos::default(), name)),
        Token::Const(c) => Ok(Expr::Const(Pos::default(), c)),
        _ => Err(ParseError::Expr)
    })
}

fn method(lexer: &mut Lexer, ids: &RefCell<IdFactory>) -> ParseResult<Case> {
    let patterns = many1(lexer, |lexer| simple(lexer, ids))?.into_iter()
                       .map(Pattern::try_from)
                       .collect::<Result<Vec<_>, _>>()?;
    let guard = optional(lexer, |lexer| {
        token(lexer, Token::Bar)?;
        expr(lexer, ids)
    })?;
    token(lexer, Token::DArrow)?;
    let body = expr(lexer, ids)?;
    Ok(Case {
        pattern: Pattern::PrimCall(patterns[0].pos().clone(), PrimOp::Tuple, patterns),
        guard: guard.unwrap_or_else(|| Expr::Const(Pos::default(), Const::Bool(true))),
        body
    })
}
