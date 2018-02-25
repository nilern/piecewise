use std::convert::TryInto;
use std::cell::RefCell;
use combine::{many, many1, sep_by1, optional, between, eof, try, not_followed_by,
              satisfy_map, token, position};

use lexer::{Lexer, Token};
use cst::{Stmt, Expr, Pattern, Case, PrimOp, Const, Def, Use, IdFactory, Pos};

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
                Box::new(Expr::Lex(self.pos(), Use::new(args.clone()))),
                methods,
                Box::new(Case {
                    patterns: Vec::new(), // FIXME
                    guard: Expr::Const(self.pos(), Const::Bool(true)),
                    body: Expr::Call(self.pos(),
                                     Box::new(Expr::Lex(self.pos(),
                                                        Use::new(closure.clone()))),
                                     vec![Expr::Lex(self.pos(), Use::new(closure.clone())),
                                          Expr::PrimCall(self.pos(), PrimOp::IAdd,
                                                         vec![Expr::Lex(self.pos(),
                                                                  Use::new(method_i)),
                                                              Expr::Const(self.pos(),
                                                                          Const::Int(1))]),
                                          Expr::Lex(self.pos(), Use::new(args))])
                })
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
        fn actual_call(factory: &CstFactory, callee: Expr, args: Vec<Expr>) -> Expr {
            let args = vec![callee.clone(),
                            Expr::Const(factory.pos(), Const::Int(0)),
                            Expr::PrimCall(factory.pos(), PrimOp::Tuple, args)];
            Expr::Call(factory.pos(), Box::new(callee), args)
        }

        if callee.is_trivial() {
            actual_call(self, callee, args)
        } else {
            let f = Def::new("f");
            let f_use = Expr::Lex(self.pos(), Use::new(f.clone()));
            Expr::Block(self.pos(),
                        vec![Stmt::Def(Pattern::Lex(self.pos(), f), callee)],
                        Box::new(actual_call(self, f_use, args)))
        }
    }
}

// ================================================================================================

parser!{
    pub fn program['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        body(id_factory).skip(eof())
    }
}

parser!{
    fn body['a, 'input](ids: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        (position(), body_parts(ids))
        .map(|(pos, (mut stmts, expr))| {
            stmts.reverse();
            CstFactory::new(pos).block(stmts, expr)
        })
    }
}

parser!{
    fn body_parts['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>)
        -> (Vec<Stmt>, Expr)
    {
        try(
            (stmt(id_factory).skip(token(Token::Semicolon)),
                body_parts(id_factory)).map(|(stmt, mut body)| {
                     body.0.push(stmt);
                     body
                })
        ).or(expr(id_factory).map(|expr| (Vec::new(), expr)))
    }
}

parser!{
    fn stmt['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>) -> Stmt {
        (expr(id_factory), optional(token(Token::Eq).with(expr(id_factory))))
        .map(|(pattern, oval)| if let Some(val) = oval {
            Stmt::Def(pattern.try_into().unwrap(), val)
        } else {
            Stmt::Expr(pattern)
        })
    }
}

parser!{
    fn expr['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        call(id_factory)
    }
}

parser!{
    fn call['a, 'input](ids: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        (position(), simple(ids), many::<Vec<_>, _>(simple(ids))).map(|(pos, callee, args)|
            if !args.is_empty() {
                CstFactory::new(pos).call(callee, args)
            } else {
                callee
            }
        )
    }
}

parser!{
    fn simple['a, 'input](ids: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        choice!(
            try(
                (position(),
                 between(token(Token::LBrace), token(Token::RBrace),
                         sep_by1(method(ids), token(Token::Semicolon))))
            ).map(|(pos, methods)| CstFactory::new(pos).function(methods)),
            between(token(Token::LBrace), token(Token::RBrace), body(ids)),
            (position(),
             between(token(Token::LBracket), token(Token::RBracket), body(ids)))
                .map(|(pos, body)| Expr::Function(pos, /* FIXME: */ vec![], Box::new(body))),
            between(token(Token::LParen), token(Token::RParen), expr(ids)),
            var(ids),
            constant()
        )
    }
}

parser!{
    fn method['a, 'input](ids: &'a RefCell<IdFactory>)(Lexer<'input>) -> Case {
        (many1::<Vec<_>, _>(simple(ids)), position(), optional(token(Token::Bar).with(expr(ids))),
         token(Token::DArrow).with(method_body(ids)))
        .map(|(patterns, guard_pos, guard, body)|
            Case {
                patterns: patterns.into_iter().map(|pat| pat.try_into().unwrap()).collect(),
                guard: guard.unwrap_or_else(|| Expr::Const(guard_pos, Const::Bool(true))),
                body
            }
        )
    }
}

// FIXME: almost the same as `body`.
parser!{
    fn method_body['a, 'input](ids: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        (position(), method_body_parts(ids))
        .map(|(pos, (mut stmts, expr))| {
            stmts.reverse();
            CstFactory::new(pos).block(stmts, expr)
        })
    }
}

// FIXME: almost the same as `body_parts`.
parser!{
    fn method_body_parts['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>)
        -> (Vec<Stmt>, Expr)
    {
        try(
            (stmt(id_factory).skip(token(Token::Semicolon)),
                method_body_parts(id_factory)).map(|(stmt, mut body)| {
                     body.0.push(stmt);
                     body
                })
        ).or(expr(id_factory).skip(not_followed_by(token(Token::DArrow).or(token(Token::Bar))))
                             .map(|expr| (Vec::new(), expr)))
    }
}

parser!{
    fn var['a, 'input](ids: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        let lex = (
            position(), satisfy_map(Token::lex_name)
        ).map(|(pos, name)| Expr::Lex(pos, ids.borrow_mut().usage(&name)));

        let dyn = (
            position(), satisfy_map(Token::dyn_name)
        ).map(|(pos, name)| Expr::Dyn(pos, name));

        lex.or(dyn)
    }
}

parser!{
    fn constant['input]()(Lexer<'input>) -> Expr {
        (position(), satisfy_map(|token: Token| token.try_into().ok()))
        .map(|(pos, c)| Expr::Const(pos, c))
    }
}
