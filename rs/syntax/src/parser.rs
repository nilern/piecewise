use std::convert::TryInto;
use std::cell::RefCell;
use combine::{many, many1, sep_by1, optional, between, eof, try, not_followed_by,
              satisfy_map, token, position};

use lexer::{Lexer, Token};
use cst::{Stmt, Expr, Case, Const, Def, Use, IdFactory};

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
            Expr::Block(pos, stmts, Box::new(expr))
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
                Expr::Call(pos, Box::new(callee), args)
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
            ).map(|(pos, methods)| {
                let closure = Def::new("self");
                let args = Def::new("args");
                Expr::Function(pos.clone(), vec![closure.clone(), args.clone()],
                    Box::new(Expr::Match(
                        pos.clone(),
                        Box::new(Expr::Lex(pos.clone(), Use::new(args.clone()))),
                        methods,
                        Box::new(Case {
                            patterns: Vec::new(), // FIXME
                            guard: Expr::Const(pos.clone(), Const::Bool(true)),
                            body: Expr::Call(pos.clone(),
                                             Box::new(Expr::Lex(pos.clone(),
                                                                Use::new(closure.clone()))),
                                             vec![Expr::Lex(pos, Use::new(args))])
                        })
                    ))
                )
            }),
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
        (many1::<Vec<_>, _>(expr(ids)), position(), optional(token(Token::Bar).with(expr(ids))),
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
            Expr::Block(pos, stmts, Box::new(expr))
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
