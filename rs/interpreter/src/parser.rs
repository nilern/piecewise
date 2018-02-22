use std::convert::TryInto;
use std::cell::RefCell;
use combine::{optional, eof, try, satisfy_map, token, position};

use lexer::{Lexer, Token};
use cst::{Stmt, Expr, IdFactory};

parser!{
    pub fn program['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>)
        -> (Vec<Stmt>, Expr)
    {
        body(id_factory).skip(eof())
    }
}

parser!{
    fn body['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>)
        -> (Vec<Stmt>, Expr)
    {
        try(
            (stmt(id_factory).skip(token(Token::Semicolon)),
                body(id_factory)).map(|(stmt, mut body)| {
                     body.0.insert(0, stmt);
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
        var(id_factory).or(constant())
    }
}

parser!{
    fn var['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        let lex = (
            position(), satisfy_map(Token::lex_name)
        ).map(|(pos, name)| Expr::Lex(pos, id_factory.borrow_mut().get(&name)));
        
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
