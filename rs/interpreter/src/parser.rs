use std::convert::TryInto;
use std::cell::RefCell;
use combine::{eof, try, many, satisfy_map, token, position};

use lexer::{Lexer, Token, Separator, Var};
use cst::{Stmt, Expr, Const, IdFactory};

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
            (stmt(id_factory).skip(token(Token::Separator(Separator::Semicolon))),
                body(id_factory)).map(|(stmt, mut body)| {
                     body.0.insert(0, stmt);
                     body
                })
        ).or(expr(id_factory).map(|expr| (Vec::new(), expr)))
    }
}

parser!{
    fn stmt['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>) -> Stmt {
        try(
            (expr(id_factory).skip(token(Token::Eq)), expr(id_factory))
                .map(|(pattern, value)|
                    Stmt::Def(pattern.try_into().unwrap(), value)
                )
        ).or(expr(id_factory).map(Stmt::Expr))
    }
}

parser!{
    fn expr['a, 'input](id_factory: &'a RefCell<IdFactory>)(Lexer<'input>) -> Expr {
        let int = satisfy_map(|token: Token| token.try_into().ok().map(Const::Int));
        let string = satisfy_map(|token: Token| token.try_into().ok().map(Const::String));

        let constant = (position(), int.or(string)).map(|(pos, c)| Expr::Const(pos, c));

        let lex = (position(), satisfy_map(|token: Token|
            token.try_into().ok().and_then(|var| if let Var::Lex(name) = var {
                Some(id_factory.borrow_mut().get(&name))
            } else {
                None
            })
        )).map(|(pos, id)| Expr::Lex(pos, id));
        let dyn = (position(), satisfy_map(|token: Token|
            token.try_into().ok().and_then(|var| if let Var::Dyn(name) = var {
                Some(name)
            } else {
                None
            })
        )).map(|(pos, name)| Expr::Dyn(pos, name));

        let var = lex.or(dyn);

        var.or(constant)
    }
}
