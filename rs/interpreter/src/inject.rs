use pcws_domain::Allocator;
use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::values::Symbol;
use pcws_syntax::cst::{Expr, Stmt, Pattern, Case, Const};

use ast;

// ================================================================================================

/// Like `Into`, but must produce a `ValueRef(T<_>)` and is provided with an `Allocator`.
pub trait Inject {
    /// The type to convert to.
    type Target: Into<ValueRef>;

    /// Perform the conversion.
    fn inject(self, allocator: &mut Allocator) -> Option<Self::Target>;
}

// Then we implement the conversion using the obvious, although tedious, structural recursion:

impl Inject for Expr {
    type Target = ValueRef;

    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        use self::Expr::*;

        match self {
            Function(..) => unimplemented!(),
            Block(_, stmts, expr) =>
                stmts.into_iter()
                     .map(|stmt| stmt.inject(allocator))
                     .collect::<Option<Vec<_>>>()
                     .and_then(|stmts|
                         expr.inject(allocator)
                             .and_then(|expr|
                                 ast::Block::new(allocator, &stmts, expr).map(From::from)
                             )
                     ),
            Match(..) => unimplemented!(),
            Call(_, callee, args) =>
                callee.inject(allocator)
                      .and_then(|callee|
                          args.into_iter()
                              .map(|arg| arg.inject(allocator))
                              .collect::<Option<Vec<_>>>()
                              .and_then(|args|
                                  ast::Call::new(allocator, callee, &args).map(From::from)
                              )
                      ),
            PrimCall(..) => unimplemented!(),
            Lex(_, def) =>
                Symbol::new(allocator, &def.borrow().name)
                       .and_then(|name| ast::Lex::new(allocator, name).map(From::from)),
            Dyn(_, name) =>
                Symbol::new(allocator, &name)
                       .and_then(|name| ast::Dyn::new(allocator, name).map(From::from)),
            Const(_, c) =>
                c.inject(allocator)
                 .and_then(|c| ast::Const::new(allocator, c).map(From::from))
        }
    }
}

impl Inject for Pattern {
    type Target = ValueRef;

    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        use self::Pattern::*;

        match self {
            Call(_, callee, args) =>
                callee.inject(allocator)
                      .and_then(|callee|
                          args.into_iter()
                              .map(|arg| arg.inject(allocator))
                              .collect::<Option<Vec<_>>>()
                              .and_then(|args|
                                  ast::Call::new(allocator, callee, &args).map(From::from)
                              )
                      ),
            PrimCall(..) => unimplemented!(),
            Lex(_, def) =>
                Symbol::new(allocator, &def.borrow().name)
                       .and_then(|name| ast::Lex::new(allocator, name).map(From::from)),
            Dyn(_, name) =>
                Symbol::new(allocator, &name)
                       .and_then(|name| ast::Dyn::new(allocator, name).map(From::from)),
            Const(_, c) =>
                c.inject(allocator)
                 .and_then(|c| ast::Const::new(allocator, c).map(From::from))
        }
    }
}

impl Inject for Stmt {
    type Target = ValueRef;

    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        match self {
            Stmt::Def(pattern, expr) =>
                pattern.inject(allocator)
                       .and_then(|pattern|
                           expr.inject(allocator)
                               .and_then(|expr|
                                   ast::Def::new(allocator, pattern, expr).map(From::from)
                               )
                       ),
            Stmt::Expr(expr) => expr.inject(allocator)
        }
    }
}

impl Inject for Case {
    type Target = ValueRefT<ast::Method>;

    fn inject(self, _: &mut Allocator) -> Option<ValueRefT<ast::Method>> {
        unimplemented!()
    }
}

impl Inject for Const {
    type Target = ValueRef;

    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        match self {
            Const::Int(n) => Some(ValueRefT::from(n).into()),
            Const::Float(n) => Some(ValueRefT::from(n).into()),
            Const::Char(c) => Some(ValueRefT::from(c).into()),
            Const::Bool(b) => Some(ValueRefT::from(b).into()),
            Const::String(_) => unimplemented!(),
            Const::Symbol(cs) => Symbol::new(allocator, &cs).map(From::from)
        }
    }
}
