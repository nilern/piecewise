use pcws_domain::Allocator;
use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::values::Symbol;
use cst;
use ast;

// ================================================================================================

/// Like `Into`, but must produce a `ValueRef(T<_>)` and can use an `Allocator` to do it.
pub trait Inject {
    /// The type to convert to.
    type Target: Into<ValueRef>;

    /// Perform the conversion.
    fn inject(self, allocator: &mut Allocator) -> Option<Self::Target>;
}

// Then we implement the conversion using the obvious, although tedious, structural recursion:

impl Inject for cst::Expr {
    type Target = ValueRef;

    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        use cst::Expr::*;
    
        match self {
            Function(_, methods) =>
                methods.into_iter()
                       .map(|expr| expr.inject(allocator))
                       .collect::<Option<Vec<_>>>()
                       .and_then(|methods|
                          ast::Function::new(allocator, &methods).map(From::from)
                       ),
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
            Lex(_, name) =>
                Symbol::new(allocator, &name)
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

impl Inject for cst::Pattern {
    type Target = ValueRef;
    
    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        use cst::Pattern::*;
    
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
            Lex(_, name) =>
                Symbol::new(allocator, &name)
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

impl Inject for cst::Stmt {
    type Target = ValueRef;
    
    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        match self {
            cst::Stmt::Def(pattern, expr) =>
                pattern.inject(allocator)
                       .and_then(|pattern|
                           expr.inject(allocator)
                               .and_then(|expr|
                                   ast::Def::new(allocator, pattern, expr).map(From::from)
                               )
                       ),
            cst::Stmt::Expr(expr) => expr.inject(allocator)
        }
    }
}

impl Inject for cst::Method {
    type Target = ValueRefT<ast::Method>;
    
    fn inject(self, _: &mut Allocator) -> Option<ValueRefT<ast::Method>> {
        unimplemented!()
    }
}

impl Inject for cst::Const {
    type Target = ValueRef;
    
    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        match self {
            cst::Const::Int(n) => Some(ValueRefT::from(n).into()),
            cst::Const::Float(n) => Some(ValueRefT::from(n).into()),
            cst::Const::Char(c) => Some(ValueRefT::from(c).into()),
            cst::Const::Bool(b) => Some(ValueRefT::from(b).into()),
            cst::Const::String(_) => unimplemented!(),
            cst::Const::Symbol(cs) => Symbol::new(allocator, &cs).map(From::from)
        }
    }
}
