use pcws_domain::Allocator;
use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::values::{String, Symbol, Tuple};
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

        fn pattern_binders(pat: ValueRef, lbs: &mut Vec<ValueRef>, dbs: &mut Vec<ValueRef>) {
            typecase!(pat, {
                lvar: ast::Lex => lbs.push(lvar.name().into()),
                dvar: ast::Dyn => dbs.push(dvar.name().into()),
                _ => unimplemented!()
            })
        }

        fn stmt_binders(stmt: ValueRef, lbs: &mut Vec<ValueRef>, dbs: &mut Vec<ValueRef>) {
            typecase!(stmt, {
                def: ast::Def => pattern_binders(def.pattern(), lbs, dbs),
                _ => {}
            })
        }

        fn block_binders(stmts: &[ValueRef]) -> (Vec<ValueRef>, Vec<ValueRef>) {
            let mut lbs = Vec::new();
            let mut dbs = Vec::new();

            for stmt in stmts {
                stmt_binders(*stmt, &mut lbs, &mut dbs);
            }

            (lbs, dbs)
        }

        match self {
            Function(_, params, body) =>
                params.into_iter()
                      .map(|param| Symbol::new(allocator, &param.borrow().name))
                      .collect::<Option<Vec<_>>>()
                      .and_then(|params|
                          body.inject(allocator)
                              .and_then(|body|
                                  ast::Function::new(allocator, &params, body).map(From::from)
                              )
                      ),
            Block(_, stmts, expr) =>
                stmts.into_iter()
                     .map(|stmt| stmt.inject(allocator))
                     .collect::<Option<Vec<_>>>()
                     .and_then(|stmts|
                         expr.inject(allocator)
                             .and_then(|expr| {
                                 let (lbs, dbs) = block_binders(&stmts);
                                 Tuple::new(allocator, lbs.len(), lbs.into_iter())
                                     .and_then(|lbs|
                                          Tuple::new(allocator, dbs.len(), dbs.into_iter())
                                              .and_then(|dbs|
                                                  ast::Block::new(allocator, lbs, dbs,
                                                                  &stmts, expr).map(From::from)
                                              )
                                     )
                             })
                     ),
            Match(_, matchee, cases, default) =>
                matchee.inject(allocator)
                       .and_then(|matchee|
                           cases.into_iter()
                                .map(|case| case.inject(allocator))
                                .collect::<Option<Vec<_>>>()
                                .and_then(|cases|
                                    default.inject(allocator)
                                           .and_then(|default|
                                               ast::Match::new(allocator, matchee, &cases, default)
                                                   .map(From::from)
                                           )
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
            PrimCall(_, op, args) =>
                args.into_iter()
                    .map(|arg| arg.inject(allocator))
                    .collect::<Option<Vec<_>>>()
                    .and_then(|args|
                        ast::PrimCall::new(allocator, op, &args).map(From::from)
                    ),
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
            PrimCall(_, op, args) =>
                args.into_iter()
                    .map(|arg| arg.inject(allocator))
                    .collect::<Option<Vec<_>>>()
                    .and_then(|args|
                        ast::PrimCall::new(allocator, op, &args).map(From::from)
                    ),
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
    type Target = ValueRefT<ast::Case>;

    fn inject(self, allocator: &mut Allocator) -> Option<ValueRefT<ast::Case>> {
        let Case { pattern, guard, body } = self;

        pattern.inject(allocator)
               .and_then(|pattern|
                   guard.inject(allocator)
                        .and_then(|guard|
                            body.inject(allocator)
                                .and_then(|body|
                                    ast::Case::new(allocator, pattern, guard, body).map(From::from)
                                )
                        )
               )
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
            Const::String(cs) => String::new(allocator, &cs).map(From::from),
            Const::Symbol(cs) => Symbol::new(allocator, &cs).map(From::from)
        }
    }
}
