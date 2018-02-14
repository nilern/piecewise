use std::rc::Rc;
use std::collections::HashMap;

use pcws_domain::Allocator;
use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::values::Symbol;
use cst::{self, Program, Id, IdTable, IdFactory};
use ast;

// ================================================================================================

#[derive(Debug)]
pub enum Parsed {}

// ================================================================================================

#[derive(Debug)]
pub enum Alphatized {}

impl Program<Parsed> {
    pub fn alphatize(mut self) -> Program<Alphatized> {
        let mut ctx = AlphaCtx {
            old_ids: self.ids,
            id_factory: IdFactory::new(),
            env: Rc::new(AlphaEnv::TopLevel)
        };
        self.cst.alphatize(&mut ctx);
        Program::new(self.cst, ctx.id_factory.build())
    }
}

#[derive(Debug)]
struct AlphaCtx {
    old_ids: IdTable,
    id_factory: IdFactory,
    env: Rc<AlphaEnv>
}

#[derive(Debug)]
enum AlphaEnv {
    TopLevel,
    Nested {
        bindings: AlphaBindings,
        parent: Rc<AlphaEnv>
    }
}

impl AlphaEnv {
    fn push(parent: Rc<AlphaEnv>, bindings: HashMap<Id, Id>) -> AlphaEnv {
        AlphaEnv::Nested { bindings, parent }
    }

    fn get(&self, name: Id) -> Option<Id> {
        match *self {
            AlphaEnv::Nested { ref bindings, ref parent } =>
                bindings.get(&name).map(|&id| id).or_else(|| parent.get(name)),
            AlphaEnv::TopLevel => None
        }
    }
}

type AlphaBindings = HashMap<Id, Id>;

trait Alphatize {
    fn alphatize(&mut self, ctx: &mut AlphaCtx);
}

impl Alphatize for cst::Expr {
    fn alphatize(&mut self, ctx: &mut AlphaCtx) {
        use cst::Expr::*;

        fn pattern_definiends(pattern: &cst::Pattern, ctx: &mut AlphaCtx,
                              bindings: &mut AlphaBindings)
        {
            match *pattern {
                cst::Pattern::Call(_, _, ref args) =>
                    for arg in args { pattern_definiends(arg, ctx, bindings) },
                cst::Pattern::Lex(_, id) => {
                    let name = ctx.old_ids.get_name(id).unwrap();
                    bindings.insert(id, ctx.id_factory.fresh(name));
                },
                cst::Pattern::Dyn(..) | cst::Pattern::Const(..) => {}
            }
        }

        fn stmt_definiends(stmt: &cst::Stmt, ctx: &mut AlphaCtx, bindings: &mut AlphaBindings) {
            match *stmt {
                cst::Stmt::Def(ref pattern, _) => pattern_definiends(pattern, ctx, bindings),
                cst::Stmt::Expr(_) => {}
            }
        }

        match *self {
            Block(_, ref mut stmts, ref mut expr) => {
                let mut bindings = HashMap::new();
                for stmt in stmts.iter() {
                    stmt_definiends(stmt, ctx, &mut bindings)
                }
                ctx.env = Rc::new(AlphaEnv::push(ctx.env.clone(), bindings));

                for stmt in stmts { stmt.alphatize(ctx) }
                expr.alphatize(ctx);
            },
            Call(_, ref mut callee, ref mut args) => {
                callee.alphatize(ctx);
                for arg in args { arg.alphatize(ctx) }
            },
            Lex(_, ref mut name) =>
                *name = ctx.env.get(*name).unwrap(),
            Dyn(..) | Const(..) => {}
            _ => unimplemented!()
        }
    }
}

impl Alphatize for cst::Pattern {
    fn alphatize(&mut self, ctx: &mut AlphaCtx) {
        use cst::Pattern::*;

        match *self {
            Call(_, ref mut callee, ref mut args) => {
                callee.alphatize(ctx);
                for arg in args { arg.alphatize(ctx) }
            },
            Lex(_, ref mut name) =>
                *name = ctx.env.get(*name).unwrap(),
            Dyn(..) | Const(..) => {}
        }
    }
}

impl Alphatize for cst::Stmt {
    fn alphatize(&mut self, ctx: &mut AlphaCtx) {
        match *self {
            cst::Stmt::Def(ref mut pattern, ref mut expr) => {
                pattern.alphatize(ctx);
                expr.alphatize(ctx);
            },
            cst::Stmt::Expr(ref mut expr) => expr.alphatize(ctx)
        }
    }
}

// ================================================================================================

impl Program<Alphatized> {
    pub fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        self.cst.inject(&self.ids, allocator)
    }
}

/// Like `Into`, but must produce a `ValueRef(T<_>)`
/// and is provided with an `IdTable` and an `Allocator`.
trait Inject {
    /// The type to convert to.
    type Target: Into<ValueRef>;

    /// Perform the conversion.
    fn inject(self, ids: &IdTable, allocator: &mut Allocator) -> Option<Self::Target>;
}

// Then we implement the conversion using the obvious, although tedious, structural recursion:

impl Inject for cst::Expr {
    type Target = ValueRef;

    fn inject(self, ids: &IdTable, allocator: &mut Allocator) -> Option<ValueRef> {
        use cst::Expr::*;

        match self {
            Function(_, methods) =>
                methods.into_iter()
                       .map(|expr| expr.inject(ids, allocator))
                       .collect::<Option<Vec<_>>>()
                       .and_then(|methods|
                          ast::Function::new(allocator, &methods).map(From::from)
                       ),
            Block(_, stmts, expr) =>
                stmts.into_iter()
                     .map(|stmt| stmt.inject(ids, allocator))
                     .collect::<Option<Vec<_>>>()
                     .and_then(|stmts|
                         expr.inject(ids, allocator)
                             .and_then(|expr|
                                 ast::Block::new(allocator, &stmts, expr).map(From::from)
                             )
                     ),
            Call(_, callee, args) =>
                callee.inject(ids, allocator)
                      .and_then(|callee|
                          args.into_iter()
                              .map(|arg| arg.inject(ids, allocator))
                              .collect::<Option<Vec<_>>>()
                              .and_then(|args|
                                  ast::Call::new(allocator, callee, &args).map(From::from)
                              )
                      ),
            Lex(_, id) =>
                Symbol::new(allocator, ids.get_name(id).unwrap()) // FIXME: unwrap
                       .and_then(|name| ast::Lex::new(allocator, name).map(From::from)),
            Dyn(_, name) =>
                Symbol::new(allocator, &name)
                       .and_then(|name| ast::Dyn::new(allocator, name).map(From::from)),
            Const(_, c) =>
                c.inject(ids, allocator)
                 .and_then(|c| ast::Const::new(allocator, c).map(From::from))
        }
    }
}

impl Inject for cst::Pattern {
    type Target = ValueRef;

    fn inject(self, ids: &IdTable, allocator: &mut Allocator) -> Option<ValueRef> {
        use cst::Pattern::*;

        match self {
            Call(_, callee, args) =>
                callee.inject(ids, allocator)
                      .and_then(|callee|
                          args.into_iter()
                              .map(|arg| arg.inject(ids, allocator))
                              .collect::<Option<Vec<_>>>()
                              .and_then(|args|
                                  ast::Call::new(allocator, callee, &args).map(From::from)
                              )
                      ),
            Lex(_, id) =>
                Symbol::new(allocator, ids.get_name(id).unwrap()) // FIXME: unwrap
                       .and_then(|name| ast::Lex::new(allocator, name).map(From::from)),
            Dyn(_, name) =>
                Symbol::new(allocator, &name)
                       .and_then(|name| ast::Dyn::new(allocator, name).map(From::from)),
            Const(_, c) =>
                c.inject(ids, allocator)
                 .and_then(|c| ast::Const::new(allocator, c).map(From::from))
        }
    }
}

impl Inject for cst::Stmt {
    type Target = ValueRef;

    fn inject(self, ids: &IdTable, allocator: &mut Allocator) -> Option<ValueRef> {
        match self {
            cst::Stmt::Def(pattern, expr) =>
                pattern.inject(ids, allocator)
                       .and_then(|pattern|
                           expr.inject(ids, allocator)
                               .and_then(|expr|
                                   ast::Def::new(allocator, pattern, expr).map(From::from)
                               )
                       ),
            cst::Stmt::Expr(expr) => expr.inject(ids, allocator)
        }
    }
}

impl Inject for cst::Method {
    type Target = ValueRefT<ast::Method>;

    fn inject(self, _: &IdTable, _: &mut Allocator) -> Option<ValueRefT<ast::Method>> {
        unimplemented!()
    }
}

impl Inject for cst::Const {
    type Target = ValueRef;

    fn inject(self, _: &IdTable, allocator: &mut Allocator) -> Option<ValueRef> {
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
