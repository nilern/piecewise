use std::rc::Rc;
use std::collections::HashMap;

use pcws_domain::Allocator;
use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::values::Symbol;
use pcws_syntax::cst::{self, Program, Parsed, Id, IdTable, IdFactory};
use ast;

// ================================================================================================

#[derive(Debug)]
pub enum Alphatized {}

pub trait AlphatizationPass {
    type NextIR;

    fn alphatize(self) -> Self::NextIR;
}

impl AlphatizationPass for Program<Parsed> {
    type NextIR = Program<Alphatized>;

    fn alphatize(mut self) -> Program<Alphatized> {
        let mut id_factory = IdFactory::new();
        let env = Rc::new(AlphaEnv::TopLevel);
        self.cst.alphatize(&self.ids, &mut id_factory, &env);
        Program::new(self.cst, id_factory.build())
    }
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
    fn alphatize(&mut self, old_ids: &IdTable, id_factory: &mut IdFactory, env: &Rc<AlphaEnv>);
}

impl Alphatize for cst::Expr {
    fn alphatize(&mut self, old_ids: &IdTable, id_factory: &mut IdFactory, env: &Rc<AlphaEnv>) {
        use pcws_syntax::cst::Expr::*;

        match *self {
            Function(_, ref params, ref mut body) => {
                let mut bindings = HashMap::new();
                for param in params {
                    let name = old_ids.get_name(*param).unwrap();
                    bindings.insert(*param, id_factory.fresh(name));
                }
                let env = Rc::new(AlphaEnv::push(env.clone(), bindings));

                body.alphatize(old_ids, id_factory, &env);
            },
            Block(_, ref mut stmts, ref mut expr) => {
                let mut bindings = HashMap::new();
                for stmt in stmts.iter() {
                    stmt_definiends(stmt, old_ids, id_factory, &mut bindings)
                }
                let env = Rc::new(AlphaEnv::push(env.clone(), bindings));

                for stmt in stmts { stmt.alphatize(old_ids, id_factory, &env) }
                expr.alphatize(old_ids, id_factory, &env);
            },
            Match(_, ref mut cases, ref mut default_case) => {
                for case in cases { case.alphatize(old_ids, id_factory, env) }
                default_case.alphatize(old_ids, id_factory, env);
            },
            Call(_, ref mut callee, ref mut args) => {
                callee.alphatize(old_ids, id_factory, env);
                for arg in args { arg.alphatize(old_ids, id_factory, env) }
            },
            Lex(_, ref mut name) =>
                *name = env.get(*name).unwrap(),
            Dyn(..) | Const(..) => {}
        }
    }
}

impl Alphatize for cst::Pattern {
    fn alphatize(&mut self, old_ids: &IdTable, id_factory: &mut IdFactory, env: &Rc<AlphaEnv>) {
        use pcws_syntax::cst::Pattern::*;

        match *self {
            Call(_, ref mut callee, ref mut args) => {
                callee.alphatize(old_ids, id_factory, env);
                for arg in args { arg.alphatize(old_ids, id_factory, env) }
            },
            Lex(_, ref mut name) =>
                *name = env.get(*name).unwrap(),
            Dyn(..) | Const(..) => {}
        }
    }
}

impl Alphatize for cst::Stmt {
    fn alphatize(&mut self, old_ids: &IdTable, id_factory: &mut IdFactory, env: &Rc<AlphaEnv>) {
        match *self {
            cst::Stmt::Def(ref mut pattern, ref mut expr) => {
                pattern.alphatize(old_ids, id_factory, env);
                expr.alphatize(old_ids, id_factory, env);
            },
            cst::Stmt::Expr(ref mut expr) => expr.alphatize(old_ids, id_factory, env)
        }
    }
}

impl Alphatize for cst::Case {
    fn alphatize(&mut self, old_ids: &IdTable, id_factory: &mut IdFactory, env: &Rc<AlphaEnv>) {
        let &mut cst::Case { ref mut patterns, ref mut guard, ref mut body } = self;

        let mut bindings = HashMap::new();
        for pattern in patterns.iter() {
            pattern_definiends(pattern, old_ids, id_factory, &mut bindings);
        }
        let env = Rc::new(AlphaEnv::push(env.clone(), bindings));

        for pattern in patterns { pattern.alphatize(old_ids, id_factory, &env) }
        guard.alphatize(old_ids, id_factory, &env);
        body.alphatize(old_ids, id_factory, &env);
    }
}

fn pattern_definiends(pattern: &cst::Pattern, old_ids: &IdTable, id_factory: &mut IdFactory,
                      bindings: &mut AlphaBindings)
{
    match *pattern {
        cst::Pattern::Call(_, _, ref args) =>
            for arg in args { pattern_definiends(arg, old_ids, id_factory, bindings) },
        cst::Pattern::Lex(_, id) => {
            let name = old_ids.get_name(id).unwrap();
            bindings.insert(id, id_factory.fresh(name));
        },
        cst::Pattern::Dyn(..) | cst::Pattern::Const(..) => {}
    }
}

fn stmt_definiends(stmt: &cst::Stmt, old_ids: &IdTable, id_factory: &mut IdFactory,
                   bindings: &mut AlphaBindings)
{
    match *stmt {
        cst::Stmt::Def(ref pattern, _) =>
            pattern_definiends(pattern, old_ids, id_factory, bindings),
        cst::Stmt::Expr(_) => {}
    }
}

// ================================================================================================

pub trait InjectionPass {
    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef>;
}

impl InjectionPass for Program<Alphatized> {
    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
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
        use pcws_syntax::cst::Expr::*;

        match self {
            Function(..) => unimplemented!(),
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
            Match(..) => unimplemented!(),
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
        use pcws_syntax::cst::Pattern::*;

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

impl Inject for cst::Case {
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
