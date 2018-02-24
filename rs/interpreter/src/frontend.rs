use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use pcws_domain::Allocator;
use pcws_domain::object_model::{ValueRef, ValueRefT};
use pcws_domain::values::Symbol;
use pcws_syntax::cst::{self, Program, Parsed, Use, DefRef};
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
        let env = Rc::new(AlphaEnv::TopLevel);
        self.cst.alphatize(&env);
        Program::new(self.cst)
    }
}

type AlphaBindings = HashMap<String, DefRef>;

#[derive(Debug)]
enum AlphaEnv {
    TopLevel,
    Nested {
        bindings: AlphaBindings,
        parent: Rc<AlphaEnv>
    }
}

impl AlphaEnv {
    fn push(parent: Rc<AlphaEnv>, bindings: HashMap<String, DefRef>) -> AlphaEnv {
        AlphaEnv::Nested { bindings, parent }
    }

    fn get(&self, name: &str) -> Option<DefRef> {
        match *self {
            AlphaEnv::Nested { ref bindings, ref parent } =>
                bindings.get(name).map(Clone::clone).or_else(|| parent.get(name)),
            AlphaEnv::TopLevel => None
        }
    }
}

trait Alphatize {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>);
}

impl Alphatize for cst::Expr {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        use pcws_syntax::cst::Expr::*;

        match *self {
            // Binding Exprs:
            Function(_, ref mut params, ref mut body) => {
                let mut bindings = HashMap::new();
                for param in params.iter_mut() {
                    bindings.insert(param.borrow().name.clone(), param.clone());
                }
                let env = Rc::new(AlphaEnv::push(env.clone(), bindings));

                body.alphatize(&env);
            },
            Block(_, ref mut stmts, ref mut expr) => {
                let mut bindings = HashMap::new();
                for stmt in stmts.iter_mut() {
                    stmt_definiends(stmt, &mut bindings)
                }
                let env = Rc::new(AlphaEnv::push(env.clone(), bindings));

                for stmt in stmts { stmt.alphatize(&env) }
                expr.alphatize(&env);
            },

            // Just recurse to subexprs:
            Match(_, ref mut matchee, ref mut cases, ref mut default_case) => {
                matchee.alphatize(env);
                for case in cases { case.alphatize(env) }
                default_case.alphatize(env);
            },
            Call(_, ref mut callee, ref mut args) => {
                callee.alphatize(env);
                for arg in args { arg.alphatize(env) }
            },

            // Update Use:
            Lex(_, ref mut usage) => {
                let def = env.get(&usage.def.borrow().name).unwrap(); // FIXME: unwrap
                *usage = Use::new(def);
            },

            // These don't contain defs or uses at all:
            Dyn(..) | Const(..) => {}
        }
    }
}

impl Alphatize for cst::Pattern {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        use pcws_syntax::cst::Pattern::*;

        // Here we only need to do the expressions, i.e. only the Call callees.
        match *self {
            Call(_, ref mut callee, ref mut args) => {
                callee.alphatize(env);
                for arg in args { arg.alphatize(env) }
            },
            Lex(..) | Dyn(..) | Const(..) => {}
        }
    }
}

impl Alphatize for cst::Stmt {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        match *self {
            cst::Stmt::Def(ref mut pattern, ref mut expr) => {
                pattern.alphatize(env);
                expr.alphatize(env);
            },
            cst::Stmt::Expr(ref mut expr) => expr.alphatize(env)
        }
    }
}

impl Alphatize for cst::Case {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        let &mut cst::Case { ref mut patterns, ref mut guard, ref mut body } = self;

        let mut bindings = HashMap::new();
        for pattern in patterns.iter_mut() {
            pattern_definiends(pattern, &mut bindings);
        }
        let env = Rc::new(AlphaEnv::push(env.clone(), bindings));

        for pattern in patterns { pattern.alphatize(&env) }
        guard.alphatize(&env);
        body.alphatize(&env);
    }
}

fn pattern_definiends(pattern: &mut cst::Pattern, bindings: &mut AlphaBindings) {
    match *pattern {
        cst::Pattern::Call(_, _, ref mut args) =>
            for arg in args.iter_mut() { pattern_definiends(arg, bindings) },

        cst::Pattern::Lex(_, ref mut def) => {
            let new_def = Rc::new(RefCell::new(def.borrow().clone()));
            *def = new_def.clone();
            let name = new_def.borrow().name.clone();
            bindings.insert(name, new_def);
        },

        cst::Pattern::Dyn(..) | cst::Pattern::Const(..) => {}
    }
}

fn stmt_definiends(stmt: &mut cst::Stmt, bindings: &mut AlphaBindings) {
    match *stmt {
        cst::Stmt::Def(ref mut pattern, _) =>
            pattern_definiends(pattern, bindings),
        cst::Stmt::Expr(_) => {}
    }
}

// ================================================================================================

pub trait InjectionPass {
    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef>;
}

impl InjectionPass for Program<Alphatized> {
    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        self.cst.inject(allocator)
    }
}

/// Like `Into`, but must produce a `ValueRef(T<_>)` and is provided with an `Allocator`.
trait Inject {
    /// The type to convert to.
    type Target: Into<ValueRef>;

    /// Perform the conversion.
    fn inject(self, allocator: &mut Allocator) -> Option<Self::Target>;
}

// Then we implement the conversion using the obvious, although tedious, structural recursion:

impl Inject for cst::Expr {
    type Target = ValueRef;

    fn inject(self, allocator: &mut Allocator) -> Option<ValueRef> {
        use pcws_syntax::cst::Expr::*;

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
            Lex(_, usage) =>
                Symbol::new(allocator, &usage.def.borrow().name)
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
        use pcws_syntax::cst::Pattern::*;

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

impl Inject for cst::Case {
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
