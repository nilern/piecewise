use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;

use pcws_syntax::cst::{self, Program, Parsed, Expr, Stmt, Pattern, Case, Def, Use, DefRef, PrimOp,
                       Pos, Positioned};

// ================================================================================================

type EnvBindings<Var> = HashMap<String, Var>;

#[derive(Debug)]
enum Env<Var> {
    TopLevel,
    Nested {
        bindings: EnvBindings<Var>,
        parent: Rc<Env<Var>>
    }
}

impl<V> Env<V> {
    fn push(parent: Rc<Env<V>>, bindings: EnvBindings<V>) -> Env<V> {
        Env::Nested { bindings, parent }
    }

    fn get(&self, name: &str) -> Option<&V> {
        match *self {
            Env::Nested { ref bindings, ref parent } =>
                bindings.get(name).or_else(|| parent.get(name)),
            Env::TopLevel => None
        }
    }
}

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
        let env = Rc::new(Env::TopLevel);
        self.cst.alphatize(&env);
        Program::new(self.cst)
    }
}

type AlphaBindings = EnvBindings<DefRef>;
type AlphaEnv = Env<DefRef>;

trait Alphatize {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>);
}

impl Alphatize for Expr {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        use self::Expr::*;

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
            PrimCall(_, _, ref mut args) => {
                for arg in args { arg.alphatize(env) }
            },

            // Update Use:
            Lex(_, ref mut usage) => {
                let def = env.get(&usage.def.borrow().name).unwrap(); // FIXME: unwrap
                *usage = Use::new(def.clone());
            },

            // These don't contain defs or uses at all:
            Dyn(..) | Const(..) => {}
        }
    }
}

impl Alphatize for Pattern {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        use self::Pattern::*;

        // Here we only need to do the expressions, i.e. only the Call callees.
        match *self {
            Call(_, ref mut callee, ref mut args) => {
                callee.alphatize(env);
                for arg in args { arg.alphatize(env) }
            },
            PrimCall(_, _, ref mut args) => {
                for arg in args { arg.alphatize(env) }
            },
            Lex(..) | Dyn(..) | Const(..) => {}
        }
    }
}

impl Alphatize for Stmt {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        match *self {
            Stmt::Def(ref mut pattern, ref mut expr) => {
                pattern.alphatize(env);
                expr.alphatize(env);
            },
            Stmt::Expr(ref mut expr) => expr.alphatize(env)
        }
    }
}

impl Alphatize for Case {
    fn alphatize(&mut self, env: &Rc<AlphaEnv>) {
        let &mut Case { ref mut patterns, ref mut commit, ref mut guard, ref mut body } =
            self;

        for pattern in patterns.iter_mut() { pattern.alphatize(&env) }

        let mut bindings = HashMap::new();
        for pattern in patterns.iter_mut() {
            pattern_definiends(pattern, &mut bindings);
        }
        let env = Rc::new(AlphaEnv::push(env.clone(), bindings));

        for stmt in commit { stmt.alphatize(&env) }
        guard.alphatize(&env);
        body.alphatize(&env);
    }
}

fn pattern_definiends(pattern: &mut Pattern, bindings: &mut AlphaBindings) {
    match *pattern {
        Pattern::Call(_, _, ref mut args) =>
            for arg in args.iter_mut() { pattern_definiends(arg, bindings) },
        Pattern::PrimCall(_, _, ref mut args) =>
            for arg in args.iter_mut() { pattern_definiends(arg, bindings) },

        Pattern::Lex(_, ref mut def) => {
            let new_def = Rc::new(RefCell::new(def.borrow().clone()));
            *def = new_def.clone();
            let name = new_def.borrow().name.clone();
            bindings.insert(name, new_def);
        },

        Pattern::Dyn(..) | Pattern::Const(..) => {}
    }
}

fn stmt_definiends(stmt: &mut Stmt, bindings: &mut AlphaBindings) {
    match *stmt {
        Stmt::Def(ref mut pattern, _) =>
            pattern_definiends(pattern, bindings),
        Stmt::Expr(_) => {}
    }
}

// ================================================================================================

pub enum BindingsReified {}

pub trait BindingReificationPass {
    fn reify_bindings(self) -> Program<BindingsReified>;
}

impl BindingReificationPass for Program<Alphatized> {
    fn reify_bindings(self) -> Program<BindingsReified> {
        Program::new(self.cst.reify_bindings(&None))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scoping { Linear, Recursive }

// HACK: This wouldn't need to be an Option if the toplevel block was treated specially (which is
//       unavoidable anyway for e.g. a proper REPL experience).
type DynEnv = Option<DefRef>;

struct DeclBuilder {
    start_pos: Pos,
    decls: Vec<Stmt>,
    denv_push_args: Vec<Expr>
}

impl DeclBuilder {
    fn new(start_pos: Pos, parent_denv: DynEnv) -> DeclBuilder {
        let denv_push_args = vec![if let Some(parent_def) = parent_denv {
            Expr::Lex(start_pos.clone(), Use::new(parent_def))
        } else {
            Expr::PrimCall(start_pos.clone(), PrimOp::DenvEmpty, Vec::new())
        }];

        DeclBuilder {
            start_pos,
            decls: Vec::new(),
            denv_push_args
        }
    }

    fn push_lex(&mut self, pos: Pos, def: DefRef) {
        self.decls.push(Stmt::Def(Pattern::Lex(pos.clone(), def),
                                  Expr::PrimCall(pos, PrimOp::Promise, Vec::new())));
    }

    fn push_dyn_base(&mut self, pos: Pos, name: String, def: DefRef) {
        // def = __promise
        self.decls.push(Stmt::Def(Pattern::Lex(pos.clone(), def.clone()),
                                      Expr::PrimCall(pos.clone(), PrimOp::Promise,
                                                     Vec::new())));

        // ... :name def ...
        self.denv_push_args.push(Expr::Const(pos.clone(), cst::Const::Symbol(name)));
        self.denv_push_args.push(Expr::Lex(pos, Use::new(def)))
    }

    fn push_dyn_lin(&mut self, pos: Pos, name: String, temp: DefRef) {
        let def = Def::new(name.clone());
        self.push_dyn_base(pos.clone(), name, def.clone());

        // __redirect def temp
        self.decls.push(Stmt::Expr(Expr::PrimCall(pos.clone(), PrimOp::Redirect, vec![
            Expr::Lex(pos.clone(), Use::new(def)),
            Expr::Lex(pos, Use::new(temp))
        ])));
    }

    fn push_dyn_rec(&mut self, pos: Pos, name: String) {
        let def = Def::new(name.clone());
        self.push_dyn_base(pos, name, def);
    }

    fn into_stmts(self, denv: DefRef) -> Vec<Stmt> {
        let mut decls = self.decls;
        decls.push(Stmt::Def(Pattern::Lex(self.start_pos.clone(), denv),
                             Expr::PrimCall(self.start_pos.clone(), PrimOp::Denv,
                                            self.denv_push_args)));
        decls
    }
}

trait ReifyBindings {
    // OPTIMIZE: &mut self instead
    fn reify_bindings(self, denv: &DynEnv) -> Self;
}

trait ReifyPatternBindings {
    // OPTIMIZE: &mut self instead
    fn reify_bindings(self, denv: &DynEnv, scoping: Scoping, changes: &mut Vec<(Pattern, DefRef)>)
        -> Self;
}

impl ReifyBindings for Expr {
    fn reify_bindings(self, denv: &DynEnv) -> Expr {
        use self::Expr::*;

        // __denvGet denv :name
        fn denv_get(pos: Pos, denv: DefRef, name: String) -> Expr {
            Expr::PrimCall(pos.clone(), PrimOp::DenvGet, vec![
                Expr::Lex(pos.clone(), Use::new(denv)),
                Expr::Const(pos, cst::Const::Symbol(name))
            ])
        }

        // __redirect dest temp
        fn redirection(pos: Pos, dest: Expr, temp: DefRef) -> Stmt {
            Stmt::Expr(Expr::PrimCall(pos.clone(), PrimOp::Redirect, vec![
                dest, Expr::Lex(pos, Use::new(temp))
            ]))
        }

        match self {
            Function(pos, mut params, body) => {
                let denv_def = Def::new("denv");
                let denv = Some(denv_def.clone());
                params.insert(0, denv_def);
                return Function(pos, params, Box::new(body.reify_bindings(&denv)));
            },

            Block(pos, old_stmts, expr) => {
                let mut decls = DeclBuilder::new(pos.clone(), denv.clone());
                let mut stmts = Vec::new();

                // The entire block is converted in its own scope:
                let denv = Some(Def::new("denv"));

                // Convert stmts and build declarations:
                for stmt in old_stmts {
                    let mut changes = Vec::new();

                    stmts.push( stmt.reify_bindings(&denv, Scoping::Recursive, &mut changes));

                    for (old_pat, temp) in changes {
                        match old_pat {
                            Pattern::Lex(pos, def) => {
                                decls.push_lex(pos.clone(), def.clone());
                                stmts.push(redirection(pos.clone(), Expr::Lex(pos, Use::new(def)),
                                                                    temp));
                            },
                            Pattern::Dyn(pos, name) => {
                                decls.push_dyn_rec(pos.clone(), name.clone());
                                stmts.push(redirection(pos.clone(),
                                                       denv_get(pos, denv.clone().unwrap(), name),
                                                       temp));
                            },
                            _ => unreachable!()
                        }
                    }
                }
                let mut decls = decls.into_stmts(denv.clone().unwrap());

                // Append the declarations and converted stmts, convert expr and build the result:
                decls.extend(stmts);
                return Block(pos, decls, Box::new(expr.reify_bindings(&denv)));
            },

            // OPTIMIZE: Map the subexprs in place:
            Match(pos, matchee, cases, default_case) =>
                return Match(pos, Box::new(matchee.reify_bindings(denv)),
                                  cases.into_iter()
                                       .map(|case| case.reify_bindings(denv))
                                       .collect(),
                                  Box::new(default_case.reify_bindings(denv))),
            Call(pos, callee, args) =>
                return Call(pos.clone(),
                            Box::new(callee.reify_bindings(denv)),
                            iter::once(Expr::Lex(pos, Use::new(denv.clone().unwrap())))
                                 .chain(args.into_iter().map(|arg| arg.reify_bindings(denv)))
                                 .collect()),
            PrimCall(pos, op, args) =>
                return PrimCall(pos, op, args.into_iter()
                                             .map(|arg| arg.reify_bindings(denv))
                                             .collect()),

            Dyn(pos, name) => return denv_get(pos, denv.clone().unwrap(), name),

            Lex(..) | Const(..) => {}
        }

        return self;
    }
}

impl ReifyBindings for Case {
    fn reify_bindings(self, parent_denv: &DynEnv) -> Case {
        let pos = self.pos().clone();
        let Case { patterns, commit, guard, body } = self;
        assert!(commit.is_empty());

        // Convert patterns in parent scope:
        let mut changes = Vec::new();
        let patterns =
            patterns.into_iter()
                    .map(|pat| pat.reify_bindings(&parent_denv, Scoping::Linear, &mut changes))
                    .collect();

        // Build commit declarations based on changes made to patterns:
        let denv = Some(Def::new("denv"));
        let mut decls = DeclBuilder::new(pos, parent_denv.clone());
        for (old_pat, temp) in changes {
            match old_pat {
                Pattern::Dyn(pos, name) => decls.push_dyn_lin(pos, name, temp),
                _ => unreachable!()
            }
        }
        let commit = decls.into_stmts(denv.clone().unwrap());

        // Convert guard and body in the scope of this case:
        Case {
            patterns,
            commit,
            guard: guard.reify_bindings(&denv),
            body: body.reify_bindings(&denv)
        }
    }
}

impl ReifyPatternBindings for Stmt {
    fn reify_bindings(self, denv: &DynEnv, scoping: Scoping, changes: &mut Vec<(Pattern, DefRef)>)
        -> Self
    {
        match self {
            Stmt::Def(pat, expr) => Stmt::Def(pat.reify_bindings(denv, scoping, changes),
                                              expr.reify_bindings(denv)),
            Stmt::Expr(expr) => Stmt::Expr(expr.reify_bindings(denv))
        }
    }
}

impl ReifyPatternBindings for Pattern {
    fn reify_bindings(self, denv: &DynEnv, scoping: Scoping, changes: &mut Vec<(Pattern, DefRef)>)
        -> Pattern
    {
        use self::Pattern::*;

        let (pos, temp) = match self {
            Call(pos, callee, args) =>
                return Call(pos, callee.reify_bindings(denv),
                                 args.into_iter()
                                     .map(|pat| pat.reify_bindings(denv, scoping, changes))
                                     .collect()),
            PrimCall(pos, op, args) =>
                return PrimCall(pos, op, args.into_iter()
                                             .map(|pat| pat.reify_bindings(denv, scoping, changes))
                                             .collect()),
            Lex(..) if scoping == Scoping::Linear => return self,
            Lex(ref pos, ref def) => (pos.clone(), Def::new(def.borrow().name.clone())),
            Dyn(ref pos, ref name) => (pos.clone(), Def::new(name.clone())),
            Const(..) => return self
        };

        changes.push((self, temp.clone()));
        Lex(pos, temp)
    }
}
