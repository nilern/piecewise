use std::fmt::{self, Display, Formatter};
use std::collections::{HashSet, HashMap};
use pretty::{self, Doc, DocAllocator, DocBuilder};

use pcws_syntax::cst::{PrimOp, Def, DefRef, Positioned};
use anf::{Block, Function, Stmt, Expr, Triv};

// ================================================================================================

pub struct FirstOrderProgram {
    pub fns: HashMap<DefRef, Function>,
    pub entry: DefRef
}

impl Display for FirstOrderProgram {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let allocator = pretty::Arena::new();
        let doc = allocator.intersperse(
            self.fns.iter().map(|(k, v)| allocator.as_string(k.borrow())
                                                  .append(" = ")
                                                  .append(v.pretty(&allocator))),
            allocator.newline());
        <DocBuilder<_> as Into<Doc<_>>>::into(doc).render_fmt(80, f)
    }
}

// ================================================================================================

impl Block {
    pub fn closure_convert(mut self) -> FirstOrderProgram {
        self.analyze_closures();

        let mut fns = HashMap::new();
        let body = self.convert_closures(&HashMap::new(), Vec::new(), &mut fns);
        let entry = Def::new("start");
        fns.insert(entry.clone(), Function {
            pos: body.pos().clone(),
            params: Vec::new(),
            free_vars: HashSet::new(),
            body: Box::new(body)
        });
        FirstOrderProgram { fns, entry }
    }
}

// ================================================================================================

impl Block {
    fn analyze_closures(&mut self) -> HashSet<DefRef> {
        let &mut Block { ref mut stmts, ref mut expr } = self;
        stmts.iter_mut().rev().fold(expr.analyze_closures(), |frees, stmt|
            match *stmt {
                Stmt::Def(ref mut def, ref mut expr) => {
                    let mut frees = &frees | &expr.analyze_closures();
                    frees.remove(def);
                    frees
                },
                Stmt::Expr(ref mut expr) => &frees | &expr.analyze_closures()
            }
        )
    }
}

impl Expr {
    fn analyze_closures(&mut self) -> HashSet<DefRef> {
        use self::Expr::*;

        match *self {
            Function(self::Function { pos: _, ref params, ref mut free_vars, ref mut body }) => {
                let mut frees = body.analyze_closures();
                for param in params.iter() { frees.remove(param); }
                *free_vars = frees.clone();
                frees
            },
            Call(_, ref mut callee, ref mut args) =>
                args.iter_mut()
                    .fold(callee.analyze_closures(), |fvs, arg| &fvs | &arg.analyze_closures()),
            PrimCall(_, _, ref mut args) =>
                args.iter_mut()
                    .fold(HashSet::new(), |fvs, arg| &fvs | &arg.analyze_closures()),
            Triv(_, ref t) => t.analyze_closures()
        }
    }
}

impl Triv {
    fn analyze_closures(&self) -> HashSet<DefRef> {
        match *self {
            Triv::Var(ref def) => {
                let mut frees = HashSet::new();
                frees.insert(def.clone());
                frees
            },
            Triv::Const(_) => HashSet::new()
        }
    }
}

// ================================================================================================

impl Block {
    fn convert_closures(self, env: &HashMap<DefRef, DefRef>, mut stmts: Vec<Stmt>,
                        fns: &mut HashMap<DefRef, Function>) -> Block
    {
        let Block { stmts: old_stmts, expr } = self;

        for stmt in old_stmts {
            stmt.convert_closures(env, &mut stmts, fns);
        }
        let expr = expr.convert_closures(env, &mut stmts, fns);
        Block { stmts, expr }
    }
}

impl Function {
    fn convert_closures(self, parent_env: &HashMap<DefRef, DefRef>,
                        fns: &mut HashMap<DefRef, Function>) -> Expr
    {
        let Function { pos, params, free_vars, body } = self;

        let mut prelude = Vec::new();
        let mut env = HashMap::new();
        let mut args = Vec::new();

        for var in free_vars.iter() {
            let new_var = Def::new(var.borrow().name.clone());
            prelude.push(Stmt::Def(new_var.clone(),
                                   Expr::PrimCall(pos.clone(), PrimOp::ClosureGet, vec![
                                       // TODO
                                   ])));
            env.insert(var.clone(), new_var);
            args.push(Triv::Var(parent_env.get(var).map(Clone::clone).unwrap_or(var.clone())));
        }

        let name = Def::new("f");

        let f = Function {
            pos: pos.clone(),
            params,
            free_vars,
            body: Box::new(body.convert_closures(&env, prelude, fns))
        };
        fns.insert(name.clone(), f);

        let mut all_args = vec![name.into()];
        all_args.extend(args);
        // TODO: Grab fn ptr, wrap in a singleton Fn
        Expr::PrimCall(pos, PrimOp::Closure, all_args)
    }
}

impl Expr {
    fn convert_closures(self, env: &HashMap<DefRef, DefRef>, stmts: &mut Vec<Stmt>,
                        fns: &mut HashMap<DefRef, Function>) -> Expr
    {
        use self::Expr::*;

        match self {
            Function(f) => f.convert_closures(env, fns),
            Triv(pos, self::Triv::Var(def)) => {
                let def = env.get(&def).map(Clone::clone).unwrap_or(def);
                Triv(pos, self::Triv::Var(def))
            },
            Call(..) | PrimCall(..) | Triv(_, self::Triv::Const(..)) => self
        }
    }
}

impl Stmt {
    fn convert_closures(self, env: &HashMap<DefRef, DefRef>, stmts: &mut Vec<Stmt>,
                        fns: &mut HashMap<DefRef, Function>)
    {
        match self {
            Stmt::Def(def, expr) => {
                let expr = expr.convert_closures(env, stmts, fns);
                stmts.push(Stmt::Def(def, expr));
            },
            Stmt::Expr(expr) => {
                let expr = expr.convert_closures(env, stmts, fns);
                stmts.push(Stmt::Expr(expr));
            }
        }
    }
}
