use std::collections::{HashSet, HashMap};

use pcws_syntax::cst::{PrimOp, Def, DefRef};
use anf::{Program, Function, Stmt, Expr, Triv};

// ================================================================================================

impl Program<DefRef> {
    pub fn closure_convert(self) -> Program<DefRef> {
        let mut fns = Vec::new();

        for (name, mut f) in self.fns {
            f.analyze_closures();
            f.convert_closures(name, &HashMap::new(), &mut fns);
        }

        Program { fns, entry: self.entry }
    }
}

// ================================================================================================

impl Function<DefRef> {
    fn analyze_closures(&mut self) -> HashSet<DefRef> {
        let &mut Function { pos: _, ref params, ref mut free_vars, ref mut stmts, ref mut expr } =
            self;

        let mut frees = stmts.iter_mut().rev().fold(expr.analyze_closures(), |frees, stmt|
            match *stmt {
                Stmt::Def(ref mut def, ref mut expr) => {
                    let mut frees = &frees | &expr.analyze_closures();
                    frees.remove(def);
                    frees
                },
                Stmt::Expr(ref mut expr) => &frees | &expr.analyze_closures()
            }
        );

        for param in params.iter() { frees.remove(param); }

        *free_vars = frees.clone();
        frees
    }
}

impl Expr<DefRef> {
    fn analyze_closures(&mut self) -> HashSet<DefRef> {
        use self::Expr::*;

        match *self {
            Function(ref mut f) => f.analyze_closures(),
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

impl Triv<DefRef> {
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

impl Function<DefRef> {
    fn convert_closures(self, name: DefRef, parent_env: &HashMap<DefRef, DefRef>,
                        fns: &mut Vec<(DefRef, Function<DefRef>)>) -> Expr<DefRef>
    {
        let Function { pos, params, free_vars, stmts, expr } = self;

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

        for stmt in stmts {
            stmt.convert_closures(&env, &mut prelude, fns);
        }
        let expr = expr.convert_closures(&env, fns);

        let f = Function {
            pos: pos.clone(),
            params,
            free_vars,
            stmts:prelude,
            expr
        };
        fns.push((name.clone(), f));

        let mut all_args = vec![name.into()];
        all_args.extend(args);
        // TODO: Grab fn ptr, wrap in a singleton Fn
        Expr::PrimCall(pos, PrimOp::Closure, all_args)
    }
}

impl Stmt<DefRef> {
    fn convert_closures(self, env: &HashMap<DefRef, DefRef>, stmts: &mut Vec<Stmt<DefRef>>,
                        fns: &mut Vec<(DefRef, Function<DefRef>)>)
    {
        match self {
            Stmt::Def(def, expr) =>
                stmts.push(Stmt::Def(def, expr.convert_closures(env, fns))),
            Stmt::Expr(expr) =>
                stmts.push(Stmt::Expr(expr.convert_closures(env, fns))),
        }
    }
}

impl Expr<DefRef> {
    fn convert_closures(self, env: &HashMap<DefRef, DefRef>,
                        fns: &mut Vec<(DefRef, Function<DefRef>)>) -> Expr<DefRef>
    {
        use self::Expr::*;

        match self {
            // TODO: Give better name to code object.
            Function(f) => f.convert_closures(Def::new("f"), env, fns),
            Triv(pos, self::Triv::Var(def)) => {
                let def = env.get(&def).map(Clone::clone).unwrap_or(def);
                Triv(pos, self::Triv::Var(def))
            },
            Call(..) | PrimCall(..) | Triv(_, self::Triv::Const(..)) => self
        }
    }
}
