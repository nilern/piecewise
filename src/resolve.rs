use std::collections::HashMap;
use std::rc::Rc;

use ast;
use ast::{AST, App, Var, VarRef, Block, Stmt, Clause, CtxMapping};

// TODO: Carry some useful information
/// An error to signal when resolution fails.
#[derive(Debug)]
pub struct ResolveError;

struct Env {
    bindings: HashMap<String, String>,
    parent: Option<Rc<Env>>
}

impl Env {
    fn new(parent: Option<Rc<Env>>, bindings: HashMap<String, String>) -> Env {
        Env {
            bindings: bindings,
            parent: parent
        }
    }

    fn resolve(&self, name: &str) -> VarRef {
        self.bindings.get(name)
                     .map(|name| VarRef::Local(name.clone()))
                     .or_else(||
                         self.parent.clone()
                                    .and_then(|parent| parent.resolve_str(name))
                                    .map(|name| VarRef::Clover(name.clone())))
                     .unwrap_or(VarRef::Global(name.to_string()))
    }

    fn resolve_str(&self, name: &str) -> Option<String> {
        self.bindings.get(name)
                     .map(Clone::clone)
                     .or_else(||
                         self.parent.clone().and_then(|parent| parent.resolve_str(name)))
    }
}

struct Resolve {
    counter: usize
}

impl Resolve {
    fn new() -> Resolve {
        Resolve { counter: 0 }
    }

    fn rename(&mut self, name: &str) -> String {
        let res = format!("{}{}", name, self.counter);
        self.counter += 1;
        res
    }

    fn block_bindings<'a, I>(&mut self, bindings: &mut HashMap<String, String>, stmts: I)
        where I: Iterator<Item=&'a Stmt>
    {
        for stmt in stmts {
            match stmt {
                &Stmt::Def { ref name, .. } => {
                    bindings.insert(name.clone(), self.rename(name));
                },
                &Stmt::Expr(..) => ()
            }
        }
    }

    fn param_bindings<'a>(&mut self, bindings: &mut HashMap<String, String>, params: &str) {
        bindings.insert(params.to_string(), self.rename(params));
    }
}

impl CtxMapping for Resolve {
    type Ctx = Option<Rc<Env>>;
    type Err = ResolveError;

    fn map_block(&mut self, node: Block, env: Option<Rc<Env>>) -> Result<AST, Self::Err> {
        let mut bindings = HashMap::new();
        self.block_bindings(&mut bindings, node.stmts.iter());
        let env = Some(Rc::new(Env::new(env, bindings)));
        Ok(AST::Block(Block {
            pos: node.pos,
            stmts: node.stmts.into_iter()
                             .map(|stmt| self.map_stmt(stmt, env.clone()))
                             .collect::<Result<Vec<Stmt>, Self::Err>>()?
        }))
    }

    fn map_fn(&mut self, node: ast::Fn, env: Option<Rc<Env>>) -> Result<AST, Self::Err> {
        Ok(AST::Fn(ast::Fn {
            pos: node.pos,
            clauses: node.clauses.into_iter()
                                 .map(|clause| self.map_clause(clause, env.clone()))
                                 .collect::<Result<Vec<Clause>, Self::Err>>()?
        }))
    }

    fn map_app(&mut self, node: App, env: Option<Rc<Env>>) -> Result<AST, Self::Err> {
        Ok(AST::App(App {
            pos: node.pos,
            op: Box::new(node.op.accept_ctx(self, env.clone())?),
            args: node.args.into_iter()
                           .map(|arg| arg.accept_ctx(self, env.clone()))
                           .collect::<Result<Vec<AST>, Self::Err>>()?
        }))
    }

    fn map_var(&mut self, node: Var, env: Option<Rc<Env>>) -> Result<AST, Self::Err> {
        Ok(env.map(|env| AST::Var(Var { pos: node.pos, name: env.resolve(&node.name()) }))
              .unwrap_or(AST::Var(node)))
    }

    fn map_stmt(&mut self, node: Stmt, env: Option<Rc<Env>>) -> Result<Stmt, Self::Err> {
        match node {
            Stmt::Def { name, val } => {
                Ok(Stmt::Def {
                    name: env.clone().and_then(|env| env.resolve_str(&name)).unwrap_or(name),
                    val: val.accept_ctx(self, env.clone())?
                })
            },
            Stmt::Expr(e) => Ok(Stmt::Expr(e.accept_ctx(self, env)?))
         }
    }

    fn map_clause(&mut self, node: Clause, env: Option<Rc<Env>>) -> Result<Clause, Self::Err> {
        let mut param_bindings = HashMap::new();
        self.param_bindings(&mut param_bindings, &node.params);
        let param_env = Some(Rc::new(Env::new(env.clone(), param_bindings.clone())));

        let mut bindings = param_bindings;
        self.block_bindings(&mut bindings, node.body.iter());
        let env = Some(Rc::new(Env::new(env.clone(), bindings)));

        Ok(Clause {
            pos: node.pos,
            params: param_env.clone()
                             .and_then(|env| env.resolve_str(&node.params))
                             .unwrap(), // we just put it there with param_bindings
            cond: node.cond.accept_ctx(self, param_env)?,
            body: node.body.into_iter()
                           .map(|stmt| self.map_stmt(stmt, env.clone()))
                           .collect::<Result<Vec<Stmt>, Self::Err>>()?
        })
    }
}

impl AST {
    /// Resolve variables to be local, closed over or global and alphatize their names.
    pub fn resolve(self) -> Result<AST, ResolveError> {
        self.accept_ctx(&mut Resolve::new(), None)
    }
}
