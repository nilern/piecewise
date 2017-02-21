use ast;
use ast::{AST, Block, App, Clause, Stmt, Var, Const};

pub trait Expand {
    type Ret;

    fn expand(self) -> Self::Ret;
}

impl Expand for AST {
    type Ret = AST;

    fn expand(self) -> AST {
        use ast::AST::*;

        match self {
            Block(block) => block.expand(),
            Fn(f) => f.expand(),
            App(app) => app.expand(),
            Var(v) => v.expand(),
            Const(c) => c.expand()
        }
    }
}

impl Expand for Block {
    type Ret = AST;

    fn expand(self) -> AST {
        AST::Block(Block {
            pos: self.pos,
            stmts: self.stmts.into_iter().map(Stmt::expand).collect()
        })
    }
}

impl Expand for ast::Fn {
    type Ret = AST;

    fn expand(self) -> AST {
        AST::Fn(ast::Fn {
            pos: self.pos,
            clauses: self.clauses.into_iter().map(Clause::expand).collect()
        })
    }
}

impl Expand for App {
    type Ret = AST;

    fn expand(self) -> AST {
        match self {
            App { pos, op: box AST::Var(Var { pos: oppos, name: opname}), args } =>
                if opname.chars().next().unwrap() == '@' {
                    let mut it = args.into_iter();
                    let cond = it.next().unwrap(); // HACK
                    match &opname as &str {
                        "@if" => if let Some(AST::Block(ast::Block { stmts, .. })) = it.next() {
                            let mut stit = stmts.into_iter();
                            if let Some(Stmt::Expr(then)) = stit.next() {
                                if let Some(Stmt::Expr(els)) = stit.next() {
                                    return AST::new_if(pos, cond, then, els).expand();
                                }
                            }

                            panic!() // HACK
                        } else {
                            panic!() // HACK
                        },
                        _ => unimplemented!()
                    }
                } else {
                    AST::App(App {
                        pos: pos,
                        op: Box::new(AST::Var(Var { pos: oppos, name: opname})),
                        args: args.into_iter().map(AST::expand).collect()
                    })
                },
            App { pos, op, args} => AST::App(App {
                pos: pos,
                op: Box::new(op.expand()),
                args: args.into_iter().map(AST::expand).collect()
            }),
        }
    }
}

impl Expand for Var {
    type Ret = AST;

    fn expand(self) -> AST {
        AST::Var(self)
    }
}

impl Expand for Const {
    type Ret = AST;

    fn expand(self) -> AST {
        AST::Const(self)
    }
}

impl Expand for Clause {
    type Ret = Clause;

    fn expand(self) -> Clause {
        Clause {
            pos: self.pos,
            params: self.params,
            cond: self.cond.expand(),
            body: self.body.into_iter().map(Stmt::expand).collect()
        }
    }
}

impl Expand for Stmt {
    type Ret = Stmt;

    fn expand(self) -> Stmt {
        use ast::Stmt::*;

        match self {
            Def { name, val } => Def {
                name: name,
                val: val.expand()
            },
            Expr(expr) => Expr(expr.expand())
        }
    }
}
