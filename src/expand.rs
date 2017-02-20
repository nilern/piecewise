use ast;
use ast::{AST, Block, Clause, Stmt};

pub trait Expand {
    fn expand(self) -> Self;
}

impl Expand for AST {
    fn expand(self) -> AST {
        use ast::AST::*;

        match self {
            Block(block) => Block(block.expand()),
            Fn { pos, clauses } => Fn {
                pos: pos,
                clauses: clauses.into_iter().map(Clause::expand).collect()
            },
            App { pos, op: box Var { pos: oppos, name: opname}, args } =>
                if opname.chars().next().unwrap() == '@' {
                    let mut it = args.into_iter();
                    let cond = it.next().unwrap(); // HACK
                    match &opname as &str {
                        "@if" => if let Some(Block(ast::Block { stmts, .. })) = it.next() {
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
                    App {
                        pos: pos,
                        op: Box::new(Var { pos: oppos, name: opname}),
                        args: args.into_iter().map(AST::expand).collect()
                    }
                },
            App { pos, op, args} => App {
                pos: pos,
                op: Box::new(op.expand()),
                args: args.into_iter().map(AST::expand).collect()
            },
            v @ Var { .. } => v,
            c @ Const { .. } => c
        }
    }
}

impl Expand for Block {
    fn expand(self) -> Block {
        Block {
            pos: self.pos,
            stmts: self.stmts.into_iter().map(Stmt::expand).collect()
        }
    }
}

impl Expand for Clause {
    fn expand(self) -> Clause {
        Clause {
            params: self.params,
            cond: self.cond.expand(),
            body: self.body.expand()
        }
    }
}

impl Expand for Stmt {
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
