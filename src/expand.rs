use ast;
use ast::{AST, App, Stmt, Var, NodeMapping};

struct ExpandStep;

impl NodeMapping for ExpandStep {
    fn map_app(&mut self, node: App) -> AST {
        match node {
            App { pos, op: box AST::Var(Var { pos: oppos, name: opname}), args } =>
                if opname.chars().next().unwrap() == '@' {
                    let mut it = args.into_iter();
                    let cond = it.next().unwrap(); // HACK
                    match &opname as &str {
                        "@if" => if let Some(AST::Block(ast::Block { stmts, .. })) = it.next() {
                            let mut stit = stmts.into_iter();
                            if let Some(Stmt::Expr(then)) = stit.next() {
                                if let Some(Stmt::Expr(els)) = stit.next() {
                                    return AST::new_if(pos, cond, then, els).accept(self)
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

impl AST {
    pub fn expand(self) -> AST {
        self.prewalk(ExpandStep)
    }
}
