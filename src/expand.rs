use ast;
use ast::{AST, App, Stmt, Var, VarRef, NodeMapping};

// TODO: Carry some useful information
/// An error to signal when the AST cannot be expanded.
#[derive(Debug)]
pub struct ExpansionError;

struct ExpandStep;

impl NodeMapping for ExpandStep {
    type Err = ExpansionError;

    fn map_app(&mut self, node: App) -> Result<AST, ExpansionError> {
        match node {
            App { pos,
                  op: box AST::Var(Var { pos: oppos, name: VarRef::Global(opname) }), args } => {
                let opstr = opname.to_string();
                if opstr.chars().next().unwrap() == '@' {
                    let mut it = args.into_iter();
                    let cond = it.next().unwrap(); // HACK
                    match &opstr as &str {
                        "@if" => if let Some(AST::Block(ast::Block { stmts, .. })) = it.next() {
                            let mut stit = stmts.into_iter();
                            if let Some(Stmt::Expr(then)) = stit.next() {
                                if let Some(Stmt::Expr(els)) = stit.next() {
                                    return AST::new_if(pos, cond, then, els).accept(self);
                                }
                            }
                            Err(ExpansionError)
                        } else {
                            Err(ExpansionError)
                        },
                        _ => unimplemented!()
                    }
                } else {
                    Ok(AST::App(App {
                        pos: pos,
                        op: Box::new(AST::Var(Var { pos: oppos, name: VarRef::Global(opname) })),
                        args: args.into_iter()
                                  .map(AST::expand)
                                  .collect::<Result<Vec<AST>, ExpansionError>>()?
                    }))
                }
            },
            App { pos, op, args} => Ok(AST::App(App {
                pos: pos,
                op: Box::new(op.expand()?),
                args: args.into_iter()
                          .map(AST::expand)
                          .collect::<Result<Vec<AST>, ExpansionError>>()?
            })),
        }
    }
}

impl AST {
    /// Recursively expand macros.
    pub fn expand(self) -> Result<AST, ExpansionError> {
        self.prewalk(ExpandStep)
    }
}
