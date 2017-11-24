use gc::Gc;

#[derive(Debug, Trace, Finalize)]
pub enum Expr {
    Block(Block),
    Name(String)
}

#[derive(Debug, Trace, Finalize)]
pub enum Stmt {
    Expr(Expr)
}

#[derive(Debug, Trace, Finalize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Gc<Expr>
}
