#[derive(Debug)]
pub enum AST {
    App(Box<AST>, Box<AST>),   // foo bar
    Block(Vec<AST>, Vec<AST>), // {|foo| bar; baz}
    Def(Box<AST>, Box<AST>),   // foo = bar

    Tuple(Vec<AST>),           // (,)
    Array(Vec<AST>),           // [,]
    Set(Vec<AST>),             // {,}
    Map(Vec<(AST, AST)>),      // {:,}

    Symbol(String),            // foo
    Int(isize)                 // 123
}
