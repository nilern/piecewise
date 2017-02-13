use std::fmt;
use std::fmt::Display;

#[derive(Debug)]
pub enum CST {
    Block(Vec<CST>),          // {foo; bar}
    Def(Box<CST>, Box<CST>),   // foo = bar
    Params(Box<CST>, Box<CST>), // foo bar => baz
    App(Box<CST>, Vec<CST>),   // foo bar

    Tuple(Vec<CST>),           // (,)
    Array(Vec<CST>),           // [,]
    // Set(Vec<CST>),             // {,}
    // Map(Vec<(CST, CST)>),      // {:,}

    Symbol(String),            // foo
    Int(isize)                 // 123
}

impl Display for CST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &CST::App(ref op, ref args) => {
                try!(write!(f, "({} ", op));
                let mut it = args.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for arg in it {
                    try!(write!(f, " {}", arg));
                }
                write!(f, ")")
            },
            &CST::Block(ref stmts) => {
                try!(write!(f, "{{"));
                let mut it = stmts.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for arg in it {
                    try!(write!(f, " {}", arg));
                }
                write!(f, "}}")
            },
            &CST::Def(ref pat, ref val) => write!(f, "{} = {}", pat, val),
            &CST::Params(ref pat, ref val) => write!(f, "{} => {}", pat, val),
            &CST::Tuple(ref vals) => {
                try!(write!(f, "("));
                let mut it = vals.iter();
                if let Some(arg) = it.next() {
                    try!(write!(f, "{}", arg));
                }
                for v in it {
                    try!(write!(f, ", {}", v));
                }
                write!(f, ")")
            },
            &CST::Array(ref vals) => {
                try!(write!(f, "["));
                let mut it = vals.iter();
                if let Some(v) = it.next() {
                    try!(write!(f, "{}", v));
                }
                for v in it {
                    try!(write!(f, ", {}", v));
                }
                write!(f, "]")
            },
            &CST::Symbol(ref chars) => write!(f, "{}", chars),
            &CST::Int(i) => write!(f, "{}", i)
        }
    }
}

// enum AST {
//     Block(Vec<AST>),
//     Fn(Vec<Clause>),
//     App(Box<AST>, Vec<AST>),
//
//     Var(String),
//     Const(isize) // TODO: Const(Value)
// }
//
// struct Clause {
//     params: Vec<AST>,
//     cond: AST,
//     body: AST
// }
