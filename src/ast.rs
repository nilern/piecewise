use std::fmt;
use std::fmt::Display;

#[derive(Debug)]
pub enum AST {
    App(Box<AST>, Vec<AST>),   // foo bar
    Block(Vec<AST>),          // {foo; bar}
    Def(Box<AST>, Box<AST>),   // foo = bar

    Tuple(Vec<AST>),           // (,)
    Array(Vec<AST>),           // [,]
    // Set(Vec<AST>),             // {,}
    // Map(Vec<(AST, AST)>),      // {:,}

    Symbol(String),            // foo
    Int(isize)                 // 123
}

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &AST::App(ref op, ref args) => {
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
            &AST::Block(ref stmts) => {
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
            &AST::Def(ref pat, ref val) => write!(f, "{} = {}", pat, val),
            &AST::Tuple(ref vals) => {
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
            &AST::Array(ref vals) => {
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
            &AST::Symbol(ref chars) => write!(f, "{}", chars),
            &AST::Int(i) => write!(f, "{}", i)
        }
    }
}
