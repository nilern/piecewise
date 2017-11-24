extern crate gc;
#[macro_use]
extern crate gc_derive;

use std::io::{self, Read};

mod util;
mod value;
mod ast;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::parse_Program;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    let lexer = Lexer::new(&src);
    println!("{:?}", parse_Program(lexer));
}
