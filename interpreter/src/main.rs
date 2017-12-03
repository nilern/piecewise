#![feature(try_from)]

extern crate gc;
#[macro_use]
extern crate gc_derive;

use std::io::{self, Read};

mod util;
mod value;
mod ast;
mod lexer;
mod parser;
mod eval;

use lexer::Lexer;
use parser::parse_Program;
use eval::run;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    let lexer = Lexer::new(&src);
    println!("{:?}", run(parse_Program(lexer).unwrap()));
}
