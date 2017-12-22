#![feature(try_from)]

extern crate gc;
#[macro_use]
extern crate gc_derive;

use std::io::{self, Read};

mod util;
mod value;
mod ast;
mod lexer;
mod parser { include!(concat!(env!("OUT_DIR"), "/grammar.rs")); }
mod eval;

use parser::program;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    println!("{:?}", program(&src));
}
