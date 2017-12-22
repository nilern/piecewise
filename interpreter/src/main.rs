#![feature(try_from, nonzero, unique, shared)]

extern crate core;
extern crate nix;
#[macro_use]
extern crate intrusive_collections;

#[macro_use]
extern crate lazy_static;

extern crate gc;
#[macro_use]
extern crate gc_derive;

use std::io::{self, Read};

// HACK: these are pub just for rustdoc
pub mod util;
pub mod gce;
pub mod object_model;
pub mod value;
pub mod ast;
pub mod lexer;
pub mod parser { include!(concat!(env!("OUT_DIR"), "/grammar.rs")); }
pub mod eval;

use parser::program;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    println!("{:?}", program(&src));
}
