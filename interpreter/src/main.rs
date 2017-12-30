#![feature(try_from, nonzero, unique, shared)]

extern crate core;
extern crate nix;
#[macro_use]
extern crate intrusive_collections;

use std::io::{self, Read};

// HACK: these are pub just for rustdoc
pub mod util;
pub mod gce;
pub mod value;
pub mod value_refs;
//pub mod ast;
pub mod lexer;
pub mod parser { include!(concat!(env!("OUT_DIR"), "/grammar.rs")); }
pub mod eval;

use parser::program;
use value::DynamicDebug;
use eval::Interpreter;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    let mut interpreter = Interpreter::new(4*1024*1024);
    match program(&src, &mut interpreter.values) {
        Ok(prog) => {
            println!("{:#?}", prog.fmt_wrap(&interpreter.values));
            match interpreter.run(prog) {
                Ok(value) => println!("{:#?}", value.fmt_wrap(&interpreter.values)),
                Err(err) => println!("{:#?}", err.fmt_wrap(&interpreter.values))
            }
        },
        Err(err) => println!("{:#?}", err)
    }
}
