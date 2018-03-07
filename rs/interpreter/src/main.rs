#![feature(range_contains, try_from, nonzero, unique, const_atomic_isize_new)]

extern crate core;
extern crate pretty;

extern crate pcws_gc;
#[macro_use]
extern crate pcws_domain;
extern crate pcws_syntax;

mod ast;
mod binding;
mod inject;
mod patterns;
mod anf;
mod closures;
mod registers;

use std::io::{self, Read};
use std::str::FromStr;

use pcws_syntax::cst::{Program, Parsed, DefRef};
use binding::{AlphatizationPass, BindingReificationPass};
use patterns::PatternMatchingPass;
use registers::Reg;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    match Program::<Parsed>::from_str(&src) {
        Ok(program) => {
            println!("{}", program);

            println!("\n---\n");

            let program = program.alphatize();
            println!("{}", program);

            println!("\n---\n");

            let program = program.reify_bindings();
            println!("{}", program);

            println!("\n---\n");

            let program = program.expand_patterns();
            println!("{}", program);

            println!("\n---\n");

            let program: anf::Program<DefRef> = program.into();
            println!("{}", program);

            println!("\n---\n");

            let program = program.closure_convert();
            println!("{}", program);

            println!("\n---\n");

            let program: anf::Program<Reg> = program.into();
            println!("{}", program);
        },
        Err(err) => println!("ParseError: {}", err.0)
    }
}
