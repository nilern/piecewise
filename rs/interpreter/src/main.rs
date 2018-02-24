#![feature(try_from, nonzero, unique, const_atomic_isize_new)]

extern crate core;

extern crate pcws_gc;
#[macro_use]
extern crate pcws_domain;
extern crate pcws_syntax;

mod ast;
mod frontend;

use std::io::{self, Read};
use std::str::FromStr;

use pcws_domain::{Allocator, DynamicDebug};
use pcws_syntax::cst::{Program, Parsed};
use frontend::{AlphatizationPass, InjectionPass};

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    match Program::<Parsed>::from_str(&src) {
        Ok(program) => {
            println!("{}", program);

            let program = program.alphatize();
            println!("{}", program);

            let mut allocator = Allocator::new(4*1024*1024);
            let ast = program.inject(&mut allocator).unwrap(); // FIXME: unwrap
            println!("{:?}", ast.fmt_wrap(&allocator));
        },
        Err(err) => println!("ParseError: {}", err.0)
    }
}
