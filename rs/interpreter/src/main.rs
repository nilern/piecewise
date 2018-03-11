#![feature(try_from, nonzero, unique, const_atomic_isize_new)]

extern crate core;
extern crate pretty;

extern crate pcws_gc;
#[macro_use]
extern crate pcws_domain;
extern crate pcws_syntax;

mod ast;
mod inject;
mod env;
mod continuation;
mod interpret;

use std::io::{self, Read};
use std::str::FromStr;

use pcws_domain::{Allocator, DynamicDisplay};
use pcws_syntax::cst::Expr;
use inject::Inject;
use interpret::interpret;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    match Expr::from_str(&src) {
        Ok(program) => {
            println!("{}", program);

            println!("\n---\n");

            let mut allocator = Allocator::new(4*1024*1024);
            let ast = program.inject(&mut allocator).unwrap(); // FIXME: unwrap
            println!("{}", ast.display_wrap(&mut allocator));

            println!("\n---\n");

            println!("{}", interpret(&mut allocator, ast).unwrap().display_wrap(&mut allocator));
        },
        Err(err) => println!("ParseError: {}", err.0)
    }
}
