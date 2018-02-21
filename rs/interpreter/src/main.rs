#![feature(nonzero, unique, const_atomic_isize_new)]

extern crate core;

extern crate pcws_gc;
#[macro_use]
extern crate pcws_domain;
#[macro_use]
extern crate combine;

mod cst;
mod lexer;
mod ast;
mod frontend;

use std::io::{self, Read};

use lexer::Lexer;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    for tok in Lexer::new(&src) {
        println!("{:?}", tok);
    }

    /* let mut allocator = Allocator::new(4*1024*1024);
    let mut id_factory = IdFactory::new();

    match parser::program(&src, &mut id_factory) {
        Ok(cst) => {
            let program: Program<Parsed> = Program::new(cst, id_factory.build());
            println!("{:?}", program);

            let program = program.alphatize();
            println!("{:?}", program);

            let ast = program.inject(&mut allocator) .unwrap(); // FIXME: unwrap
            println!("{:?}", ast.fmt_wrap(&allocator));
        },
        Err(err) => println!("ParseError: {}", err)
    } */
}
