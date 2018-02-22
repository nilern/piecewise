#![feature(try_from, nonzero, unique, const_atomic_isize_new)]

extern crate core;
extern crate combine;

extern crate pcws_gc;
#[macro_use]
extern crate pcws_domain;
extern crate pcws_syntax;

mod ast;
mod frontend;

use std::io::{self, Read};
use std::cell::RefCell;
use combine::Parser;

use pcws_domain::{Allocator, DynamicDebug};
use pcws_syntax::cst::{Program, IdFactory};
use pcws_syntax::lexer::Lexer;
use pcws_syntax::parser::program;
use frontend::{Parsed, AlphatizationPass, InjectionPass};

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    for tok_res in Lexer::new(&src) {
        match tok_res {
            Ok(token) => print!("'{}', ", token),
            Err(_) => {
                println!("{:?}", tok_res);
                break;
            }
        }
    }

    let id_factory = RefCell::new(IdFactory::new());

    match program(&id_factory).parse(Lexer::new(&src)) {
        Ok((cst, _)) => {
            let program: Program<Parsed> = Program::new(cst, id_factory.into_inner().build());
            println!("{:?}", program);

            let program = program.alphatize();
            println!("{:?}", program);

            let mut allocator = Allocator::new(4*1024*1024);
            let ast = program.inject(&mut allocator).unwrap(); // FIXME: unwrap
            println!("{:?}", ast.fmt_wrap(&allocator));
        },
        Err(err) => println!("ParseError: {}", err)
    }
}
