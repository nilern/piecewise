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
mod interpret;

use std::str::FromStr;
use std::io::{self, Read};

use pcws_domain::{Allocator, register_static_t};
use pcws_domain::values;
use pcws_syntax::cst::Expr;
use env::Env;
use inject::Inject;
use interpret::interpret;

fn main() {
    register_static_t::<values::Promise>();
    register_static_t::<values::Tuple>();
    register_static_t::<values::String>();
    register_static_t::<values::Symbol>();
    register_static_t::<ast::Function>();
    register_static_t::<ast::Block>();
    register_static_t::<ast::Match>();
    register_static_t::<ast::Case>();
    register_static_t::<ast::Def>();
    register_static_t::<ast::Call>();
    register_static_t::<ast::PrimCall>();
    register_static_t::<ast::Lex>();
    register_static_t::<ast::Dyn>();
    register_static_t::<ast::Const>();
    register_static_t::<Env>();

    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    match Expr::from_str(&src) {
        Ok(program) => {
            println!("{}", program);

            println!("\n---\n");

            let ast = program.inject(&mut *Allocator::instance()).unwrap(); // FIXME: unwrap

            // println!("{}", ast);
            //
            // println!("\n---\n");

            let value = interpret(ast).unwrap();
            println!("{}", value);
        },
        Err(err) => println!("ParseError: {:?}", err)
    }
}
