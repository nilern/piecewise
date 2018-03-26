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

use std::str::FromStr;
use std::io::{self, Read};

use pcws_domain::Allocator;
use pcws_domain::values::Type;
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

            let ast = {
                let heap = &mut *Allocator::instance_mut();
                Type::new::<ast::Function>(heap);
                Type::new::<ast::Block>(heap);
                Type::new::<ast::Match>(heap);
                Type::new::<ast::Case>(heap);
                Type::new::<ast::Def>(heap);
                Type::new::<ast::Call>(heap);
                Type::new::<ast::PrimCall>(heap);
                Type::new::<ast::Lex>(heap);
                Type::new::<ast::Dyn>(heap);
                Type::new::<ast::Const>(heap);
                Type::new::<continuation::Halt>(heap);
                Type::new::<continuation::CalleeCont>(heap);
                Type::new::<continuation::BlockCont>(heap);

                program.inject(heap).unwrap() // FIXME: unwrap
            };

            // println!("{}", ast);
            //
            // println!("\n---\n");

            println!("{}", interpret(ast).unwrap());
        },
        Err(err) => println!("ParseError: {:?}", err)
    }
}
