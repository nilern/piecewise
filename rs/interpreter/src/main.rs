#![feature(nonzero, unique, const_atomic_isize_new)]

extern crate core;

extern crate pcws_gc;
#[macro_use]
extern crate pcws_domain;

mod ast;
mod parser { include!(concat!(env!("OUT_DIR"), "/grammar.rs")); }

use std::io::{self, Read};

use pcws_domain::{Allocator, DynamicDebug};

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    let mut allocator = Allocator::new(4*1024*1024);
    match parser::program(&src, &mut allocator) {
        Ok(ast) => println!("{:?}", ast.fmt_wrap(&allocator)),
        Err(err) => println!("ParseError: {}", err)
    }
}

// #![feature(try_from, nonzero, unique, shared)]
//
// extern crate core;
// extern crate nix;
// #[macro_use]
// extern crate intrusive_collections;
//
// use std::io::{self, Read};
//
// // HACK: these are pub just for rustdoc
// pub mod util;
// pub mod gce;
// pub mod domain;
// pub mod value;
// pub mod interpreter;
// pub mod ast;
// pub mod lexer;
// pub mod parser { include!(concat!(env!("OUT_DIR"), "/grammar.rs")); }
// pub mod continuations;
// pub mod env;
// pub mod eval;
//
// use parser::program;
// use interpreter::Interpreter;
//
// fn main() {
//     let mut src = String::new();
//     io::stdin().read_to_string(&mut src).unwrap();
//
//     let mut interpreter = Interpreter::new(4*1024*1024);
//     match interpreter.with_gc_retry(|allocator| Some(program(&src, allocator)) /* HACK */,
//                                     &mut []).unwrap() {
//         Ok(prog) => {
//             println!("{:#?}", prog.fmt_wrap(&mut interpreter));
//             match interpreter.run(prog) {
//                 Ok(value) => println!("{:#?}", value.fmt_wrap(&interpreter)),
//                 Err(err) => println!("{:#?}", err.fmt_wrap(&interpreter))
//             }
//         },
//         Err(err) => println!("{:#?}", err)
//     }
// }
