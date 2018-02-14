#![feature(nonzero, unique, const_atomic_isize_new)]

extern crate core;

extern crate pcws_gc;
#[macro_use]
extern crate pcws_domain;

mod cst;
mod parser { include!(concat!(env!("OUT_DIR"), "/grammar.rs")); }
mod ast;
mod frontend;

use std::io::{self, Read};

use pcws_domain::{Allocator, DynamicDebug};
use cst::IdFactory;
use frontend::Inject;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    let mut allocator = Allocator::new(4*1024*1024);
    let mut id_factory = IdFactory::new();

    match parser::program(&src, &mut id_factory) {
        Ok(cst) => {
            let ids = id_factory.build();
            println!("{:?}\nwhere {:?}", cst, ids);

            let ast = cst.inject(&ids, &mut allocator).unwrap(); // FIXME: unwrap
            println!("{:?}", ast.fmt_wrap(&allocator));
        },
        Err(err) => println!("ParseError: {}", err)
    }
}
