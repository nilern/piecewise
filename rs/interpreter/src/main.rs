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
use std::fmt::{self, Formatter};
use std::str::FromStr;
use std::collections::HashMap;
use std::any::TypeId;

use pcws_gc::GSize;
use pcws_domain::{Allocator, DynamicDebug, DynamicDisplay, UnsafeFmtFn};
use pcws_domain::object_model::{HeapValue, HeapValueSub};
use pcws_domain::values;
use pcws_syntax::cst::Expr;
use inject::Inject;
use continuation::{Halt, CalleeCont};
use interpret::interpret;

fn main() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    match Expr::from_str(&src) {
        Ok(program) => {
            println!("{}", program);

            println!("\n---\n");

            let mut allocator = Allocator::new(4*1024*1024, type_basis!(
                values::Type, values::Promise, values::Tuple, values::String, values::Symbol,
                ast::Function, ast::Block, ast::Match, ast::Case, ast::Def,
                ast::Call, ast::PrimCall,
                ast::Lex, ast::Dyn, ast::Const,
                Halt, CalleeCont
            ));
            let ast = program.inject(&mut allocator).unwrap(); // FIXME: unwrap
            println!("{}", ast.display_wrap(&mut allocator));

            println!("\n---\n");

            println!("{}", interpret(&mut allocator, ast).unwrap().display_wrap(&mut allocator));
        },
        Err(err) => println!("ParseError: {}", err.0)
    }
}
