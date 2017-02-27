#![feature(box_patterns, try_from, associated_consts)]

extern crate rustyline;
extern crate lalrpop_util as __lalrpop_util;

use std::fs::File;
use std::io::Read;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub mod util;
pub mod gc;
pub mod value;
pub mod ast;
pub mod lexer;
pub mod parser;
pub mod expand;
pub mod bytecode;
pub mod vm;

use util::ProffError;
use lexer::Lexer;

fn main() {
    let mut args = std::env::args();
    match args.len() {
        1 => {
            let mut rl = Editor::<()>::new();
            loop {
                let readline = rl.readline("prf> ");
                match readline {
                    Ok(line) => {
                        rl.add_history_entry(&line);
                        // for tok in Lexer::new(&line).with_ws_stx() {
                        //     match tok {
                        //         Ok((s, tok, e)) => println!("#<{} @ {}-{}>", tok, s, e),
                        //         Err(err) => println!("Error: {:?}", err)
                        //     }
                        // }
                        // println!("");
                        let ast = parser::parse_Expr(Lexer::new(&line).with_ws_stx())
                            .map_err(ProffError::from)
                            .and_then(|ast| ast.expand().map_err(ProffError::from));
                        match ast {
                            Ok(ast) => println!("{}", ast),
                            Err(err) => println!("Error: {:?}", err)
                        }
                    },
                    Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
                    Err(err) => {
                        println!("Error: {:?}", err);
                        break;
                    }
                }
            }
        },
        2 => {
            let _ = args.next();
            let mut f = File::open(args.next().unwrap()).expect("unable to open file");
            let mut code = String::new();
            f.read_to_string(&mut code).expect("error reading from file");
            // for tok in Lexer::new(&code).with_ws_stx() {
            //     match tok {
            //         Ok((s, tok, e)) => println!("#<{} @ {}-{}>", tok, s, e),
            //         Err(err) => println!("Error: {:?}", err)
            //     }
            // }
            // println!("");
            match parser::parse_Exprs(Lexer::new(&code).with_ws_stx()).map_err(ProffError::from)
                        .and_then(|ast| ast.expand().map_err(ProffError::from)) {
                Ok(ast) => println!("{}", ast),
                Err(err) => println!("{:?}", err)
            }
        },
        _ => println!("Too many command line arguments.")
    }
}
