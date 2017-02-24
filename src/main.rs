#![feature(box_patterns)]

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
pub mod vm;

use util::ProffError;
use lexer::Lexer;
use value::RawRef;
use vm::{VM, CodeObject};
use vm::Operand::*;
use vm::Instr::*;

// TODO: tail recursive version
// fact:
//     ilt  l1, c0    ; n < 2
//     br   1         ; jump over ret
//     ret  c1        ; return 1
//     isub 2, l1, c1 ; a = n - 1
//     mov  4, l0     ; f = fact
//     mov  0, l1     ; save n
//     mov  5, l2     ; arg[0] = a
//     call 4         ; b = fact (n - 1)
//     imul 2, l0, l1 ; c = n * fact (n - 1)
//     ret  l2        ; return c

// main:
//     fun  3, 0      ; f = new fact()
//     mov  4, c0     ; arg0 = 5
//     call 3

fn main() {
    let mut vm = VM::new(CodeObject {
        code: vec![
            Fun(3, 0),
            Mov(4, From::from(Const(0))),
            Call(3),
            Halt
        ],
        consts: vec![RawRef(5)],
        reg_req: 5,
        cobs: vec![
            CodeObject {
                code: vec![
                    ILt(From::from(Local(1)), From::from(Const(0))),
                    Br(1),
                    Ret(From::from(Const(1))),
                    ISub(2, From::from(Local(1)), From::from(Const(1))),
                    Mov(4, From::from(Local(0))),
                    Mov(0, From::from(Local(1))),
                    Mov(5, From::from(Local(2))),
                    Call(4),
                    IMul(2, From::from(Local(0)), From::from(Local(1))),
                    Ret(From::from(Local(2)))
                ],
                consts: vec![RawRef(2), RawRef(1)],
                reg_req: 6,
                cobs: vec![]
            }
        ]
    });
    vm.run();

    println!("\n{:?}\n", vm);

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
