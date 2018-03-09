use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{self, Debug, Formatter};

use pcws_domain::Allocator;
use pcws_domain::object_model::ValueRef;

// ================================================================================================

#[derive(Debug)]
pub enum VMError {}

type VMResult = Result<ValueRef, VMError>;

// ================================================================================================

#[derive(Clone)]
pub struct CodeObject {

}

impl Debug for CodeObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        unimplemented!()
    }
}

// ================================================================================================

pub struct VM {
    allocator: Rc<RefCell<Allocator>>,
    code: Vec<CodeObject>,

    cob: CodeObject,
    pc: usize,

    stack: Vec<ValueRef>,
    fp: usize
}

impl VM {
    pub fn new(allocator: Rc<RefCell<Allocator>>, code: Vec<CodeObject>, entry: usize) -> VM {
        let cob = code[entry].clone();
        VM { allocator, code, cob, pc: 0, stack: Vec::new(), fp: 0 }
    }

    pub fn run(&mut self) -> VMResult {
        unimplemented!()
    }
}
