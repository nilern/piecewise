use gc::Gc;

pub type ValueRef = Gc<Value>;

#[derive(Debug, Trace, Finalize)]
pub enum Value {
    Function { methods: Vec<ValueRef> },
    Method {
        pattern: ValueRef,
        guard: ValueRef,
        body: ValueRef
    },
    Block {
        stmts: Vec<ValueRef>,
        expr: ValueRef
    },
    Call {
        callee: ValueRef,
        args: Vec<ValueRef>
    },
    Lex(ValueRef),
    Const(ValueRef),

    Symbol(String),

    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
}
