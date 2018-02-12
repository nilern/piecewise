use std::fmt::{self, Formatter};

use pcws_domain::{DynamicDebug, Allocator};
use pcws_domain::object_model::{RefTailed, ValueRef, ValueRefT};
use pcws_domain::values::Symbol;

// ================================================================================================

/// Block AST node
heap_struct! {
    pub struct Block: RefTailed<TailItem=ValueRef> {
        expr: ValueRef
    }
}

impl Block {
    pub fn new(allocator: &mut Allocator, stmts: &[ValueRef], expr: ValueRef)
        -> Option<ValueRefT<Block>>
    {
        allocator.create_with_slice(|base| Block { base, expr }, stmts)
    }

    pub fn stmts(&self) -> &[ValueRef] { self.tail() }
}

impl DynamicDebug for Block {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Block")
         .field("base", &self.base.fmt_wrap(types))
         .field("expr", &self.expr.fmt_wrap(types))
         .field("stmts", &self.stmts().fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// AST node for definition statements.
heap_struct! {
    pub struct Def: UniformHeapValue {
        pattern: ValueRef,
        expr: ValueRef
    }
}

impl Def {
    pub fn new(allocator: &mut Allocator, pattern: ValueRef, expr: ValueRef)
        -> Option<ValueRefT<Def>>
    {
        allocator.create_uniform(|base| Def { base, pattern, expr })
    }
}

impl DynamicDebug for Def {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Def")
         .field("base", &self.base.fmt_wrap(types))
         .field("pattern", &self.pattern.fmt_wrap(types))
         .field("expr", &self.expr.fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// Function AST node
heap_struct! {
    pub struct Function: RefTailed<TailItem=ValueRefT<Method>> {}
}

impl Function {
    pub fn new(allocator: &mut Allocator, methods: &[ValueRefT<Method>])
        -> Option<ValueRefT<Function>>
    {
        allocator.create_with_slice(|base| Function { base }, methods)
    }

    pub fn methods(&self) -> &[ValueRefT<Method>] { self.tail() }
}

impl DynamicDebug for Function {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Function")
         .field("base", &self.base.fmt_wrap(types))
         .field("methods", &self.methods().fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// Method AST (function sub)node
heap_struct! {
    pub struct Method: UniformHeapValue {
        pattern: ValueRef,
        guard: ValueRef,
        body: ValueRef
    }
}

impl Method {
    pub fn new(allocator: &mut Allocator, pattern: ValueRef, guard: ValueRef, body: ValueRef)
        -> Option<ValueRefT<Method>>
    {
        allocator.create_uniform(|base| Method { base, pattern, guard, body })
    }
}

impl DynamicDebug for Method {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Method")
         .field("base", &self.base.fmt_wrap(types))
         .field("pattern", &self.pattern.fmt_wrap(types))
         .field("guard", &self.guard.fmt_wrap(types))
         .field("body", &self.body.fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// Call AST node
heap_struct! {
    pub struct Call: RefTailed<TailItem=ValueRef> {
        callee: ValueRef
    }
}

impl Call {
    pub fn new(allocator: &mut Allocator, callee: ValueRef, args: &[ValueRef])
        -> Option<ValueRefT<Call>>
    {
        allocator.create_with_slice(|base| Call { base, callee }, args)
    }

    pub fn args(&self) -> &[ValueRef] { self.tail() }
}

impl DynamicDebug for Call {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Call")
         .field("base", &self.base.fmt_wrap(types))
         .field("callee", &self.callee.fmt_wrap(types))
         .field("args", &self.args().fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// AST node for lexical variable names.
heap_struct! {
    pub struct Lex: UniformHeapValue {
        name: ValueRefT<Symbol>
    }
}

impl Lex {
    pub fn new(allocator: &mut Allocator, name: ValueRefT<Symbol>) -> Option<ValueRefT<Lex>> {
        allocator.create_uniform(|base| Lex { base, name })
    }
}

impl DynamicDebug for Lex {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Lex")
         .field("base", &self.base.fmt_wrap(types))
         .field("name", &self.name.fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// AST node for dynamic variable names.
heap_struct! {
    pub struct Dyn: UniformHeapValue {
        name: ValueRefT<Symbol>
    }
}

impl Dyn {
    pub fn new(allocator: &mut Allocator, name: ValueRefT<Symbol>) -> Option<ValueRefT<Dyn>> {
        allocator.create_uniform(|base| Dyn { base, name })
    }
}

impl DynamicDebug for Dyn {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Dyn")
         .field("base", &self.base.fmt_wrap(types))
         .field("name", &self.name.fmt_wrap(types))
         .finish()
    }
}

// ================================================================================================

/// AST node for constants.
heap_struct! {
    pub struct Const: UniformHeapValue {
        value: ValueRef
    }
}

impl Const {
    pub fn new(allocator: &mut Allocator, value: ValueRef) -> Option<ValueRefT<Const>> {
        allocator.create_uniform(|base| Const { base, value })
    }
}

impl DynamicDebug for Const {
    fn fmt(&self, f: &mut Formatter, types: &Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Const")
         .field("base", &self.base.fmt_wrap(types))
         .field("value", &self.value.fmt_wrap(types))
         .finish()
    }
}
