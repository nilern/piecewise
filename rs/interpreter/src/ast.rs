use std::iter;
use std::fmt::{self, Formatter};
use pretty::{self, DocAllocator, DocBuilder};

use pcws_domain::{DynamicDebug, DynamicDisplay, Allocator};
use pcws_domain::object_model::{RefTailed, ValueRef, ValueRefT};
use pcws_domain::values::Symbol;
use pcws_syntax::cst::PrimOp;

// ================================================================================================

pub trait Pretty {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>;
}

macro_rules! display_pretty {
    { $name:ident } => {
        impl DynamicDisplay for $name {
            fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
                let docs = pretty::Arena::new();
                <_ as Into<pretty::Doc<_>>>::into(self.pretty(types, &docs)).render_fmt(80, f)
            }
        }
    }
}

macro_rules! pretty_display {
    { $name:ident } => {
        impl Pretty for $name {
            fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
                -> DocBuilder<'a, A>
            {
                docs.as_string(self.display_wrap(types))
            }
        }
    }
}

impl Pretty for ValueRef {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        typecase!(self, types, {
            // FIXME
            _ => docs.as_string(self.display_wrap(types))
        })
    }
}

// ================================================================================================

/// Function AST node
heap_struct! {
    pub struct Function: RefTailed<TailItem=ValueRefT<Symbol>> {
        body: ValueRef
    }
}

impl Function {
    pub fn new(allocator: &mut Allocator, params: &[ValueRefT<Symbol>], body: ValueRef)
        -> Option<ValueRefT<Function>>
    {
        allocator.create_with_slice(|base| Function { base, body }, params)
    }

    pub fn params(&self) -> &[ValueRefT<Symbol>] { self.tail() }
}

impl DynamicDebug for Function {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Function")
         .field("base", &self.base.debug_wrap(types))
         .field("body", &self.body.debug_wrap(types))
         .field("params", &self.params().debug_wrap(types))
         .finish()
    }
}

display_pretty! { Function }

impl Pretty for Function {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        docs.text("(@fn (")
            .append(docs.intersperse(self.params().iter()
                                         .map(|param| docs.text(param.chars())),
                                     docs.text(", ")))
            .append(") ")
            .append(self.body.pretty(types, docs))
            .append(docs.text(")"))
    }
}

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
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Block")
         .field("base", &self.base.debug_wrap(types))
         .field("expr", &self.expr.debug_wrap(types))
         .field("stmts", &self.stmts().debug_wrap(types))
         .finish()
    }
}

display_pretty! { Block }

impl Pretty for Block {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        docs.text("{")
            .append(docs.newline()
                        .append(docs.intersperse(self.stmts().iter()
                                                     .map(|stmt| stmt.pretty(types, docs))
                                                     .collect::<Vec<_>>().into_iter() // HACK
                                                     .chain(iter::once(
                                                                self.expr.pretty(types, docs))),
                                                 docs.text(";").append(docs.newline())))
                        .nest(2))
            .append(docs.newline()).append(docs.text("}"))
    }
}

// ================================================================================================

heap_struct! {
    pub struct Match: RefTailed<TailItem=ValueRefT<Case>> {
        matchee: ValueRef,
        default: ValueRef
    }
}

impl Match {
    pub fn new(allocator: &mut Allocator, matchee: ValueRef, cases: &[ValueRefT<Case>],
               default: ValueRef) -> Option<ValueRefT<Match>>
    {
        allocator.create_with_slice(|base| Match { base, matchee, default }, cases)
    }

    fn cases(&self) -> &[ValueRefT<Case>] { self.tail() }
}

impl DynamicDebug for Match {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Match")
         .field("base", &self.base.debug_wrap(types))
         .field("matchee", &self.matchee.debug_wrap(types))
         .field("default", &self.default.debug_wrap(types))
         .field("cases", &self.cases().debug_wrap(types))
         .finish()
    }
}

display_pretty! { Match }

impl Pretty for Match {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        docs.text("@match ")
            .append(self.matchee.pretty(types, docs))
            .append(" {")
            .append(docs.newline()
                        .append(docs.intersperse(self.cases().iter()
                                                     .map(|case| case.pretty(types, docs)),
                                                 docs.text(";").append(docs.newline())))
                        .nest(2))
            .append(docs.text(";")).append(docs.newline())
            .append(self.default.pretty(types, docs))
            .append(docs.newline()).append(docs.text("}"))
    }
}

// ================================================================================================

/// Case AST (match sub)node
heap_struct! {
    pub struct Case: UniformHeapValue {
        pattern: ValueRef,
        guard: ValueRef,
        body: ValueRef
    }
}

impl Case {
    pub fn new(allocator: &mut Allocator, pattern: ValueRef, guard: ValueRef, body: ValueRef)
        -> Option<ValueRefT<Case>>
    {
        allocator.create_uniform(|base| Case { base, pattern, guard, body })
    }
}

impl DynamicDebug for Case {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Case")
         .field("base", &self.base.debug_wrap(types))
         .field("pattern", &self.pattern.debug_wrap(types))
         .field("guard", &self.guard.debug_wrap(types))
         .field("body", &self.body.debug_wrap(types))
         .finish()
    }
}

display_pretty! { Case }

impl Pretty for Case {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        self.pattern.pretty(types, docs)
                    .append(docs.text(" | "))
                    .append(self.guard.pretty(types, docs))
                    .append(docs.text(" => "))
                    .append(self.body.pretty(types, docs))
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

    pub fn callee(&self) -> ValueRef { self.callee }

    pub fn args(&self) -> &[ValueRef] { self.tail() }
}

impl DynamicDebug for Call {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Call")
         .field("base", &self.base.debug_wrap(types))
         .field("callee", &self.callee.debug_wrap(types))
         .field("args", &self.args().debug_wrap(types))
         .finish()
    }
}

display_pretty! { Call }

impl Pretty for Call {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        docs.text("(")
            .append(docs.intersperse(iter::once(self.callee.pretty(types, docs))
                                          .chain(self.args().iter()
                                                     .map(|arg| arg.pretty(types, docs))),
                                     docs.text(" ")))
            .append(docs.text(")"))
    }
}

// ================================================================================================

/// PrimCall AST node
heap_struct! {
    pub struct PrimCall: RefTailed<TailItem=ValueRef> {
        op: PrimOp
    }
}

impl PrimCall {
    pub fn new(allocator: &mut Allocator, op: PrimOp, args: &[ValueRef])
        -> Option<ValueRefT<PrimCall>>
    {
        allocator.create_with_slice(|base| PrimCall { base, op }, args)
    }

    pub fn args(&self) -> &[ValueRef] { self.tail() }
}

impl DynamicDebug for PrimCall {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Call")
         .field("base", &self.base.debug_wrap(types))
         .field("op", &self.op)
         .field("args", &self.args().debug_wrap(types))
         .finish()
    }
}

display_pretty! { PrimCall }

impl Pretty for PrimCall {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        docs.text("(")
            .append(docs.intersperse(iter::once(docs.as_string(self.op))
                                          .chain(self.args().iter()
                                                     .map(|arg| arg.pretty(types, docs))),
                                     docs.text(" ")))
            .append(docs.text(")"))
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
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Def")
         .field("base", &self.base.debug_wrap(types))
         .field("pattern", &self.pattern.debug_wrap(types))
         .field("expr", &self.expr.debug_wrap(types))
         .finish()
    }
}

display_pretty! { Def }

impl Pretty for Def {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, types: &mut Allocator, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        self.pattern.pretty(types, docs)
                    .append(docs.text(" = "))
                    .append(self.expr.pretty(types, docs))
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
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Lex")
         .field("base", &self.base.debug_wrap(types))
         .field("name", &self.name.debug_wrap(types))
         .finish()
    }
}

impl DynamicDisplay for Lex {
    fn fmt(&self, f: &mut Formatter, _: &mut Allocator) -> Result<(), fmt::Error> {
        write!(f, "{}", self.name.chars())
    }
}

pretty_display! { Lex }

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
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Dyn")
         .field("base", &self.base.debug_wrap(types))
         .field("name", &self.name.debug_wrap(types))
         .finish()
    }
}

impl DynamicDisplay for Dyn {
    fn fmt(&self, f: &mut Formatter, _: &mut Allocator) -> Result<(), fmt::Error> {
        write!(f, "${}", self.name.chars())
    }
}

pretty_display! { Dyn }

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

    pub fn value(&self) -> ValueRef { self.value }
}

impl DynamicDebug for Const {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        f.debug_struct("Const")
         .field("base", &self.base.debug_wrap(types))
         .field("value", &self.value.debug_wrap(types))
         .finish()
    }
}

impl DynamicDisplay for Const {
    fn fmt(&self, f: &mut Formatter, types: &mut Allocator) -> Result<(), fmt::Error> {
        <_ as DynamicDisplay>::fmt(&self.value, f, types)
    }
}

pretty_display! { Const }
