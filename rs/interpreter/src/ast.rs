use std::iter;
use std::fmt::{self, Debug, Display, Formatter};
use pretty::{self, DocAllocator, DocBuilder};

use pcws_domain::Allocator;
use pcws_domain::object_model::{RefTailed, ValueRef, ValueRefT};
use pcws_domain::values::{Symbol, Tuple};
use pcws_syntax::cst::PrimOp;

// ================================================================================================

pub trait Pretty {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A>;
}

macro_rules! display_pretty {
    { $name:ident } => {
        impl Display for $name {
            fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
                let docs = pretty::Arena::new();
                <_ as Into<pretty::Doc<_>>>::into(self.pretty(&docs)).render_fmt(80, f)
            }
        }
    }
}

macro_rules! pretty_display {
    { $name:ident } => {
        impl Pretty for $name {
            fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A> {
                docs.as_string(self)
            }
        }
    }
}

impl Pretty for ValueRef {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        typecase!(self, {
            // FIXME
            _ => docs.as_string(self)
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

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Function")
         .field("base", &self.base)
         .field("body", &self.body)
         .field("params", &self.params())
         .finish()
    }
}

display_pretty! { Function }

impl Pretty for Function {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A> {
        docs.text("(@fn (")
            .append(docs.intersperse(self.params().iter()
                                         .map(|param| docs.text(param.chars())),
                                     docs.text(", ")))
            .append(") ")
            .append(self.body.pretty(docs))
            .append(docs.text(")"))
    }
}

// ================================================================================================

/// Block AST node
heap_struct! {
    pub struct Block: RefTailed<TailItem=ValueRef> {
        lex_binders: ValueRefT<Tuple>,
        dyn_binders: ValueRefT<Tuple>,
        expr: ValueRef
    }
}

impl Block {
    pub fn new(allocator: &mut Allocator,
               lex_binders: ValueRefT<Tuple>, dyn_binders: ValueRefT<Tuple>,
               stmts: &[ValueRef], expr: ValueRef) -> Option<ValueRefT<Block>>
    {
        allocator.create_with_slice(|base| Block { base, lex_binders, dyn_binders, expr }, stmts)
    }

    pub fn lex_binders(&self) -> ValueRefT<Tuple> { self.lex_binders }

    pub fn dyn_binders(&self) -> ValueRefT<Tuple> { self.dyn_binders }

    pub fn stmts(&self) -> &[ValueRef] { self.tail() }

    pub fn expr(&self) -> ValueRef { self.expr }
}

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Block")
         .field("base", &self.base)
         .field("expr", &self.expr)
         .field("stmts", &self.stmts())
         .finish()
    }
}

display_pretty! { Block }

impl Pretty for Block {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A> {
        docs.text("{")
            .append(docs.newline()
                        .append(docs.intersperse(self.stmts().iter()
                                                     .map(|stmt| stmt.pretty(docs))
                                                     .collect::<Vec<_>>().into_iter() // HACK
                                                     .chain(iter::once(self.expr.pretty(docs))),
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

impl Debug for Match {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Match")
         .field("base", &self.base)
         .field("matchee", &self.matchee)
         .field("default", &self.default)
         .field("cases", &self.cases())
         .finish()
    }
}

display_pretty! { Match }

impl Pretty for Match {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A)
        -> DocBuilder<'a, A>
    {
        docs.text("@match ")
            .append(self.matchee.pretty(docs))
            .append(" {")
            .append(docs.newline()
                        .append(docs.intersperse(self.cases().iter()
                                                     .map(|case| case.pretty(docs)),
                                                 docs.text(";").append(docs.newline())))
                        .nest(2))
            .append(docs.text(";")).append(docs.newline())
            .append(self.default.pretty(docs))
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

impl Debug for Case {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Case")
         .field("base", &self.base)
         .field("pattern", &self.pattern)
         .field("guard", &self.guard)
         .field("body", &self.body)
         .finish()
    }
}

display_pretty! { Case }

impl Pretty for Case {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A> {
        self.pattern.pretty(docs)
                    .append(docs.text(" | "))
                    .append(self.guard.pretty(docs))
                    .append(docs.text(" => "))
                    .append(self.body.pretty(docs))
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

impl Debug for Call {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Call")
         .field("base", &self.base)
         .field("callee", &self.callee)
         .field("args", &self.args())
         .finish()
    }
}

display_pretty! { Call }

impl Pretty for Call {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A> {
        docs.text("(")
            .append(docs.intersperse(iter::once(self.callee.pretty(docs))
                                          .chain(self.args().iter()
                                                     .map(|arg| arg.pretty(docs))),
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

impl Debug for PrimCall {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Call")
         .field("base", &self.base)
         .field("op", &self.op)
         .field("args", &self.args())
         .finish()
    }
}

display_pretty! { PrimCall }

impl Pretty for PrimCall {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A> {
        docs.text("(")
            .append(docs.intersperse(iter::once(docs.as_string(self.op))
                                          .chain(self.args().iter()
                                                     .map(|arg| arg.pretty(docs))),
                                     docs.text(" ")))
            .append(docs.text(")"))
    }
}

// ================================================================================================

/// AST node for definition statements.
heap_struct! {
    pub struct Def: UniformHeapValue {
        lex_defs: ValueRefT<Tuple>,
        dyn_defs: ValueRefT<Tuple>,
        pattern: ValueRef,
        expr: ValueRef
    }
}

impl Def {
    pub fn new(allocator: &mut Allocator, lex_defs: ValueRefT<Tuple>, dyn_defs: ValueRefT<Tuple>,
               pattern: ValueRef, expr: ValueRef) -> Option<ValueRefT<Def>>
    {
        allocator.create_uniform(|base| Def { base, lex_defs, dyn_defs, pattern, expr })
    }

    pub fn lex_defs(&self) -> ValueRefT<Tuple> { self.lex_defs }

    pub fn dyn_defs(&self) -> ValueRefT<Tuple> { self.dyn_defs }

    pub fn pattern(&self) -> ValueRef { self.pattern }

    pub fn expr(&self) -> ValueRef { self.expr }
}

impl Debug for Def {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Def")
         .field("base", &self.base)
         .field("pattern", &self.pattern)
         .field("expr", &self.expr)
         .finish()
    }
}

display_pretty! { Def }

impl Pretty for Def {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, docs: &'a A) -> DocBuilder<'a, A> {
        self.pattern.pretty(docs)
                    .append(docs.text(" = "))
                    .append(self.expr.pretty(docs))
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

    pub fn name(&self) -> ValueRefT<Symbol> { self.name }
}

impl Debug for Lex {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Lex")
         .field("base", &self.base)
         .field("name", &self.name)
         .finish()
    }
}

impl Display for Lex {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
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

    pub fn name(&self) -> ValueRefT<Symbol> { self.name }
}

impl Debug for Dyn {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Dyn")
         .field("base", &self.base)
         .field("name", &self.name)
         .finish()
    }
}

impl Display for Dyn {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
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

impl Debug for Const {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Const")
         .field("base", &self.base)
         .field("value", &self.value)
         .finish()
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        Display::fmt(&self.value, f)
    }
}

pretty_display! { Const }
