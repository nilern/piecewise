use std::hash::Hash;
use std::iter;
use std::fmt::{self, Display, Formatter};
use std::collections::HashSet;
use pretty::{self, Doc, DocAllocator, DocBuilder};

use pcws_syntax::cst::{self, Pattern, Def, DefRef, Const, PrimOp, Pos, Positioned};
use patterns::PatternsExpanded;

// ================================================================================================

pub struct Program<V> where V: Eq + Hash {
    pub fns: Vec<(DefRef, Function<V>)>,
    pub entry: DefRef
}

impl<V> Display for Program<V> where V: Display + Eq + Hash {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let allocator = pretty::Arena::new();
        let doc = allocator.intersperse(
            self.fns.iter().map(|&(ref k, ref v)| allocator.as_string(k.borrow())
                                                           .append(" = ")
                                                           .append(v.pretty(&allocator))),
            allocator.newline());
        <DocBuilder<_> as Into<Doc<_>>>::into(doc).render_fmt(80, f)
    }
}

#[derive(Debug, Clone)]
pub struct Function<V> where V: Eq + Hash {
    pub pos: Pos,
    pub params: Vec<V>,
    pub free_vars: HashSet<DefRef>,
    pub stmts: Vec<Stmt<V>>,
    pub expr: Expr<V>
}

impl<V> Positioned for Function<V> where V: Eq + Hash {
    fn pos(&self) -> &Pos { &self.pos }
}

#[derive(Debug, Clone)]
pub enum Expr<V> where V: Eq + Hash {
    Function(Box<Function<V>>),
    Call(Pos, Triv<V>, Vec<Triv<V>>),
    PrimCall(Pos, PrimOp, Vec<Triv<V>>),
    Triv(Pos, Triv<V>)
}

impl<V> Positioned for Expr<V> where V: Eq + Hash {
    fn pos(&self) -> &Pos {
        use self::Expr::*;

        match *self {
            Function(ref f) => f.pos(),
            Call(ref pos, _, _) | PrimCall(ref pos, _, _) | Triv(ref pos, _) => pos,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt<V> where V: Eq + Hash {
    Def(V, Expr<V>),
    Expr(Expr<V>)
}

impl<V> Positioned for Stmt<V> where V: Eq + Hash {
    fn pos(&self) -> &Pos {
        match *self {
            Stmt::Def(_, ref expr) => expr.pos(),
            Stmt::Expr(ref expr) => expr.pos()
        }
    }
}

impl<V> From<Expr<V>> for Stmt<V> where V: Eq + Hash {
    fn from(expr: Expr<V>) -> Stmt<V> { Stmt::Expr(expr) }
}

#[derive(Debug, Clone)]
pub enum Triv<V> where V: Eq + Hash {
    Var(V),
    Const(Const)
}

impl From<DefRef> for Triv<DefRef> {
    fn from(var: DefRef) -> Triv<DefRef> { Triv::Var(var) }
}

impl<V> From<Const> for Triv<V> where V: Eq + Hash {
    fn from(c: Const) -> Triv<V> { Triv::Const(c) }
}

// ================================================================================================

impl From<cst::Program<PatternsExpanded>> for Program<DefRef> {
    fn from(program: cst::Program<PatternsExpanded>) -> Program<DefRef> {
        let entry = Def::new("entry");
        Program {
            fns: vec![(entry.clone(), Function::from_cst(
                program.cst.pos().clone(),
                Vec::new(),
                program.cst
            ))],
            entry
        }
    }
}

impl Function<DefRef> {
    fn from_cst(pos: Pos, params: Vec<DefRef>, body: cst::Expr) -> Function<DefRef> {
        let mut stmts = Vec::new();
        let expr = body.convert(&mut stmts);
        Function {
            pos, params: Vec::new(), free_vars: HashSet::new(),
            stmts, expr
        }
    }
}

trait AnfConvert {
    fn convert(self, stmts: &mut Vec<Stmt<DefRef>>) -> Expr<DefRef>;
}

impl AnfConvert for cst::Expr {
    fn convert(self, stmts: &mut Vec<Stmt<DefRef>>) -> Expr<DefRef> {
        use self::Expr::*;

        match self {
            cst::Expr::Function(pos, params, body) =>
                Function(Box::new(self::Function::from_cst(pos, params, *body))),
            cst::Expr::Block(_, old_stmts, expr) => {
                for stmt in old_stmts {
                    match stmt {
                        cst::Stmt::Def(Pattern::Lex(_, def), expr) => {
                            let expr = expr.convert(stmts);
                            stmts.push(Stmt::Def(def, expr));
                        },
                        cst::Stmt::Def(..) => unreachable!(),
                        cst::Stmt::Expr(expr) => {
                            let expr = expr.convert(stmts);
                            stmts.push(expr.into());
                        }
                    }
                }
                expr.convert(stmts)
            },
            cst::Expr::Call(pos, callee, args) =>
                Call(pos, callee.convert(stmts).trivialize(stmts),
                          args.into_iter()
                              .map(|arg| arg.convert(stmts).trivialize(stmts))
                              .collect()),
            cst::Expr::PrimCall(pos, op, args) =>
                PrimCall(pos, op, args.into_iter()
                                      .map(|arg| arg.convert(stmts).trivialize(stmts))
                                      .collect()),
            cst::Expr::Lex(pos, def) => Triv(pos, def.into()),
            cst::Expr::Const(pos, c) => Triv(pos, c.into()),
            cst::Expr::Match(..) | cst::Expr::Dyn(..) => unreachable!()
        }
    }
}

trait Trivialize {
    fn trivialize(self, stmts: &mut Vec<Stmt<DefRef>>) -> Triv<DefRef>;
}

impl Trivialize for Expr<DefRef> {
    fn trivialize(self, stmts: &mut Vec<Stmt<DefRef>>) -> Triv<DefRef> {
        use self::Expr::*;

        if let Triv(_, t) = self {
            t
        } else {
            let def = Def::new("tmp");
            stmts.push(Stmt::Def(def.clone(), self));
            def.into()
        }
    }
}

// ================================================================================================

impl<V> Display for Function<V> where V: Display + Eq + Hash {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let allocator = pretty::Arena::new();
        <DocBuilder<_> as Into<Doc<_>>>::into(self.pretty(&allocator))
                                        .render_fmt(80, f)
    }
}

impl<V> Function<V> where V: Display + Eq + Hash {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        fn pretty_block<'a, V, A>(stmts: &'a [Stmt<V>], expr: &'a Expr<V>, allocator: &'a A)
            -> DocBuilder<'a, A>
            where V: Display + Eq + Hash, A: DocAllocator<'a>
        {
            allocator.intersperse(stmts.iter()
                                       .map(|stmt| stmt.pretty(allocator))
                                       .chain(iter::once(expr.pretty(allocator))),
                                  allocator.newline())
        }

        let &Function { pos: _, ref params, free_vars: _, ref stmts, ref expr } = self;
        allocator.text("@fn (")
                 .append(allocator.intersperse(
                            params.iter().map(|param| allocator.as_string(param)),
                            " "))
                 .append(") {")
                 .append(allocator.newline()
                                  .append(pretty_block(stmts, expr, allocator))
                                  .nest(2))
                 .append(allocator.newline())
                 .append("}")
    }
}

impl<V> Expr<V> where V: Display + Eq + Hash {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        use self::Expr::*;

        match self {
            &Function(ref f) => f.pretty(allocator),
            &Call(_, ref callee, ref args) =>
                allocator.intersperse(
                    iter::once(callee.pretty(allocator))
                         .chain(args.iter().map(|arg| arg.pretty(allocator))),
                    " "),
            &PrimCall(_, op, ref args) =>
                allocator.intersperse(
                    iter::once(allocator.as_string(op))
                         .chain(args.iter().map(|arg| arg.pretty(allocator))),
                    " "),
            &Triv(_, ref t) => t.pretty(allocator)
        }
    }
}

impl<V> Stmt<V> where V: Display + Eq + Hash {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        match self {
            &Stmt::Def(ref def, ref val) =>
                allocator.as_string(def)
                         .append(" = ").append(val.pretty(allocator)),
            &Stmt::Expr(ref expr) => expr.pretty(allocator)
        }
    }
}

impl<V> Triv<V> where V: Display + Eq + Hash {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        match self {
            &Triv::Var(ref def) => allocator.as_string(def),
            &Triv::Const(ref c) => allocator.as_string(c)
        }
    }
}
