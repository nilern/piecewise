use std::iter;
use std::fmt::{self, Display, Formatter};
use pretty::{self, Doc, DocAllocator, DocBuilder};

use pcws_syntax::cst::{self, Program, Pattern, Def, DefRef, Const, PrimOp, Pos};
use patterns::PatternsExpanded;

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Expr
}

#[derive(Debug, Clone)]
pub enum Expr {
    Function(Pos, Vec<DefRef>, Box<Block>),
    Call(Pos, Triv, Vec<Triv>),
    PrimCall(Pos, PrimOp, Vec<Triv>),
    Triv(Pos, Triv)
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Def(DefRef, Expr),
    Expr(Expr)
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Stmt { Stmt::Expr(expr) }
}

#[derive(Debug, Clone)]
pub enum Triv {
    Var(DefRef),
    Const(Const)
}

impl From<DefRef> for Triv {
    fn from(def: DefRef) -> Triv { Triv::Var(def) }
}

impl From<Const> for Triv {
    fn from(c: Const) -> Triv { Triv::Const(c) }
}

// ================================================================================================

impl From<Program<PatternsExpanded>> for Block {
    fn from(program: Program<PatternsExpanded>) -> Block {
        program.cst.into()
    }
}

impl From<cst::Expr> for Block {
    fn from(expr: cst::Expr) -> Block {
        let mut stmts = Vec::new();
        let expr = expr.convert(&mut stmts);
        Block { stmts, expr }
    }
}

trait AnfConvert {
    fn convert(self, stmts: &mut Vec<Stmt>) -> Expr;
}

impl AnfConvert for cst::Expr {
    fn convert(self, stmts: &mut Vec<Stmt>) -> Expr {
        use self::Expr::*;

        match self {
            cst::Expr::Function(pos, params, body) =>
                Function(pos, params, Box::new(Block::from(*body))),
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
    fn trivialize(self, stmts: &mut Vec<Stmt>) -> Triv;
}

impl Trivialize for Expr {
    fn trivialize(self, stmts: &mut Vec<Stmt>) -> Triv {
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

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let allocator = pretty::Arena::new();
        <DocBuilder<_> as Into<Doc<_>>>::into(self.pretty(&allocator))
                                        .render_fmt(80, f)
    }
}

impl Block {
    fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        let &Block { ref stmts, ref expr } = self;
        allocator.intersperse(stmts.iter()
                                   .map(|stmt| stmt.pretty(allocator))
                                   .chain(iter::once(expr.pretty(allocator))),
                              allocator.newline())
    }
}

impl Expr {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        use self::Expr::*;

        match self {
            &Function(_, ref params, ref body) =>
                allocator.text("@fn (")
                         .append(allocator.intersperse(
                                    params.iter().map(|param| allocator.as_string(param.borrow())),
                                    " "))
                         .append(") {")
                         .append(allocator.newline()
                                          .append(body.pretty(allocator))
                                          .nest(2))
                         .append(allocator.newline())
                         .append("}"),
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

impl Stmt {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        match self {
            &Stmt::Def(ref def, ref val) =>
                allocator.as_string(def.borrow())
                         .append(" = ").append(val.pretty(allocator)),
            &Stmt::Expr(ref expr) => expr.pretty(allocator)
        }
    }
}

impl Triv {
    pub fn pretty<'a, A: DocAllocator<'a>>(&'a self, allocator: &'a A) -> DocBuilder<'a, A> {
        match self {
            &Triv::Var(ref def) => allocator.as_string(def.borrow()),
            &Triv::Const(ref c) => allocator.as_string(c)
        }
    }
}
