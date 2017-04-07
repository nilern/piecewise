use std::fmt;
use std::fmt::Display;
use std::collections::{HashMap, HashSet};
use std::iter;
use std::ops::Range;

use util::{Sourced, Name, SrcPos, Either, fresh_label, push_label};
use ast;
use ast::{Var, VarRef, Const, ConstVal};
use passes::flatten;
use passes::flatten::{FAST, Closure};

// TODO: Differentiate trivial/serious exprs on the type level.

#[derive(Debug)]
pub struct CPS {
    procs: HashMap<Name, Fun>,
    body: ContMap
}

impl From<FAST> for CPS {
    fn from(fast: FAST) -> CPS {
        CPS {
            procs: fast.procs.into_iter()
                             .map(|(name, f)| (name, Fun::from(f)))
                             .collect(),
            body: ContMap::new(fast.expr, ContRef::Halt)
        }
    }
}

impl Display for CPS {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut procvec: Vec<(&Name, &Fun)> = self.procs.iter().collect();
        procvec.sort_by_key(|&(n, _)| n);

        for (name, p) in procvec {
            write!(f, "{} {}\n\n", name, p)?;
        }
        write!(f, "{}", self.body)
    }
}

#[derive(Debug)]
pub enum Expr {
    App(App, ContRef),
    Next(App),
    If(Triv, ContRef, ContRef),
    Closure(Closure, ContRef),
    Triv(Triv, ContRef)
}

impl Expr {
    fn uses(&self) -> Box<Iterator<Item=Name>> {
        use self::Expr::*;
        match self {
            &App(ref app, _) => app.uses(),
            &Next(ref app) => app.uses(),
            &If(ref cond, _, _) => cond.uses(),
            &Closure(ref c, _) => c.uses(),
            &Triv(ref t, _) => t.uses()
        }
    }

    fn successors(&self) -> Box<Iterator<Item=usize>> {
        use self::Expr::*;
        match self {
            &App(_, ContRef::Local(k)) => Box::new(iter::once(k)),
            &If(_, ContRef::Local(k), ContRef::Local(l)) =>
                Box::new(iter::once(l).chain(iter::once(k))),
            &If(_, ContRef::Local(k), _) | &If(_, _, ContRef::Local(k)) => Box::new(iter::once(k)),
            &Closure(_, ContRef::Local(k)) => Box::new(iter::once(k)),
            &Triv(_, ContRef::Local(k)) => Box::new(iter::once(k)),
            _ => Box::new(iter::empty())
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Expr::App(ref app, k) => write!(f, "{} -> {}", app, k),
            &Expr::Next(ref app) => write!(f, "next {} -> ret", app),
            &Expr::If(ref app, k, l) => write!(f, "{} -> {} | {}", app, k, l),
            &Expr::Closure(ref cl, k) => write!(f, "{} -> {}", cl, k),
            &Expr::Triv(ref t, k) => write!(f, "{} -> {}", t, k)
        }
    }
}

impl Closure {
    fn uses(&self) -> Box<Iterator<Item=Name>> {
        let clover_uses: Vec<Name> = self.freevars.iter().cloned().collect();
        Box::new(clover_uses.into_iter())
    }
}

#[derive(Debug)]
pub enum Triv {
    Var(Var),
    Const(Const)
}

impl Triv {
    fn uses(&self) -> Box<Iterator<Item=Name>> {
        match self {
            &Triv::Var(ref v) => v.uses(),
            &Triv::Const(_) => Box::new(iter::empty())
        }
    }
}

impl Display for Triv {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Triv::Var(ref v) => v.fmt(f),
            &Triv::Const(ref c) => c.fmt(f)
        }
    }
}

impl Var {
    fn uses(&self) -> Box<Iterator<Item=Name>> {
        match &self.name {
            &VarRef::Local(ref n) => Box::new(iter::once(n.clone())),
            &VarRef::Clover(_) | &VarRef::Global(_) => Box::new(iter::empty())
        }
    }
}

pub type App = ast::App<Triv>;

impl App {
    fn uses(&self) -> Box<Iterator<Item=Name>> {
        let arg_uses: Vec<Name> = self.args.iter().flat_map(|arg| arg.uses()).collect();
        Box::new(self.op.uses().chain(arg_uses.into_iter()))
    }
}

#[derive(Debug)]
pub struct Fun {
    pub pos: SrcPos,
    pub freevars: Vec<Name>,
    pub clauses: HashMap<usize, Clause>
}

impl Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "[")?;
        for v in self.freevars.iter() {
            write!(f, "{}, ", v)?;
        }
        try!(write!(f, "] {{\n"));
        let mut it = self.clauses.iter();
        if let Some((_, clause)) = it.next() {
            try!(write!(f, "{}", clause));
        }
        for (_, clause) in it {
            try!(write!(f, "; {}", clause));
        }
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct Clause {
    pub pos: SrcPos,
    pub params: Vec<Name>,
    pub body: ContMap,
}

impl Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "{:?}", self.body.ralloc_prereqs());
        for param in self.params.iter() {
            write!(f, "{} ", param)?;
        }
        write!(f, "=>\n{}", self.body)
    }
}

/// Continuation.
#[derive(Debug)]
pub struct Cont {
    pub param: Option<Name>,
    pub expr: Expr
}

impl Cont {
    fn def(&self) -> Box<Iterator<Item=Name>> {
        Box::new(self.param.clone().into_iter())
    }

    fn successors(&self) -> Box<Iterator<Item=usize>> {
        self.expr.successors()
    }
}

impl Display for Cont {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for param in self.param.iter() {
            write!(f, "{} ", param)?;
        }
        write!(f, "= {}", self.expr)
    }
}

/// Continuation reference (similar to variable reference)
#[derive(Debug, Clone, Copy)]
pub enum ContRef {
    Local(usize),
    Ret,
    Halt
}

impl Display for ContRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ContRef::Local(i) => write!(f, "k[{}]", i),
            &ContRef::Ret => write!(f, "ret"),
            &ContRef::Halt => write!(f, "halt")
        }
    }
}

/// A mapping from labels to continuations.
#[derive(Debug)]
pub struct ContMap {
    entry: usize,
    conts: HashMap<usize, Cont>
}

impl ContMap {
    fn new(expr: flatten::Expr, cont: ContRef) -> ContMap {
        let k = fresh_label();
        let mut res = ContMap {
            entry: k,
            conts: HashMap::new()
        };
        let _ = res.convert_nontrivially(k, None, expr, cont);
        res
    }

    fn get(&self, label: usize) -> Option<&Cont> {
        self.conts.get(&label)
    }

    fn insert(&mut self, label: usize, tempname: Option<Name>, expr: Expr) {
        self.conts.insert(label, Cont {
            param: tempname,
            expr: expr
        });
    }

    fn postwalk<F>(&self, mut f: F) where F: FnMut(usize, &Cont) -> () {
        fn dfs<F>(conts: &ContMap, seen: &mut HashSet<usize>, f: &mut F, label: usize)
            where F: FnMut(usize, &Cont) -> ()
        {
            if !seen.contains(&label) {
                seen.insert(label);
                let cont = conts.get(label).unwrap();
                for label in cont.successors().into_iter() {
                    dfs(conts, seen, f, label);
                }
                f(label, cont)
            }
        }

        let mut seen = HashSet::new();
        dfs(self, &mut seen, &mut f, self.entry)
    }

    fn post_order(&self) -> Vec<usize> {
        let mut order = Vec::new();
        self.postwalk(|label, _| order.push(label));
        order
    }

    fn convert_step(&mut self, label: usize, mut tempname: Option<Name>, expr: flatten::Expr,
                    cont: ContRef) -> Either<(Triv, Option<Name>), bool> {
        use util::Either::*;
        match expr {
            flatten::Expr::Block(flatten::Block { pos, mut stmts }) => {
                // TODO: Declare variables at start of block.
                if stmts.len() > 0 {
                    let mut oldk = label;
                    let mut k = None;
                    let lasti = stmts.len() - 1;
                    for stmt in stmts.drain(0..lasti) {
                        k = Some(fresh_label());
                        match stmt {
                            ast::Stmt::Def { name, val } => { // Nontail definition:
                                let _ = self.convert_nontrivially(oldk, tempname, val, ContRef::Local(k.unwrap()));
                                oldk = k.unwrap();
                                tempname = Some(name);
                            },
                            ast::Stmt::Expr(expr) => // Nontail expression:
                                tempname = match self.convert_step(oldk, tempname, expr, ContRef::Local(k.unwrap()))
                                {
                                    Left((_, n)) => {
                                        push_label(k.unwrap());
                                        k = None;
                                        n // treat as dead code
                                    },
                                    Right(_) => {
                                        oldk = k.unwrap();
                                        None
                                    }
                                }
                        }
                    }
                    // TODO: What to do with `Stmt::Def`:s as last statements?
                    Right(self.convert_nontrivially(oldk, tempname, stmts.pop().unwrap().into_expr(), cont))
                } else { // Empty block:
                    Left((Triv::Const(Const {
                        pos: pos,
                        val: ConstVal::Bool(false) // TODO: Return `()`
                    }), tempname))
                }
            },
            flatten::Expr::App(flatten::App { pos, box op, args }) => {
                let mut oldk = label;
                let mut k = Some(fresh_label());
                let (cop, lused, mut tempname) = self.convert_subexpr(oldk, tempname, op, ContRef::Local(k.unwrap()));
                if lused {
                    oldk = k.unwrap();
                } else {
                    push_label(k.unwrap());
                    k = None;
                }
                let mut cargs = Vec::new();
                for arg in args {
                    k = Some(fresh_label());
                    let (carg, lused, tn) = self.convert_subexpr(oldk, tempname.take(), arg, ContRef::Local(k.unwrap()));
                    if lused {
                        oldk = k.unwrap();
                    } else {
                        push_label(k.unwrap());
                        k = None;
                    }
                    tempname = tn;
                    cargs.push(carg);
                }

                self.insert(oldk, tempname, Expr::App(App {
                    pos: pos,
                    op: Box::new(cop),
                    args: cargs
                }, cont));
                Right(true)
            },
            flatten::Expr::Closure(cl) => {
                self.insert(label, tempname, Expr::Closure(cl, cont));
                Right(true)
            },
            flatten::Expr::Var(v) => Left((Triv::Var(v), tempname)),
            flatten::Expr::Const(c) => Left((Triv::Const(c), tempname))
        }
    }

    /// Like `convert_step` but always returns a trivial term even if `expr` was serious.
    fn convert_subexpr(&mut self, label: usize, tempname: Option<Name>, expr: flatten::Expr,
                       cont: ContRef) -> (Triv, bool, Option<Name>) {
        use util::Either::*;
        let pos = expr.pos();
        match self.convert_step(label, tempname, expr, cont) {
            Left((t, n)) => (t, false, n),
            Right(true) => {
                let name = Name::fresh(String::from("v"));
                (Triv::Var(Var {
                     pos: pos,
                     name: VarRef::Local(name.clone()),
                 }), true, Some(name))
            },
            Right(false) =>
                (Triv::Const(Const {
                     pos: pos,
                     val: ConstVal::Bool(false) // TODO: return `()`
                 }), true, None)
        }
    }

    /// Like `convert_step` but force pushing of `Cont` instead of returning trivial term.
    fn convert_nontrivially(&mut self, label: usize, tempname: Option<Name>, expr: flatten::Expr,
                            cont: ContRef) -> bool {
        use util::Either::*;
        match self.convert_step(label, tempname, expr, cont) {
            Left((t, n)) => {
                self.insert(label, n, Expr::Triv(t, cont));
                true
            }
            Right(b) => b
        }
    }

    // TODO: scope end sets
    fn ralloc_prereqs(&self)
        -> (HashMap<usize, HashSet<Name>>, HashMap<usize, HashMap<Name, RegGoal>>)
    {
        let mut liveness = HashMap::new();
        let mut goals = HashMap::new();
        let mut curr_goal = HashMap::new();
        for label in self.post_order().into_iter() {
            let cont = self.get(label).unwrap();

            // Live-outs (that is, continuation clovers):
            let mut live: HashSet<Name> = HashSet::new();
            for succ_label in cont.successors() {
                let succ = self.get(succ_label).unwrap();
                let succ_live: &HashSet<Name> = liveness.get(&succ_label).unwrap();
                live.extend(succ_live.iter().filter(|name|
                    succ.def().find(|def_name| def_name == *name).is_none()).cloned());
            }

            // At call sites the current goal is also changed:
            if let &Expr::App(App { pos: _, box ref op, ref args}, _) = &cont.expr {
                for succ_label in cont.successors() {
                    goals.insert(succ_label, curr_goal.clone());
                }
                let prev_goal = curr_goal;
                // FIXME: `foo(bar, bar)`
                // Function and arguments should go above all the clovers in order:
                curr_goal = iter::once(op).chain(args.iter())
                    .zip(live.len()..args.len() + 1)
                    .filter_map(|(t, r)|
                        if let &Triv::Var(Var { name: VarRef::Local(ref n), .. }) = t {
                            Some((n.clone(), RegGoal::singleton(r)))
                        } else {
                            None
                        })
                    .collect();
                // Clovers should go to registers numbered below the clover count:
                for name in live.iter() {
                    curr_goal.insert(name.clone(), prev_goal.get(name).unwrap().re_bound(0..live.len()));
                }
            }

            // Add the uses of this Cont to the live-outs:
            live.extend(cont.expr.uses());
            liveness.insert(label, live);
        }
        goals.insert(self.entry, curr_goal);
        (liveness, goals)
    }
}

impl Display for ContMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fn fmt_cont(conts: &ContMap, label: usize, cont: &Cont, seen: &mut HashSet<usize>,
                    f: &mut fmt::Formatter) -> Result<(), fmt::Error>
        {
            seen.insert(label);
            writeln!(f, "    k[{}] {}", label, cont)?;
            for label in cont.successors().into_iter() {
                fmt_cont(conts, label, conts.conts.get(&label).unwrap(), seen, f)?;
            }
            Ok(())
        }

        let mut seen = HashSet::new();
        fmt_cont(self, self.entry, self.conts.get(&self.entry).unwrap(), &mut seen, f)?;
        for (label, cont) in self.conts.iter() {
            if !seen.contains(&label) {
                writeln!(f, "    k[{}] {}", label, cont)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct RegGoal(Vec<usize>);

impl RegGoal {
    fn singleton(reg: usize) -> RegGoal {
        RegGoal(vec![reg])
    }

    fn re_bound(&self, bounds: Range<usize>) -> RegGoal {
        let mut regs: Vec<usize> = bounds.collect();
        regs.sort_by_key(|&reg| self.priority_of(reg).unwrap_or(self.0.len()));
        RegGoal(regs)
    }

    fn priority_of(&self, reg: usize) -> Option<usize> {
        self.0.iter().position(|&r| r == reg)
    }
}

// ------------------------------------------------------------------------------------------------

impl FAST {
    pub fn to_cps(self) -> CPS {
        CPS::from(self)
    }
}

// FIXME: rename param uses
impl From<flatten::Fun<flatten::Clause>> for Fun {
    fn from(fun: flatten::Fun<flatten::Clause>) -> Fun {
        fn convert_body(conts: &mut ContMap, label: usize, stmts: Vec<flatten::Stmt>) {
            // TODO: get rid of the block construction:
            conts.convert_nontrivially(label, None, flatten::Expr::Block(flatten::Block {
                pos: stmts[0].pos(),
                stmts: stmts
            }), ContRef::Ret);
        }

        fn convert_clause_vec(clauses: Vec<flatten::Clause>) -> Clause {
            let pos = clauses[0].pos();
            let mut params = None;

            let mut entry = fresh_label();
            let mut cbody = ContMap {
                entry: entry,
                conts: HashMap::new()
            };

            let mut irrefutable = false;
            for flatten::Clause { pos: _, params: ps, cond, body} in clauses {
                params = params.or(Some(ps.clone()));
                match cond {
                    flatten::Expr::Const(Const { val: ConstVal::Bool(true), pos: _ }) => {
                        convert_body(&mut cbody, entry, body);
                        irrefutable = true;
                        break;
                    },
                    flatten::Expr::Const(Const { val: ConstVal::Bool(false), pos: _ }) => {
                        continue;
                    },
                    _ => {
                        let mut ck = fresh_label();
                        let (ccond, lused, tempname) =
                            cbody.convert_subexpr(entry, None, cond, ContRef::Local(ck));
                        if !lused {
                            push_label(ck);
                            ck = entry;
                        }

                        let bk = fresh_label();
                        let nk = fresh_label();
                        cbody.insert(ck, tempname,
                                     Expr::If(ccond, ContRef::Local(bk), ContRef::Local(nk)));
                        convert_body(&mut cbody, bk, body);
                        entry = nk;
                    }
                }
            }

            if !irrefutable {
                cbody.insert(entry, None, Expr::Next(App {
                    pos: pos,
                    op: Box::new(Triv::Var(Var {
                        pos: pos,
                        name: VarRef::Local(Name::fresh(String::from("self"))) // FIXME
                    })),
                    args: params.clone().unwrap().into_iter().map(|param| Triv::Var(Var {
                        pos: pos,
                        name: VarRef::Local(param)
                    })).collect()
                }));
            }

            Clause {
                pos: pos,
                params: params.unwrap(),
                body: cbody
            }
        }

        let mut cclauses = HashMap::new();
        for (arity, clauses) in fun.clauses {
            cclauses.insert(arity, convert_clause_vec(clauses));
        }

        Fun {
            pos: fun.pos,
            freevars: fun.freevars,
            clauses: cclauses
        }
    }
}
