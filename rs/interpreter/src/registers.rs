use std::fmt::{self, Display, Formatter};
use std::collections::{HashSet, HashMap};

use pcws_syntax::cst::{DefRef, Const};
use anf::{Program, Function, Stmt, Expr, Triv};

// ================================================================================================

impl From<Program<DefRef>> for Program<Reg> {
    fn from(program: Program<DefRef>) -> Program<Reg> {
        let last_uses = program.liveness();
        Program {
            fns: program.fns.into_iter()
                        .map(|(name, f)| {
                            let luses = &last_uses[&name];
                            (name, f.allocate(luses))
                        })
                        .collect(),
            entry: program.entry
        }
    }
}

// ================================================================================================

type VarSet = HashSet<DefRef>;
type LastUses = (Vec<VarSet>, VarSet);

impl Program<DefRef> {
    fn liveness(&self) -> HashMap<DefRef, LastUses> {
        self.fns.iter()
            .map(|&(ref name, ref f)| (name.clone(), f.liveness()))
            .collect()
    }
}

// OPTIMIZE: Remember when vars were unused, could reduce register pressure.
impl Function<DefRef> {
    fn liveness(&self) -> LastUses {
        let &Function { ref params, ref stmts, ref expr, .. } = self;

        let mut frees = HashSet::new();
        let mut last_uses = (vec![HashSet::new(); stmts.len()], HashSet::new());

        expr.liveness(&mut frees, &mut last_uses.1);
        for (i, stmt) in stmts.iter().enumerate().rev() {
            match *stmt {
                Stmt::Def(ref def, ref expr) => {
                    expr.liveness(&mut frees, &mut last_uses.0[i]);
                    if !frees.remove(def) {
                        // Unused variable, schedule its deallocation ASAP:
                        if i + 1 < last_uses.0.len() {
                            last_uses.0[i + 1].insert(def.clone());
                        } else {
                            last_uses.1.insert(def.clone());
                        }
                    }
                },
                Stmt::Expr(ref expr) => expr.liveness(&mut frees, &mut last_uses.0[i])
            }
        }

        for param in params.iter() {
            if !frees.remove(param) {
                // Unused parameter, schedule its deallocation ASAP:
                if !last_uses.0.is_empty() {
                    last_uses.0[0].insert(param.clone());
                } else {
                    last_uses.1.insert(param.clone());
                }
            }
        }

        last_uses
    }
}

impl Expr<DefRef> {
    fn liveness(&self, free_vars: &mut VarSet, last_uses: &mut VarSet) {
        use self::Expr::*;

        match *self {
            Call(_, ref callee, ref args) => {
                callee.liveness(free_vars, last_uses);
                for arg in args.iter() { arg.liveness(free_vars, last_uses); }
            },
            PrimCall(_, _, ref args) =>
                for arg in args.iter() { arg.liveness(free_vars, last_uses); },
            Triv(_, ref t) => t.liveness(free_vars, last_uses),
            Function(_) => unreachable!()
        }
    }
}

impl Triv<DefRef> {
    fn liveness(&self, free_vars: &mut VarSet, last_uses: &mut VarSet) {
        match *self {
            Triv::Var(ref def) =>
                if free_vars.insert(def.clone()) {
                    last_uses.insert(def.clone());
                },
            Triv::Const(_) => {}
        }
    }
}

// ================================================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Reg(usize);

impl From<usize> for Reg {
    fn from(i: usize) -> Reg { Reg(i) }
}

impl From<Reg> for Triv<Reg> {
    fn from(reg: Reg) -> Triv<Reg> { Triv::Var(reg) }
}

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        write!(f, "r{}", self.0)
    }
}

mod register_file {
    use std::collections::{HashMap, BTreeMap};
    use std::ops::Range;

    use super::Reg;

    pub struct Registers {
        by_start: BTreeMap<usize, Range<usize>>,
        by_end: HashMap<usize, Range<usize>>,
        cap: usize
    }

    impl Registers {
        pub fn new() -> Registers {
            Registers {
                by_start: BTreeMap::new(),
                by_end: HashMap::new(),
                cap: 0
            }
        }

        fn take_by_start(&mut self, start: usize) -> Range<usize> {
            let range = self.by_start.remove(&start).unwrap();
            self.by_end.remove(&range.end);
            range
        }

        fn take_by_end(&mut self, end: usize) -> Range<usize> {
            let range = self.by_end.remove(&end).unwrap();
            self.by_start.remove(&range.start);
            range
        }

        fn deallocate_range(&mut self, range: Range<usize>) {
            // If possible, pull out adjacent ranges to extend the bounds of `range`:
            let start = if self.by_end.contains_key(&range.start) {
                self.take_by_end(range.start).start
            } else {
                range.start
            };
            let end = if self.by_start.contains_key(&range.end) {
                self.take_by_start(range.end).end
            } else {
                range.end
            };
            self.deallocate_isolated_range(start..end);
        }

        fn deallocate_isolated_range(&mut self, range: Range<usize>) {
            if range.len() > 0 {
                self.by_start.insert(range.start, range.clone());
                self.by_end.insert(range.end, range);
            }
        }

        pub fn preallocate(&mut self, reg: Reg) {
            if reg.0 < self.cap {
                if let Some((start, range)) = self.by_start.iter()
                                                  .find(|&(_, range)| range.contains(reg.0))
                                                  .map(|(start, range)|
                                                      (start.clone(), range.clone())
                                                  )
                {
                    // Remove `reg` from the range and put the remaining subranges back:
                    self.take_by_start(start);
                    self.deallocate_isolated_range(range.start..reg.0);
                    self.deallocate_isolated_range((reg.0 + 1)..range.end);
                } else {
                    panic!("attempted to preallocate {}, but it was in use", reg);
                }
            } else {
                // Add registers so that `reg` is the last one, put the others to range maps:
                let slack = self.cap..reg.0;
                self.deallocate_range(slack);
                self.cap = reg.0 + 1;
            }
        }

        pub fn allocate(&mut self) -> Reg {
            if let Some(&start) = self.by_start.keys().next() {
                // Take the first register from the range and put the others back:
                let range = self.take_by_start(start);
                self.deallocate_isolated_range((start + 1)..range.end);
                start.into()
            } else {
                // Add one register and allocate it immediately:
                let r = self.cap;
                self.cap += 1;
                r.into()
            }
        }

        pub fn preallocate_range(&mut self, range: Range<Reg>) {
            unimplemented!()
        }

        pub fn temp_range(&mut self, len: usize) -> Range<usize> {
            if let Some((&start, _)) = self.by_start.iter()
                                                    .find(|&(start, range)| range.len() >= len)
            {
                start..(start + len)
            } else {
                let range = self.cap..(self.cap + len);
                self.cap += len;
                range
            }
        }

        pub fn deallocate(&mut self, reg: Reg) {
            self.deallocate_range(reg.0..(reg.0 + 1));
        }
    }
}

use self::register_file::Registers;

// ================================================================================================

type RegEnv = HashMap<DefRef, Reg>;

enum Ctx { Def, Eff, Tail }

impl Function<DefRef> {
    fn allocate(self, last_uses: &LastUses) -> Function<Reg> {
        let Function { pos, params, free_vars, stmts: old_stmts, expr: old_expr } = self;

        // State of the registers:
        let mut registers = Registers::new();
        let mut env = HashMap::new();

        // Preallocate registers for parameters according to the calling convention:
        let mut param_regs = Vec::with_capacity(params.len());
        for (i, param) in params.iter().enumerate() {
            let reg = Reg::from(i);
            registers.preallocate(reg);
            env.insert(param.clone(), reg);
            param_regs.push(reg);
        }

        // Traverse the body, updating register state and accumulating the new body:
        let mut stmts = Vec::new();
        for (i, stmt) in old_stmts.into_iter().enumerate() {
            stmt.allocate(&last_uses.0[i], &mut env, &mut registers, &mut stmts);
        }
        let expr =
            old_expr.allocate(Ctx::Tail, &last_uses.1, &mut env, &mut registers, &mut stmts);

        Function { pos, params: param_regs, free_vars, stmts, expr }
    }
}

impl Stmt<DefRef> {
    fn allocate(self, last_uses: &VarSet, env: &mut RegEnv, registers: &mut Registers,
                stmts: &mut Vec<Stmt<Reg>>)
    {
        match self {
            Stmt::Def(def, expr) => {
                let expr = expr.allocate(Ctx::Def, last_uses, env, registers, stmts);

                // Allocate register for the result:
                let reg = registers.allocate();
                env.insert(def, reg);

                stmts.push(Stmt::Def(reg, expr));
            },
            Stmt::Expr(expr) => {
                let expr = expr.allocate(Ctx::Eff, last_uses, env, registers, stmts);
                stmts.push(Stmt::Expr(expr));
            }
        }
    }
}

impl Expr<DefRef> {
    fn allocate(self, ctx: Ctx, last_uses: &VarSet, env: &mut RegEnv, registers: &mut Registers,
                stmts: &mut Vec<Stmt<Reg>>) -> Expr<Reg>
    {
        use self::Expr::*;

        // Transform the expression, maybe also accumulating some more stmts:
        let expr = match self {
            Call(pos, callee, args) => match ctx {
                Ctx::Tail => {
                    // TODO: Determine move graph (move args to regs 0..args.len()).
                    // TODO: Emit `__mov`:s.
                    // TODO: Emit `__swap`:s.
                    // TODO: Emit `__tailCall`.
                    unimplemented!()
                },
                _ => {
                    // TODO: Compute variables to save (env.keys() - last_uses)
                    // TODO: Determine move graph (move saves to regs 0..saves.len()
                    //       [if env[save] < saves.len(), does not need to be moved],
                    //       move args to regs (saves.len() + 3)..(saves.len() + 3 + args.len()))
                    // TODO: Emit `__mov`:s, also updating `env` and `registers`.
                    // TODO: Emit `__swap`:s, also updating `env` and `registers`.
                    // TODO: Emit `__call ,(saves.len() + 3s)`.
                    unimplemented!()
                }
            },
            // FIXME: Need to remember which arity of a vararg opcode was used:
            PrimCall(pos, op, args) => match ctx {
                Ctx::Def | Ctx::Tail if args.len() <= 2 =>
                    PrimCall(pos, op, args.into_iter().map(|arg| arg.updated(env)).collect()),
                Ctx::Eff if args.len() <= 3 =>
                    PrimCall(pos, op, args.into_iter().map(|arg| arg.updated(env)).collect()),
                _ => {
                    // OPTIMIZE: Could avoid some __mov:s by allowing variables that are last used
                    //           here to overlap the temp range.
                    let temp_count = args.len();
                    let temps = registers.temp_range(temp_count);
                    let temps_start = temps.start;

                    stmts.extend(temps.zip(args)
                                      .map(|(temp, arg)|
                                          Stmt::Def(temp.into(),
                                                    Triv(pos.clone(), arg.updated(env).into()))
                                      ));

                    PrimCall(pos, op, vec![Reg::from(temps_start).into(),
                                           Const::from(temp_count as isize).into()])
                }
            },
            Triv(pos, t) => Triv(pos, t.updated(env)),
            Function(_) => unreachable!()
        };

        // Free registers of the variables whose last use or decay point was here:
        for var in last_uses.iter() {
            registers.deallocate(env[var]);
            env.remove(var);
        }

        expr
    }
}

impl Triv<DefRef> {
    fn updated(self, env: &RegEnv) -> Triv<Reg> {
        match self {
            Triv::Var(def) => if let Some(&reg) = env.get(&def) {
                Triv::Var(reg)
            } else {
                panic!("undefined variable {}", def)
            },
            Triv::Const(c) => Triv::Const(c)
        }
    }
}
