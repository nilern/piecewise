use std::iter;

use pcws_syntax::cst::{self, Program, Expr, Stmt, Case, Pattern, PrimOp, Def, DefRef,
                       Pos, Positioned, CstFactory};

use binding::BindingsReified;

// ================================================================================================

pub enum PatternsExpanded {}

pub trait PatternMatchingPass {
    type Target;

    // OPTIMIZE: &mut self
    fn expand_patterns(self) -> Self::Target;
}

impl PatternMatchingPass for Program<BindingsReified> {
    type Target = Program<PatternsExpanded>;

    fn expand_patterns(self) -> Program<PatternsExpanded> {
        Program::new(self.cst.expand_patterns())
    }
}

// ================================================================================================

trait ExpandPatterns {
    // OPTIMIZE: &mut self
    fn expand_patterns(self) -> Self;
}

impl ExpandPatterns for Expr {
    fn expand_patterns(self) -> Expr {
        use self::Expr::*;

        match self {
            Function(pos, params, body) =>
                Function(pos, params, Box::new(body.expand_patterns())),
            Block(pos, old_stmts, expr) => {
                // HACK: All blocks should use the same instance instead of this:
                let prompt = Def::new("prompt");
                // prompt = __symbolFresh :StmtPatternRefuted;
                let mut new_stmts = vec![
                    Stmt::Def(Pattern::Lex(pos.clone(), prompt.clone()),
                              PrimCall(pos.clone(), PrimOp::SymbolFresh, vec![
                                  Const(pos.clone(),
                                        cst::Const::Symbol("StmtPatternRefuted".into()))]))
                ];

                for stmt in old_stmts {
                    expand_stmt_patterns(stmt, &prompt, &mut new_stmts);
                }

                Block(pos, new_stmts, Box::new(expr.expand_patterns()))
            }
            Match(pos, matchee, cases, default_case) => {
                let matchee_def = Def::new("mval");

                // mval = matchee;
                let setup = vec![Stmt::Def(Pattern::Lex(pos.clone(), matchee_def.clone()),
                                           matchee.expand_patterns())];

                let mut match_expr = default_case.expand_patterns();

                for case in cases.into_iter().rev() {
                    let prompt_name = "prompt";
                    let prompt = Def::new(prompt_name);

                    // [...code for case...]
                    let thunk = Function(pos.clone(), /* FIXME: */ vec![],
                                         Box::new(expand_case_patterns(case, &matchee_def,
                                                                             &prompt)));

                    // @fn ??? { match_expr }
                    let handler = Function(pos.clone(), /* FIXME: */ vec![], Box::new(match_expr));

                    // { prompt = __symbolFresh :prompt;
                    //   __prompt prompt thunk handler }
                    match_expr = Block(
                        pos.clone(),
                        vec![Stmt::Def(Pattern::Lex(pos.clone(), prompt.clone()),
                                       PrimCall(pos.clone(), PrimOp::SymbolFresh, vec![
                                           Const(pos.clone(),
                                                 cst::Const::Symbol(prompt_name.into()))
                                       ]))],
                        Box::new(PrimCall(pos.clone(), PrimOp::Prompt, vec![
                            Lex(pos.clone(), prompt), thunk, handler
                        ]))
                    )
                }

                Block(pos, setup, Box::new(match_expr))
            },
            Call(pos, callee, args) =>
                Call(pos, Box::new(callee.expand_patterns()),
                          args.into_iter().map(Expr::expand_patterns).collect()),
            PrimCall(pos, op, args) =>
                PrimCall(pos, op, args.into_iter().map(Expr::expand_patterns).collect()),
            Lex(..) | Const(..) => self,
            Dyn(..) => unreachable!()
        }
    }
}

fn expand_stmt_patterns(stmt: Stmt, prompt: &DefRef, stmts: &mut Vec<Stmt>) {
    match stmt {
        Stmt::Expr(expr) => stmts.push(Stmt::Expr(expr.expand_patterns())),
        Stmt::Def(pat @ Pattern::Lex(..), expr) =>
            stmts.push(Stmt::Def(pat, expr.expand_patterns())),
        Stmt::Def(pat, expr) => {
            // OPTIMIZE: If `expr` is trivial we don't need to bind it:
            let matchee = Def::new("mval");
            // mval = expr
            stmts.push(Stmt::Def(Pattern::Lex(expr.pos().clone(), matchee.clone()),
                                 expr.expand_patterns()));
            expand_pattern(pat, &matchee, prompt, stmts);
        }
    }
}

fn expand_case_patterns(case: Case, matchee: &DefRef, prompt: &DefRef) -> Expr {
    use self::PrimOp::AssertP;

    let csts = CstFactory::new(case.pos().clone());
    let Case { pattern, commit, guard, body } = case;

    let mut stmts = Vec::new();

    expand_pattern(pattern, matchee, prompt, &mut stmts);

    for stmt in commit { expand_stmt_patterns(stmt, /* HACK: */ prompt, &mut stmts) }

    match guard {
        Expr::Const(_, cst::Const::Bool(true)) => {},
        _ => // __assertP guard prompt
            stmts.push(csts.primcall(AssertP, vec![guard.expand_patterns(), csts.lex_use(prompt)])
                           .into())
    }

    csts.block(stmts, body.expand_patterns())
}

fn expand_pattern(pattern: Pattern, matchee: &DefRef, prompt: &DefRef, stmts: &mut Vec<Stmt>) {
    use self::Pattern::*;
    use self::PrimOp::{Tuple, TupleSlice, TupleLen};

    fn is_tree_pattern(pattern: &Pattern) -> bool {
        match *pattern {
            PrimCall(_, PrimOp::Tuple, _) | Lex(..) | Const(..) => true,
            PrimCall(..) => unimplemented!(),
            Call(..) => false,
            Dyn(..) => unreachable!()
        }
    }

    fn expand_tree_pattern(pattern: Pattern, matchee: &DefRef, prompt: &DefRef,
                           stmts: &mut Vec<Stmt>)
    {
        use self::PrimOp::{AssertP, Type, Eq, Tuple, TupleLen, TupleSlice};

        match pattern {
            PrimCall(pos, PrimOp::Tuple, args) => {
                let sub_matchees = Def::new("mseq");

                let csts = CstFactory::new(pos.clone());

                // __assertP (__eq (__type matchee) Tuple) prompt;
                stmts.push(csts.primcall(AssertP, vec![
                    csts.primcall(Eq, vec![
                        csts.primcall(Type, vec![csts.lex_use(matchee)]),
                        csts.lex_use(&Def::new("Tuple")) // HACK: Wrong `Tuple`
                    ]),
                    csts.lex_use(prompt)
                ]).into());
                // mseq = __tupleSlice matchee 0 (__tupleLen matchee);
                stmts.push(csts.def(csts.lex_def(&sub_matchees),
                                    csts.primcall(TupleSlice, vec![
                                        csts.lex_use(matchee), csts.constant(0),
                                        csts.primcall(TupleLen, vec![csts.lex_use(matchee)])
                                    ])));
                expand_pattern_row(pos, args, sub_matchees, prompt, stmts);
            },
            PrimCall(..) => unimplemented!(),
            Lex(..) => {},
            Const(pos, c) => {
                let csts = CstFactory::new(pos.clone());

                let apply = csts.lex_use(&Def::new("apply")); // HACK

                // __assertP (apply denv apply 0 (__tuple (==) (__tuple matchee c))) prompt
                stmts.push(
                    csts.primcall(AssertP, vec![
                        csts.call(apply.clone(), vec![
                            csts.lex_use(&Def::new("denv")), // HACK
                            apply, csts.constant(0),
                            csts.primcall(Tuple, vec![
                                csts.lex_use(&Def::new("==")), // HACK
                                csts.primcall(Tuple, vec![
                                    csts.lex_use(matchee), csts.constant(c)
                                ])
                            ])
                        ])
                    ]).into()
                );
            },
            Call(..) | Dyn(..) => unreachable!()
        }
    }

    fn expand_pattern_row<I>(pos: Pos, patterns: I, matchees: DefRef, prompt: &DefRef,
                             stmts: &mut Vec<Stmt>) where I: IntoIterator<Item=Pattern>
    {
        use self::PrimOp::{AssertP, Eq, SliceLen};

        fn uncons(pattern: &Pattern, matchees: &DefRef, prompt: &DefRef, stmts: &mut Vec<Stmt>)
            -> (DefRef, DefRef)
        {
            use self::PrimOp::{SliceGetP, SliceSubP, SliceLen};

            let (pos, matchee) = match *pattern {
                Const(ref pos, _) | PrimCall(ref pos, PrimOp::Tuple, _) =>
                    (pos, Def::new("mval")),
                PrimCall(..) => unimplemented!(),
                Lex(ref pos, ref def) => (pos, def.clone()),
                Call(..) | Dyn(..) => unreachable!()
            };

            let csts = CstFactory::new(pos.clone());

            let matchees = csts.lex_use(matchees);
            let prompt = csts.lex_use(prompt);

            // matchee = __sliceGetP matchees 0 prompt;
            stmts.push(csts.def(csts.lex_def(&matchee),
                                csts.primcall(SliceGetP, vec![
                                    matchees.clone(), csts.constant(0), prompt.clone()
                                ])));

            let rem_matchees = Def::new("mseq");
            // matchees' = __sliceSubP matchees 1 (__sliceLen matchees) prompt;
            stmts.push(csts.def(csts.lex_def(&rem_matchees),
                                csts.primcall(SliceSubP, vec![
                                    matchees.clone(), csts.constant(1),
                                    csts.primcall(SliceLen, vec![matchees]),
                                    prompt
                                ])));

            (matchee, rem_matchees)
        }

        fn expand_inseparable_pattern(pattern: Pattern, matchees: &DefRef, prompt: &DefRef,
                                      stmts: &mut Vec<Stmt>) -> DefRef
        {
            use self::PrimOp::{Tuple, TupleGet};

            match pattern {
                Call(pos, callee, args) => {
                    let csts = CstFactory::new(pos.clone());

                    let seqs = Def::new("seqs");
                    let denv = csts.lex_use(&Def::new("denv")); // HACK
                    let unapply = csts.lex_use(&Def::new("unapply")); // HACK

                    // mseq' = unapply denv unapply 0 (__tuple callee (__tuple mseq prompt))
                    stmts.push(csts.def(csts.lex_def(&seqs),
                                        csts.call(unapply.clone(), vec![
                                            denv, unapply, csts.constant(0),
                                            csts.primcall(Tuple, vec![
                                                callee.expand_patterns(),
                                                csts.primcall(Tuple, vec![
                                                    csts.lex_use(matchees), csts.lex_use(prompt)
                                                ])
                                            ])
                                        ])));

                    let sub_matchees = Def::new("mseq");
                    // subSeq = __tupleGet seqs 0
                    stmts.push(csts.def(csts.lex_def(&sub_matchees),
                                        csts.primcall(TupleGet, vec![
                                            csts.lex_use(&seqs), csts.constant(0)
                                        ])));

                    let rem_matchees = Def::new("mseq");
                    // seq' = __tupleGet seqs 1
                    stmts.push(csts.def(csts.lex_def(&rem_matchees),
                                        csts.primcall(TupleGet, vec![
                                            csts.lex_use(&seqs), csts.constant(1)
                                        ])));

                    expand_pattern_row(pos, args, sub_matchees, prompt, stmts);
                    rem_matchees
                },
                PrimCall(..) => unimplemented!(),
                Lex(..) | Dyn(..) | Const(..) => unreachable!()
            }
        }

        let matchees = patterns.into_iter().fold(matchees, |matchees, pattern|
            if is_tree_pattern(&pattern) {
                let (matchee, matchees) = uncons(&pattern, &matchees, prompt, stmts);
                expand_tree_pattern(pattern, &matchee, prompt, stmts);
                matchees
            } else {
                expand_inseparable_pattern(pattern, &matchees, prompt, stmts)
            }
        );

        let csts = CstFactory::new(pos);

        // __assertP (__eq (__sliceLen matchees) 0) prompt
        stmts.push(csts.primcall(AssertP, vec![
            csts.primcall(Eq, vec![
                csts.primcall(SliceLen, vec![csts.lex_use(&matchees)]),
                csts.constant(0)
            ]),
            csts.lex_use(prompt)
        ]).into());
    }

    if is_tree_pattern(&pattern) {
        expand_tree_pattern(pattern, matchee, &prompt, stmts);
    } else {
        let pos = pattern.pos().clone();
        let csts = CstFactory::new(pos.clone());
        let mtup = Def::new("mtup");
        let mseq = Def::new("mseq");

        // mtup = __tuple expr
        stmts.push(csts.def(csts.lex_def(&mtup),
                            csts.primcall(Tuple, vec![csts.lex_use(matchee)])));

        // mseq = __tupleSlice mtup 0 (__tupleLen mtup)
        stmts.push(csts.def(csts.lex_def(&mseq),
                            csts.primcall(TupleSlice, vec![
                                csts.lex_use(&mtup), csts.constant(0),
                                csts.primcall(TupleLen, vec![csts.lex_use(&mtup)])
                            ])));

        expand_pattern_row(pos, iter::once(pattern), mseq, &prompt, stmts);
    }
}
