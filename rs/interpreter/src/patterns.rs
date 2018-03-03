use std::iter;

use pcws_syntax::cst::{self, Program, Expr, Stmt, Case, Pattern, PrimOp, Def, DefRef, Use,
                       Pos, Positioned};

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
                            Lex(pos.clone(), Use::new(prompt)), thunk, handler
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
        Stmt::Def(pat, expr) => {
            // OPTIMIZE: If `expr` is trivial we don't need to bind it:
            let matchee = Def::new("mval");
            // mval = expr
            stmts.push(Stmt::Def(Pattern::Lex(expr.pos().clone(), matchee.clone()),
                                 expr.expand_patterns()));
            expand_pattern(pat, &matchee, prompt, stmts);
        },
        Stmt::Expr(expr) => stmts.push(Stmt::Expr(expr.expand_patterns()))
    }
}

fn expand_case_patterns(case: Case, matchee: &DefRef, prompt: &DefRef) -> Expr {
    let pos = case.pos().clone();
    let Case { pattern, commit, guard, body } = case;

    let mut stmts = Vec::new();

    expand_pattern(pattern, matchee, prompt, &mut stmts);

    for stmt in commit { expand_stmt_patterns(stmt, /* HACK: */ prompt, &mut stmts) }

    match guard {
        Expr::Const(_, cst::Const::Bool(true)) => {},
        _ => {
            // __assertP guard prompt
            stmts.push(Stmt::Expr(Expr::PrimCall(pos.clone(), PrimOp::AssertP, vec![
                guard.expand_patterns(),
                Expr::Lex(pos.clone(), Use::new(prompt.clone()))
            ])));
        }
    }

    Expr::Block(pos, stmts, Box::new(body.expand_patterns()))
}

fn expand_pattern(pattern: Pattern, matchee: &DefRef, prompt: &DefRef, stmts: &mut Vec<Stmt>) {
    use self::Pattern::*;

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
        match pattern {
            PrimCall(pos, PrimOp::Tuple, args) => {
                let sub_matchees = Def::new("mseq");

                // __assertP (__eq (__type matchee) Tuple) prompt;
                stmts.push(Stmt::Expr(Expr::PrimCall(pos.clone(), PrimOp::AssertP, vec![
                    Expr::PrimCall(pos.clone(), PrimOp::Eq, vec![
                        Expr::PrimCall(pos.clone(), PrimOp::Type, vec![
                            Expr::Lex(pos.clone(), Use::new(matchee.clone()))
                        ]),
                        Expr::Lex(pos.clone(), Use::new(Def::new("Tuple"))) // HACK: Wrong `Tuple`
                    ]),
                    Expr::Lex(pos.clone(), Use::new(prompt.clone()))
                ])));
                // mseq = __tupleSlice matchee 0 (__tupleLen matchee);
                stmts.push(Stmt::Def(Lex(pos.clone(), sub_matchees.clone()),
                                     Expr::PrimCall(pos.clone(), PrimOp::TupleSlice, vec![
                                         Expr::Lex(pos.clone(), Use::new(matchee.clone())),
                                         Expr::Const(pos.clone(), cst::Const::Int(0)),
                                        Expr::PrimCall(pos.clone(), PrimOp::TupleLen, vec![
                                            Expr::Lex(pos.clone(), Use::new(matchee.clone()))
                                        ])
                                     ])));
                expand_pattern_row(pos, args, sub_matchees, prompt, stmts);
            },
            PrimCall(..) => unimplemented!(),
            Lex(..) => {},
            Const(pos, c) => {
                let apply = Def::new("apply"); // HACK
                let denv = Def::new("denv"); // HACK
                let eq = Def::new("=="); // HACK

                // __assertP (apply denv apply 0 (__tuple (==) (__tuple matchee c))) prompt
                stmts.push(
                    Stmt::Expr(Expr::PrimCall(pos.clone(), PrimOp::AssertP, vec![
                        Expr::Call(pos.clone(),
                            Box::new(Expr::Lex(pos.clone(), Use::new(apply.clone()))),
                            vec![
                                Expr::Lex(pos.clone(), Use::new(denv)),
                                Expr::Lex(pos.clone(), Use::new(apply)),
                                Expr::Const(pos.clone(), cst::Const::Int(0)),
                                Expr::PrimCall(pos.clone(), PrimOp::Tuple, vec![
                                    Expr::Lex(pos.clone(), Use::new(eq)),
                                    Expr::PrimCall(pos.clone(), PrimOp::Tuple, vec![
                                        Expr::Lex(pos.clone(), Use::new(matchee.clone())),
                                        Expr::Const(pos, c)])])])]))
                );
            },
            Call(..) | Dyn(..) => unreachable!()
        }
    }

    fn expand_pattern_row<I>(pos: Pos, patterns: I, matchees: DefRef, prompt: &DefRef,
                             stmts: &mut Vec<Stmt>) where I: IntoIterator<Item=Pattern>
    {
        fn uncons(pattern: &Pattern, matchees: &DefRef, prompt: &DefRef, stmts: &mut Vec<Stmt>)
            -> (DefRef, DefRef)
        {
            let (pos, matchee) = match *pattern {
                Const(ref pos, _) | PrimCall(ref pos, PrimOp::Tuple, _) =>
                    (pos, Def::new("mval")),
                PrimCall(..) => unimplemented!(),
                Lex(ref pos, ref def) => (pos, def.clone()),
                Call(..) | Dyn(..) => unreachable!()
            };

            let matchees = Expr::Lex(pos.clone(), Use::new(matchees.clone()));
            let prompt = Expr::Lex(pos.clone(), Use::new(prompt.clone()));

            // matchee = __sliceGetP matchees 0 prompt;
            stmts.push(Stmt::Def(Lex(pos.clone(), matchee.clone()),
                                 Expr::PrimCall(pos.clone(), PrimOp::SliceGetP, vec![
                                     matchees.clone(),
                                     Expr::Const(pos.clone(), cst::Const::Int(0)),
                                     prompt.clone()
                                 ])));

            let rem_matchees = Def::new("mseq");
            // matchees' = __sliceSubP matchees 1 (__sliceLen matchees) prompt;
            stmts.push(Stmt::Def(Lex(pos.clone(), rem_matchees.clone()),
                                 Expr::PrimCall(pos.clone(), PrimOp::SliceSubP, vec![
                                     matchees.clone(),
                                     Expr::Const(pos.clone(), cst::Const::Int(1)),
                                     Expr::PrimCall(pos.clone(), PrimOp::SliceLen, vec![
                                        matchees
                                     ]),
                                     prompt
                                 ])));

            (matchee, rem_matchees)
        }

        fn expand_inseparable_pattern(pattern: Pattern, matchees: &DefRef, prompt: &DefRef,
                                      stmts: &mut Vec<Stmt>) -> DefRef
        {
            match pattern {
                Call(pos, callee, args) => {
                    let seqs = Def::new("seqs");
                    let denv = Expr::Lex(pos.clone(), Use::new(Def::new("denv"))); // HACK
                    let unapply = Expr::Lex(pos.clone(), Use::new(Def::new("unapply"))); // HACK

                    // mseq' = unapply denv unapply 0 (__tuple callee (__tuple mseq prompt))
                    stmts.push(
                        Stmt::Def(Lex(pos.clone(), seqs.clone()),
                                  Expr::Call(pos.clone(), Box::new(unapply.clone()), vec![
                                      denv,
                                      unapply,
                                      Expr::Const(pos.clone(), cst::Const::Int(0)),
                                      Expr::PrimCall(pos.clone(), PrimOp::Tuple, vec![
                                          callee.expand_patterns(),
                                          Expr::PrimCall(pos.clone(), PrimOp::Tuple, vec![
                                              Expr::Lex(pos.clone(), Use::new(matchees.clone())),
                                              Expr::Lex(pos.clone(), Use::new(prompt.clone()))
                                          ])
                                      ])
                                  ]))
                    );

                    let sub_matchees = Def::new("mseq");
                    // subSeq = __tupleGet seqs 0
                    stmts.push(
                        Stmt::Def(Lex(pos.clone(), sub_matchees.clone()),
                                  Expr::PrimCall(pos.clone(), PrimOp::TupleGet, vec![
                                      Expr::Lex(pos.clone(), Use::new(seqs.clone())),
                                      Expr::Const(pos.clone(), cst::Const::Int(0))
                                  ]))
                    );

                    let rem_matchees = Def::new("mseq");
                    // seq' = __tupleGet seqs 1
                    stmts.push(
                        Stmt::Def(Lex(pos.clone(), rem_matchees.clone()),
                                  Expr::PrimCall(pos.clone(), PrimOp::TupleGet, vec![
                                      Expr::Lex(pos.clone(), Use::new(seqs)),
                                      Expr::Const(pos.clone(), cst::Const::Int(1))
                                  ]))
                    );

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

        // __assertP (__eq (__sliceLen matchees) 0) prompt
        stmts.push(Stmt::Expr(Expr::PrimCall(pos.clone(), PrimOp::AssertP, vec![
            Expr::PrimCall(pos.clone(), PrimOp::Eq, vec![
                Expr::PrimCall(pos.clone(), PrimOp::SliceLen, vec![
                    Expr::Lex(pos.clone(), Use::new(matchees))
                ]),
                Expr::Const(pos.clone(), cst::Const::Int(0))
            ]),
            Expr::Lex(pos, Use::new(prompt.clone()))
        ])));
    }

    if is_tree_pattern(&pattern) {
        expand_tree_pattern(pattern, matchee, &prompt, stmts);
    } else {
        let pos = pattern.pos().clone();
        let mtup = Def::new("mtup");
        let mseq = Def::new("mseq");
        
        // mtup = __tuple expr
        stmts.push(Stmt::Def(Pattern::Lex(pos.clone(), mtup.clone()),
                             Expr::PrimCall(pos.clone(), PrimOp::Tuple, vec![
                                Expr::Lex(pos.clone(), Use::new(matchee.clone()))
                             ])));

        // mseq = __tupleSlice mtup 0 (__tupleLen mtup)
        stmts.push(
            Stmt::Def(Pattern::Lex(pos.clone(), mseq.clone()),
                      Expr::PrimCall(pos.clone(), PrimOp::TupleSlice, vec![
                          Expr::Lex(pos.clone(), Use::new(mtup.clone())),
                          Expr::Const(pos.clone(), cst::Const::Int(0)),
                          Expr::PrimCall(pos.clone(), PrimOp::TupleLen, vec![
                              Expr::Lex(pos.clone(), Use::new(mtup))
                          ])
                      ])));

        expand_pattern_row(pos, iter::once(pattern), mseq, &prompt, stmts);
    }
}
