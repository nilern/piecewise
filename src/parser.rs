use lexer::{Tok, LexicalError, SrcPos};
use ast::AST;
use std::str::FromStr;
extern crate lalrpop_util as __lalrpop_util;

mod __parse__Expr {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use lexer::{Tok, LexicalError, SrcPos};
    use ast::AST;
    use std::str::FromStr;
    extern crate lalrpop_util as __lalrpop_util;
    use super::__ToTriple;
    #[allow(dead_code)]
    pub enum __Symbol<> {
        Term_22_28_22(Tok),
        Term_22_29_22(Tok),
        Term_22_2c_22(Tok),
        Term_22_3b_22(Tok),
        Term_22_3d_22(Tok),
        Term_22_5b_22(Tok),
        Term_22_5d_22(Tok),
        Term_22_7b_22(Tok),
        Term_22_7d_22(Tok),
        TermName(String),
        TermNumber(String),
        TermOp(String),
        Termerror(__lalrpop_util::ErrorRecovery<SrcPos, Tok, LexicalError>),
        Nt_28_3cExpr_3e_20_22_2c_22_29(AST),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2b(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29(AST),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2b(::std::vec::Vec<AST>),
        NtApp(AST),
        NtAtom(AST),
        NtColl(AST),
        NtComma_3cExpr_3e(Vec<AST>),
        NtCommaT_3cExpr_3e(Vec<AST>),
        NtExpr(AST),
        NtExpr_3f(::std::option::Option<AST>),
        NtExprs(AST),
        NtInfix(AST),
        NtSemiColon_3cStmt_3e(Vec<AST>),
        NtSimple(AST),
        NtStmt(AST),
        NtStmt_3f(::std::option::Option<AST>),
        Nt____Expr(AST),
        Nt____Exprs(AST),
    }
    const __ACTION: &'static [i32] = &[
        // State 0
        9, 0, 0, 0, 0, 10, 0, 11, 0, 12, 13, 0, 0,
        // State 1
        9, -28, -28, -28, -28, 10, -28, 11, -28, 12, 13, -28, 0,
        // State 2
        -36, -36, -36, -36, -36, -36, -36, -36, -36, -36, -36, -36, 0,
        // State 3
        -35, -35, -35, -35, -35, -35, -35, -35, -35, -35, -35, -35, 0,
        // State 4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 5
        0, -38, -38, -38, 15, 0, -38, 0, -38, 0, 0, 16, 0,
        // State 6
        -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, 0,
        // State 7
        0, -23, -23, 0, 0, 0, -23, 0, 0, 0, 0, 0, 0,
        // State 8
        9, 0, 20, 0, 0, 10, 0, 11, 0, 12, 13, 0, 0,
        // State 9
        9, 0, 0, 0, 0, 10, -18, 11, 0, 12, 13, 0, 0,
        // State 10
        9, 0, 0, 0, 0, 10, 0, 11, -30, 12, 13, 0, 0,
        // State 11
        -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, 0,
        // State 12
        -14, -14, -14, -14, -14, -14, -14, -14, -14, -14, -14, -14, 0,
        // State 13
        -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, 0,
        // State 14
        9, 0, 0, 0, 0, 10, 0, 11, 0, 12, 13, 0, 0,
        // State 15
        9, 0, 0, 0, 0, 10, 0, 11, 0, 12, 13, 0, 0,
        // State 16
        9, -21, 0, 0, 0, 10, 0, 11, 0, 12, 13, 0, 0,
        // State 17
        0, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 18
        0, 32, 33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 19
        0, -22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 20
        9, 0, 0, 0, 0, 10, -20, 11, 0, 12, 13, 0, 0,
        // State 21
        0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 0,
        // State 22
        0, 0, 33, 0, 0, 0, -17, 0, 0, 0, 0, 0, 0,
        // State 23
        9, 0, 0, 0, 0, 10, 0, 11, -32, 12, 13, 0, 0,
        // State 24
        0, 0, 0, 0, 0, 0, 0, 0, 37, 0, 0, 0, 0,
        // State 25
        0, 0, 0, 0, 0, 0, 0, 0, -26, 0, 0, 0, 0,
        // State 26
        0, 0, 0, 38, 0, 0, 0, 0, -29, 0, 0, 0, 0,
        // State 27
        0, -37, -37, -37, 0, 0, -37, 0, -37, 0, 0, 16, 0,
        // State 28
        9, -27, -27, -27, -27, 10, -27, 11, -27, 12, 13, -27, 0,
        // State 29
        0, 0, 39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 30
        -15, -15, -15, -15, -15, -15, -15, -15, -15, -15, -15, -15, 0,
        // State 31
        -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, 0,
        // State 32
        -4, -4, 0, 0, 0, -4, -4, -4, 0, -4, -4, 0, 0,
        // State 33
        0, 0, 39, 0, 0, 0, -19, 0, 0, 0, 0, 0, 0,
        // State 34
        -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, 0,
        // State 35
        0, 0, 0, 40, 0, 0, 0, 0, -31, 0, 0, 0, 0,
        // State 36
        -33, -33, -33, -33, -33, -33, -33, -33, -33, -33, -33, -33, 0,
        // State 37
        -9, 0, 0, 0, 0, -9, 0, -9, -9, -9, -9, 0, 0,
        // State 38
        -5, -5, 0, 0, 0, -5, -5, -5, 0, -5, -5, 0, 0,
        // State 39
        -10, 0, 0, 0, 0, -10, 0, -10, -10, -10, -10, 0, 0,
    ];
    const __EOF_ACTION: &'static [i32] = &[
        0,
        -28,
        -36,
        -35,
        -41,
        -38,
        -12,
        -23,
        0,
        0,
        0,
        -13,
        -14,
        -11,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        -37,
        -27,
        0,
        -15,
        -34,
        0,
        0,
        -16,
        0,
        -33,
        0,
        0,
        0,
    ];
    const __GOTO: &'static [i32] = &[
        // State 0
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 0, 5, 0, 0, 6, 0, 7, 8, 0, 0, 0,
        // State 1
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0, 0,
        // State 2
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 3
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 5
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 6
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 7
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 8
        0, 0, 17, 0, 0, 0, 2, 3, 4, 0, 18, 19, 0, 0, 6, 0, 7, 8, 0, 0, 0,
        // State 9
        0, 0, 21, 0, 0, 0, 2, 3, 4, 22, 0, 23, 0, 0, 6, 0, 7, 8, 0, 0, 0,
        // State 10
        0, 0, 0, 0, 0, 24, 2, 3, 4, 0, 0, 0, 0, 25, 6, 26, 7, 27, 0, 0, 0,
        // State 11
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 12
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 13
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 14
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 0, 0, 0, 0, 28, 0, 7, 0, 0, 0, 0,
        // State 15
        0, 0, 0, 0, 0, 0, 29, 3, 4, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0,
        // State 16
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 0, 30, 0, 0, 6, 0, 7, 8, 0, 0, 0,
        // State 17
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 18
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 19
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 20
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 0, 34, 0, 0, 6, 0, 7, 8, 0, 0, 0,
        // State 21
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 22
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 23
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 0, 0, 0, 0, 6, 0, 7, 36, 0, 0, 0,
        // State 24
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 25
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 26
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 27
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 28
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 0, 0, 0, 0, 0, 0, 14, 0, 0, 0, 0,
        // State 29
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 30
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 31
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 32
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 33
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 34
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 35
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 36
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 37
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 38
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 39
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    pub fn parse_Expr<
        __TOKEN: __ToTriple<Error=LexicalError>,
        __TOKENS: IntoIterator<Item=__TOKEN>,
    >(
        __tokens0: __TOKENS,
    ) -> Result<AST, __lalrpop_util::ParseError<SrcPos, Tok, LexicalError>>
    {
        let __tokens = __tokens0.into_iter();
        let mut __tokens = __tokens.map(|t| __ToTriple::to_triple(t));
        let mut __states = vec![0_i32];
        let mut __symbols = vec![];
        let mut __integer;
        let mut __lookahead;
        let mut __last_location = Default::default();
        '__shift: loop {
            __lookahead = match __tokens.next() {
                Some(Ok(v)) => v,
                None => break '__shift,
                Some(Err(e)) => return Err(__lalrpop_util::ParseError::User { error: e }),
            };
            __last_location = __lookahead.2.clone();
            __integer = match __lookahead.1 {
                Tok::LParen if true => 0,
                Tok::RParen if true => 1,
                Tok::Comma if true => 2,
                Tok::Semicolon if true => 3,
                Tok::Eq if true => 4,
                Tok::LBracket if true => 5,
                Tok::RBracket if true => 6,
                Tok::LBrace if true => 7,
                Tok::RBrace if true => 8,
                Tok::Name(_) if true => 9,
                Tok::Number(_) if true => 10,
                Tok::Op(_) if true => 11,
                _ => {
                    return Err(__lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: vec![],
                    });
                }
            };
            '__inner: loop {
                let __state = *__states.last().unwrap() as usize;
                let __action = __ACTION[__state * 13 + __integer];
                if __action > 0 {
                    let __symbol = match __integer {
                        0 => match __lookahead.1 {
                            __tok @ Tok::LParen => __Symbol::Term_22_28_22(__tok),
                            _ => unreachable!(),
                        },
                        1 => match __lookahead.1 {
                            __tok @ Tok::RParen => __Symbol::Term_22_29_22(__tok),
                            _ => unreachable!(),
                        },
                        2 => match __lookahead.1 {
                            __tok @ Tok::Comma => __Symbol::Term_22_2c_22(__tok),
                            _ => unreachable!(),
                        },
                        3 => match __lookahead.1 {
                            __tok @ Tok::Semicolon => __Symbol::Term_22_3b_22(__tok),
                            _ => unreachable!(),
                        },
                        4 => match __lookahead.1 {
                            __tok @ Tok::Eq => __Symbol::Term_22_3d_22(__tok),
                            _ => unreachable!(),
                        },
                        5 => match __lookahead.1 {
                            __tok @ Tok::LBracket => __Symbol::Term_22_5b_22(__tok),
                            _ => unreachable!(),
                        },
                        6 => match __lookahead.1 {
                            __tok @ Tok::RBracket => __Symbol::Term_22_5d_22(__tok),
                            _ => unreachable!(),
                        },
                        7 => match __lookahead.1 {
                            __tok @ Tok::LBrace => __Symbol::Term_22_7b_22(__tok),
                            _ => unreachable!(),
                        },
                        8 => match __lookahead.1 {
                            __tok @ Tok::RBrace => __Symbol::Term_22_7d_22(__tok),
                            _ => unreachable!(),
                        },
                        9 => match __lookahead.1 {
                            Tok::Name(__tok0) => __Symbol::TermName(__tok0),
                            _ => unreachable!(),
                        },
                        10 => match __lookahead.1 {
                            Tok::Number(__tok0) => __Symbol::TermNumber(__tok0),
                            _ => unreachable!(),
                        },
                        11 => match __lookahead.1 {
                            Tok::Op(__tok0) => __Symbol::TermOp(__tok0),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    __states.push(__action - 1);
                    __symbols.push((__lookahead.0, __symbol, __lookahead.2));
                    continue '__shift;
                } else if __action < 0 {
                    if let Some(r) = __reduce(__action, Some(&__lookahead.0), &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                        return r;
                    }
                } else {
                    return Err(__lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: vec![],
                    });
                }
            }
        }
        loop {
            let __state = *__states.last().unwrap() as usize;
            let __action = __EOF_ACTION[__state];
            if __action < 0 {
                if let Some(r) = __reduce(__action, None, &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                    return r;
                }
            } else {
                let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                    token: None,
                    expected: vec![],
                };
                return Err(__error);
            }
        }
    }
    pub fn __reduce<
    >(
        __action: i32,
        __lookahead_start: Option<&SrcPos>,
        __states: &mut ::std::vec::Vec<i32>,
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>,
        _: ::std::marker::PhantomData<()>,
    ) -> Option<Result<AST,__lalrpop_util::ParseError<SrcPos, Tok, LexicalError>>>
    {
        let __nonterminal = match -__action {
            1 => {
                // (<Expr> ",") = Expr, "," => ActionFn(33);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action33::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__nt), __end));
                0
            }
            2 => {
                // (<Expr> ",")* =  => ActionFn(31);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action31::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            3 => {
                // (<Expr> ",")* = (<Expr> ",")+ => ActionFn(32);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action32::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            4 => {
                // (<Expr> ",")+ = Expr, "," => ActionFn(36);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action36::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            5 => {
                // (<Expr> ",")+ = (<Expr> ",")+, Expr, "," => ActionFn(37);
                let __sym2 = __pop_Term_22_2c_22(__symbols);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action37::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            6 => {
                // (<Stmt> ";") = Stmt, ";" => ActionFn(26);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action26::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__nt), __end));
                3
            }
            7 => {
                // (<Stmt> ";")* =  => ActionFn(24);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action24::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            8 => {
                // (<Stmt> ";")* = (<Stmt> ";")+ => ActionFn(25);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action25::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            9 => {
                // (<Stmt> ";")+ = Stmt, ";" => ActionFn(40);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action40::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            10 => {
                // (<Stmt> ";")+ = (<Stmt> ";")+, Stmt, ";" => ActionFn(41);
                let __sym2 = __pop_Term_22_3b_22(__symbols);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action41::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            11 => {
                // App = App, Simple => ActionFn(8);
                let __sym1 = __pop_NtSimple(__symbols);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action8::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtApp(__nt), __end));
                6
            }
            12 => {
                // App = Simple => ActionFn(9);
                let __sym0 = __pop_NtSimple(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtApp(__nt), __end));
                6
            }
            13 => {
                // Atom = Name => ActionFn(16);
                let __sym0 = __pop_TermName(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action16::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtAtom(__nt), __end));
                7
            }
            14 => {
                // Atom = Number => ActionFn(17);
                let __sym0 = __pop_TermNumber(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action17::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtAtom(__nt), __end));
                7
            }
            15 => {
                // Coll = "(", CommaT<Expr>, ")" => ActionFn(14);
                let __sym2 = __pop_Term_22_29_22(__symbols);
                let __sym1 = __pop_NtCommaT_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_28_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action14::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                8
            }
            16 => {
                // Coll = "[", Comma<Expr>, "]" => ActionFn(15);
                let __sym2 = __pop_Term_22_5d_22(__symbols);
                let __sym1 = __pop_NtComma_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_5b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action15::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                8
            }
            17 => {
                // Comma<Expr> = Expr => ActionFn(44);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action44::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            18 => {
                // Comma<Expr> =  => ActionFn(45);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action45::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            19 => {
                // Comma<Expr> = (<Expr> ",")+, Expr => ActionFn(46);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action46::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            20 => {
                // Comma<Expr> = (<Expr> ",")+ => ActionFn(47);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action47::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            21 => {
                // CommaT<Expr> = (<Expr> ",")+ => ActionFn(19);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action19::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtCommaT_3cExpr_3e(__nt), __end));
                10
            }
            22 => {
                // CommaT<Expr> = "," => ActionFn(20);
                let __sym0 = __pop_Term_22_2c_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action20::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtCommaT_3cExpr_3e(__nt), __end));
                10
            }
            23 => {
                // Expr = Stmt => ActionFn(3);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr(__nt), __end));
                11
            }
            24 => {
                // Expr? = Expr => ActionFn(29);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action29::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                12
            }
            25 => {
                // Expr? =  => ActionFn(30);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action30::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                12
            }
            26 => {
                // Exprs = SemiColon<Stmt> => ActionFn(2);
                let __sym0 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExprs(__nt), __end));
                13
            }
            27 => {
                // Infix = Infix, Op, App => ActionFn(6);
                let __sym2 = __pop_NtApp(__symbols);
                let __sym1 = __pop_TermOp(__symbols);
                let __sym0 = __pop_NtInfix(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action6::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtInfix(__nt), __end));
                14
            }
            28 => {
                // Infix = App => ActionFn(7);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action7::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtInfix(__nt), __end));
                14
            }
            29 => {
                // SemiColon<Stmt> = Stmt => ActionFn(48);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action48::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            30 => {
                // SemiColon<Stmt> =  => ActionFn(49);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action49::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            31 => {
                // SemiColon<Stmt> = (<Stmt> ";")+, Stmt => ActionFn(50);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action50::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            32 => {
                // SemiColon<Stmt> = (<Stmt> ";")+ => ActionFn(51);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action51::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            33 => {
                // Simple = "{", Exprs, "}" => ActionFn(10);
                let __sym2 = __pop_Term_22_7d_22(__symbols);
                let __sym1 = __pop_NtExprs(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action10::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            34 => {
                // Simple = "(", Expr, ")" => ActionFn(11);
                let __sym2 = __pop_Term_22_29_22(__symbols);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Term_22_28_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action11::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            35 => {
                // Simple = Coll => ActionFn(12);
                let __sym0 = __pop_NtColl(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action12::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            36 => {
                // Simple = Atom => ActionFn(13);
                let __sym0 = __pop_NtAtom(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            37 => {
                // Stmt = Infix, "=", Infix => ActionFn(4);
                let __sym2 = __pop_NtInfix(__symbols);
                let __sym1 = __pop_Term_22_3d_22(__symbols);
                let __sym0 = __pop_NtInfix(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action4::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                17
            }
            38 => {
                // Stmt = Infix => ActionFn(5);
                let __sym0 = __pop_NtInfix(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action5::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                17
            }
            39 => {
                // Stmt? = Stmt => ActionFn(22);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action22::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                18
            }
            40 => {
                // Stmt? =  => ActionFn(23);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action23::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                18
            }
            41 => {
                // __Expr = Expr => ActionFn(1);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action1::<>(__sym0);
                return Some(Ok(__nt));
            }
            42 => {
                // __Exprs = Exprs => ActionFn(0);
                let __sym0 = __pop_NtExprs(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt____Exprs(__nt), __end));
                20
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __state = *__states.last().unwrap() as usize;
        let __next_state = __GOTO[__state * 21 + __nonterminal] - 1;
        __states.push(__next_state);
        None
    }
    fn __pop_Term_22_28_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_28_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_29_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_29_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_2c_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_2c_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermName<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, String, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermName(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermNumber<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, String, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermNumber(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermOp<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, String, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermOp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Termerror<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, __lalrpop_util::ErrorRecovery<SrcPos, Tok, LexicalError>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Termerror(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtApp<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtApp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtAtom<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtAtom(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtColl<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtColl(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtComma_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtComma_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtCommaT_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtCommaT_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::option::Option<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExprs<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExprs(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtInfix<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtInfix(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtSemiColon_3cStmt_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtSemiColon_3cStmt_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtSimple<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtSimple(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::option::Option<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Expr<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Expr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Exprs<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Exprs(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
}
pub use self::__parse__Expr::parse_Expr;

mod __parse__Exprs {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use lexer::{Tok, LexicalError, SrcPos};
    use ast::AST;
    use std::str::FromStr;
    extern crate lalrpop_util as __lalrpop_util;
    use super::__ToTriple;
    #[allow(dead_code)]
    pub enum __Symbol<> {
        Term_22_28_22(Tok),
        Term_22_29_22(Tok),
        Term_22_2c_22(Tok),
        Term_22_3b_22(Tok),
        Term_22_3d_22(Tok),
        Term_22_5b_22(Tok),
        Term_22_5d_22(Tok),
        Term_22_7b_22(Tok),
        Term_22_7d_22(Tok),
        TermName(String),
        TermNumber(String),
        TermOp(String),
        Termerror(__lalrpop_util::ErrorRecovery<SrcPos, Tok, LexicalError>),
        Nt_28_3cExpr_3e_20_22_2c_22_29(AST),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2b(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29(AST),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2b(::std::vec::Vec<AST>),
        NtApp(AST),
        NtAtom(AST),
        NtColl(AST),
        NtComma_3cExpr_3e(Vec<AST>),
        NtCommaT_3cExpr_3e(Vec<AST>),
        NtExpr(AST),
        NtExpr_3f(::std::option::Option<AST>),
        NtExprs(AST),
        NtInfix(AST),
        NtSemiColon_3cStmt_3e(Vec<AST>),
        NtSimple(AST),
        NtStmt(AST),
        NtStmt_3f(::std::option::Option<AST>),
        Nt____Expr(AST),
        Nt____Exprs(AST),
    }
    const __ACTION: &'static [i32] = &[
        // State 0
        11, 0, 0, 0, 0, 12, 0, 13, 0, 14, 15, 0, 0,
        // State 1
        11, 0, 0, 0, 0, 12, 0, 13, -32, 14, 15, 0, 0,
        // State 2
        11, -28, -28, -28, -28, 12, -28, 13, -28, 14, 15, -28, 0,
        // State 3
        -36, -36, -36, -36, -36, -36, -36, -36, -36, -36, -36, -36, 0,
        // State 4
        -35, -35, -35, -35, -35, -35, -35, -35, -35, -35, -35, -35, 0,
        // State 5
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 6
        0, -38, -38, -38, 18, 0, -38, 0, -38, 0, 0, 19, 0,
        // State 7
        0, 0, 0, 0, 0, 0, 0, 0, -26, 0, 0, 0, 0,
        // State 8
        -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, 0,
        // State 9
        0, 0, 0, 20, 0, 0, 0, 0, -29, 0, 0, 0, 0,
        // State 10
        11, 0, 25, 0, 0, 12, 0, 13, 0, 14, 15, 0, 0,
        // State 11
        11, 0, 0, 0, 0, 12, -18, 13, 0, 14, 15, 0, 0,
        // State 12
        11, 0, 0, 0, 0, 12, 0, 13, -30, 14, 15, 0, 0,
        // State 13
        -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, 0,
        // State 14
        -14, -14, -14, -14, -14, -14, -14, -14, -14, -14, -14, -14, 0,
        // State 15
        0, 0, 0, 30, 0, 0, 0, 0, -31, 0, 0, 0, 0,
        // State 16
        -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, 0,
        // State 17
        11, 0, 0, 0, 0, 12, 0, 13, 0, 14, 15, 0, 0,
        // State 18
        11, 0, 0, 0, 0, 12, 0, 13, 0, 14, 15, 0, 0,
        // State 19
        -9, 0, 0, 0, 0, -9, 0, -9, -9, -9, -9, 0, 0,
        // State 20
        11, -21, 0, 0, 0, 12, 0, 13, 0, 14, 15, 0, 0,
        // State 21
        0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 22
        0, 35, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 23
        0, -23, -23, 0, 0, 0, -23, 0, 0, 0, 0, 0, 0,
        // State 24
        0, -22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 25
        11, 0, 0, 0, 0, 12, -20, 13, 0, 14, 15, 0, 0,
        // State 26
        0, 0, 0, 0, 0, 0, 38, 0, 0, 0, 0, 0, 0,
        // State 27
        0, 0, 36, 0, 0, 0, -17, 0, 0, 0, 0, 0, 0,
        // State 28
        0, 0, 0, 0, 0, 0, 0, 0, 39, 0, 0, 0, 0,
        // State 29
        -10, 0, 0, 0, 0, -10, 0, -10, -10, -10, -10, 0, 0,
        // State 30
        0, -37, -37, -37, 0, 0, -37, 0, -37, 0, 0, 19, 0,
        // State 31
        11, -27, -27, -27, -27, 12, -27, 13, -27, 14, 15, -27, 0,
        // State 32
        0, 0, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 33
        -15, -15, -15, -15, -15, -15, -15, -15, -15, -15, -15, -15, 0,
        // State 34
        -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, -34, 0,
        // State 35
        -4, -4, 0, 0, 0, -4, -4, -4, 0, -4, -4, 0, 0,
        // State 36
        0, 0, 40, 0, 0, 0, -19, 0, 0, 0, 0, 0, 0,
        // State 37
        -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, 0,
        // State 38
        -33, -33, -33, -33, -33, -33, -33, -33, -33, -33, -33, -33, 0,
        // State 39
        -5, -5, 0, 0, 0, -5, -5, -5, 0, -5, -5, 0, 0,
    ];
    const __EOF_ACTION: &'static [i32] = &[
        -30,
        -32,
        -28,
        -36,
        -35,
        -42,
        -38,
        -26,
        -12,
        -29,
        0,
        0,
        0,
        -13,
        -14,
        -31,
        -11,
        0,
        0,
        -9,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        -10,
        -37,
        -27,
        0,
        -15,
        -34,
        0,
        0,
        -16,
        -33,
        0,
    ];
    const __GOTO: &'static [i32] = &[
        // State 0
        0, 0, 0, 0, 0, 2, 3, 4, 5, 0, 0, 0, 0, 6, 7, 8, 9, 10, 0, 0, 0,
        // State 1
        0, 0, 0, 0, 0, 0, 3, 4, 5, 0, 0, 0, 0, 0, 7, 0, 9, 16, 0, 0, 0,
        // State 2
        0, 0, 0, 0, 0, 0, 0, 4, 5, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0,
        // State 3
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 5
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 6
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 7
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 8
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 9
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 10
        0, 0, 21, 0, 0, 0, 3, 4, 5, 0, 22, 23, 0, 0, 7, 0, 9, 24, 0, 0, 0,
        // State 11
        0, 0, 26, 0, 0, 0, 3, 4, 5, 27, 0, 28, 0, 0, 7, 0, 9, 24, 0, 0, 0,
        // State 12
        0, 0, 0, 0, 0, 2, 3, 4, 5, 0, 0, 0, 0, 29, 7, 8, 9, 10, 0, 0, 0,
        // State 13
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 14
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 15
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 16
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 17
        0, 0, 0, 0, 0, 0, 3, 4, 5, 0, 0, 0, 0, 0, 31, 0, 9, 0, 0, 0, 0,
        // State 18
        0, 0, 0, 0, 0, 0, 32, 4, 5, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0,
        // State 19
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 20
        0, 0, 0, 0, 0, 0, 3, 4, 5, 0, 0, 33, 0, 0, 7, 0, 9, 24, 0, 0, 0,
        // State 21
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 22
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 23
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 24
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 25
        0, 0, 0, 0, 0, 0, 3, 4, 5, 0, 0, 37, 0, 0, 7, 0, 9, 24, 0, 0, 0,
        // State 26
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 27
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 28
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 29
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 30
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 31
        0, 0, 0, 0, 0, 0, 0, 4, 5, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 0,
        // State 32
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 33
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 34
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 35
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 36
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 37
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 38
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 39
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    pub fn parse_Exprs<
        __TOKEN: __ToTriple<Error=LexicalError>,
        __TOKENS: IntoIterator<Item=__TOKEN>,
    >(
        __tokens0: __TOKENS,
    ) -> Result<AST, __lalrpop_util::ParseError<SrcPos, Tok, LexicalError>>
    {
        let __tokens = __tokens0.into_iter();
        let mut __tokens = __tokens.map(|t| __ToTriple::to_triple(t));
        let mut __states = vec![0_i32];
        let mut __symbols = vec![];
        let mut __integer;
        let mut __lookahead;
        let mut __last_location = Default::default();
        '__shift: loop {
            __lookahead = match __tokens.next() {
                Some(Ok(v)) => v,
                None => break '__shift,
                Some(Err(e)) => return Err(__lalrpop_util::ParseError::User { error: e }),
            };
            __last_location = __lookahead.2.clone();
            __integer = match __lookahead.1 {
                Tok::LParen if true => 0,
                Tok::RParen if true => 1,
                Tok::Comma if true => 2,
                Tok::Semicolon if true => 3,
                Tok::Eq if true => 4,
                Tok::LBracket if true => 5,
                Tok::RBracket if true => 6,
                Tok::LBrace if true => 7,
                Tok::RBrace if true => 8,
                Tok::Name(_) if true => 9,
                Tok::Number(_) if true => 10,
                Tok::Op(_) if true => 11,
                _ => {
                    return Err(__lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: vec![],
                    });
                }
            };
            '__inner: loop {
                let __state = *__states.last().unwrap() as usize;
                let __action = __ACTION[__state * 13 + __integer];
                if __action > 0 {
                    let __symbol = match __integer {
                        0 => match __lookahead.1 {
                            __tok @ Tok::LParen => __Symbol::Term_22_28_22(__tok),
                            _ => unreachable!(),
                        },
                        1 => match __lookahead.1 {
                            __tok @ Tok::RParen => __Symbol::Term_22_29_22(__tok),
                            _ => unreachable!(),
                        },
                        2 => match __lookahead.1 {
                            __tok @ Tok::Comma => __Symbol::Term_22_2c_22(__tok),
                            _ => unreachable!(),
                        },
                        3 => match __lookahead.1 {
                            __tok @ Tok::Semicolon => __Symbol::Term_22_3b_22(__tok),
                            _ => unreachable!(),
                        },
                        4 => match __lookahead.1 {
                            __tok @ Tok::Eq => __Symbol::Term_22_3d_22(__tok),
                            _ => unreachable!(),
                        },
                        5 => match __lookahead.1 {
                            __tok @ Tok::LBracket => __Symbol::Term_22_5b_22(__tok),
                            _ => unreachable!(),
                        },
                        6 => match __lookahead.1 {
                            __tok @ Tok::RBracket => __Symbol::Term_22_5d_22(__tok),
                            _ => unreachable!(),
                        },
                        7 => match __lookahead.1 {
                            __tok @ Tok::LBrace => __Symbol::Term_22_7b_22(__tok),
                            _ => unreachable!(),
                        },
                        8 => match __lookahead.1 {
                            __tok @ Tok::RBrace => __Symbol::Term_22_7d_22(__tok),
                            _ => unreachable!(),
                        },
                        9 => match __lookahead.1 {
                            Tok::Name(__tok0) => __Symbol::TermName(__tok0),
                            _ => unreachable!(),
                        },
                        10 => match __lookahead.1 {
                            Tok::Number(__tok0) => __Symbol::TermNumber(__tok0),
                            _ => unreachable!(),
                        },
                        11 => match __lookahead.1 {
                            Tok::Op(__tok0) => __Symbol::TermOp(__tok0),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    __states.push(__action - 1);
                    __symbols.push((__lookahead.0, __symbol, __lookahead.2));
                    continue '__shift;
                } else if __action < 0 {
                    if let Some(r) = __reduce(__action, Some(&__lookahead.0), &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                        return r;
                    }
                } else {
                    return Err(__lalrpop_util::ParseError::UnrecognizedToken {
                        token: Some(__lookahead),
                        expected: vec![],
                    });
                }
            }
        }
        loop {
            let __state = *__states.last().unwrap() as usize;
            let __action = __EOF_ACTION[__state];
            if __action < 0 {
                if let Some(r) = __reduce(__action, None, &mut __states, &mut __symbols, ::std::marker::PhantomData::<()>) {
                    return r;
                }
            } else {
                let __error = __lalrpop_util::ParseError::UnrecognizedToken {
                    token: None,
                    expected: vec![],
                };
                return Err(__error);
            }
        }
    }
    pub fn __reduce<
    >(
        __action: i32,
        __lookahead_start: Option<&SrcPos>,
        __states: &mut ::std::vec::Vec<i32>,
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>,
        _: ::std::marker::PhantomData<()>,
    ) -> Option<Result<AST,__lalrpop_util::ParseError<SrcPos, Tok, LexicalError>>>
    {
        let __nonterminal = match -__action {
            1 => {
                // (<Expr> ",") = Expr, "," => ActionFn(33);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action33::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__nt), __end));
                0
            }
            2 => {
                // (<Expr> ",")* =  => ActionFn(31);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action31::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            3 => {
                // (<Expr> ",")* = (<Expr> ",")+ => ActionFn(32);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action32::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            4 => {
                // (<Expr> ",")+ = Expr, "," => ActionFn(36);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action36::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            5 => {
                // (<Expr> ",")+ = (<Expr> ",")+, Expr, "," => ActionFn(37);
                let __sym2 = __pop_Term_22_2c_22(__symbols);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action37::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            6 => {
                // (<Stmt> ";") = Stmt, ";" => ActionFn(26);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action26::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__nt), __end));
                3
            }
            7 => {
                // (<Stmt> ";")* =  => ActionFn(24);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action24::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            8 => {
                // (<Stmt> ";")* = (<Stmt> ";")+ => ActionFn(25);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action25::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            9 => {
                // (<Stmt> ";")+ = Stmt, ";" => ActionFn(40);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action40::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            10 => {
                // (<Stmt> ";")+ = (<Stmt> ";")+, Stmt, ";" => ActionFn(41);
                let __sym2 = __pop_Term_22_3b_22(__symbols);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action41::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            11 => {
                // App = App, Simple => ActionFn(8);
                let __sym1 = __pop_NtSimple(__symbols);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action8::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtApp(__nt), __end));
                6
            }
            12 => {
                // App = Simple => ActionFn(9);
                let __sym0 = __pop_NtSimple(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtApp(__nt), __end));
                6
            }
            13 => {
                // Atom = Name => ActionFn(16);
                let __sym0 = __pop_TermName(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action16::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtAtom(__nt), __end));
                7
            }
            14 => {
                // Atom = Number => ActionFn(17);
                let __sym0 = __pop_TermNumber(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action17::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtAtom(__nt), __end));
                7
            }
            15 => {
                // Coll = "(", CommaT<Expr>, ")" => ActionFn(14);
                let __sym2 = __pop_Term_22_29_22(__symbols);
                let __sym1 = __pop_NtCommaT_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_28_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action14::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                8
            }
            16 => {
                // Coll = "[", Comma<Expr>, "]" => ActionFn(15);
                let __sym2 = __pop_Term_22_5d_22(__symbols);
                let __sym1 = __pop_NtComma_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_5b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action15::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                8
            }
            17 => {
                // Comma<Expr> = Expr => ActionFn(44);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action44::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            18 => {
                // Comma<Expr> =  => ActionFn(45);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action45::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            19 => {
                // Comma<Expr> = (<Expr> ",")+, Expr => ActionFn(46);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action46::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            20 => {
                // Comma<Expr> = (<Expr> ",")+ => ActionFn(47);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action47::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                9
            }
            21 => {
                // CommaT<Expr> = (<Expr> ",")+ => ActionFn(19);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action19::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtCommaT_3cExpr_3e(__nt), __end));
                10
            }
            22 => {
                // CommaT<Expr> = "," => ActionFn(20);
                let __sym0 = __pop_Term_22_2c_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action20::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtCommaT_3cExpr_3e(__nt), __end));
                10
            }
            23 => {
                // Expr = Stmt => ActionFn(3);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr(__nt), __end));
                11
            }
            24 => {
                // Expr? = Expr => ActionFn(29);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action29::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                12
            }
            25 => {
                // Expr? =  => ActionFn(30);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action30::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                12
            }
            26 => {
                // Exprs = SemiColon<Stmt> => ActionFn(2);
                let __sym0 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExprs(__nt), __end));
                13
            }
            27 => {
                // Infix = Infix, Op, App => ActionFn(6);
                let __sym2 = __pop_NtApp(__symbols);
                let __sym1 = __pop_TermOp(__symbols);
                let __sym0 = __pop_NtInfix(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action6::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtInfix(__nt), __end));
                14
            }
            28 => {
                // Infix = App => ActionFn(7);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action7::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtInfix(__nt), __end));
                14
            }
            29 => {
                // SemiColon<Stmt> = Stmt => ActionFn(48);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action48::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            30 => {
                // SemiColon<Stmt> =  => ActionFn(49);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action49::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            31 => {
                // SemiColon<Stmt> = (<Stmt> ";")+, Stmt => ActionFn(50);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action50::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            32 => {
                // SemiColon<Stmt> = (<Stmt> ";")+ => ActionFn(51);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action51::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                15
            }
            33 => {
                // Simple = "{", Exprs, "}" => ActionFn(10);
                let __sym2 = __pop_Term_22_7d_22(__symbols);
                let __sym1 = __pop_NtExprs(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action10::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            34 => {
                // Simple = "(", Expr, ")" => ActionFn(11);
                let __sym2 = __pop_Term_22_29_22(__symbols);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Term_22_28_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action11::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            35 => {
                // Simple = Coll => ActionFn(12);
                let __sym0 = __pop_NtColl(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action12::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            36 => {
                // Simple = Atom => ActionFn(13);
                let __sym0 = __pop_NtAtom(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSimple(__nt), __end));
                16
            }
            37 => {
                // Stmt = Infix, "=", Infix => ActionFn(4);
                let __sym2 = __pop_NtInfix(__symbols);
                let __sym1 = __pop_Term_22_3d_22(__symbols);
                let __sym0 = __pop_NtInfix(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action4::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                17
            }
            38 => {
                // Stmt = Infix => ActionFn(5);
                let __sym0 = __pop_NtInfix(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action5::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                17
            }
            39 => {
                // Stmt? = Stmt => ActionFn(22);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action22::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                18
            }
            40 => {
                // Stmt? =  => ActionFn(23);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action23::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                18
            }
            41 => {
                // __Expr = Expr => ActionFn(1);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action1::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt____Expr(__nt), __end));
                19
            }
            42 => {
                // __Exprs = Exprs => ActionFn(0);
                let __sym0 = __pop_NtExprs(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0::<>(__sym0);
                return Some(Ok(__nt));
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __state = *__states.last().unwrap() as usize;
        let __next_state = __GOTO[__state * 21 + __nonterminal] - 1;
        __states.push(__next_state);
        None
    }
    fn __pop_Term_22_28_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_28_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_29_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_29_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_2c_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_2c_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Tok, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermName<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, String, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermName(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermNumber<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, String, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermNumber(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermOp<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, String, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermOp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Termerror<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, __lalrpop_util::ErrorRecovery<SrcPos, Tok, LexicalError>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Termerror(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::vec::Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtApp<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtApp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtAtom<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtAtom(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtColl<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtColl(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtComma_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtComma_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtCommaT_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtCommaT_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::option::Option<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExprs<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExprs(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtInfix<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtInfix(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtSemiColon_3cStmt_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, Vec<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtSemiColon_3cStmt_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtSimple<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtSimple(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, ::std::option::Option<AST>, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Expr<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Expr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Exprs<
    >(
        __symbols: &mut ::std::vec::Vec<(SrcPos,__Symbol<>,SrcPos)>
    ) -> (SrcPos, AST, SrcPos) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Exprs(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
}
pub use self::__parse__Exprs::parse_Exprs;

pub fn __action0<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action1<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action2<
>(
    (_, stmts, _): (SrcPos, Vec<AST>, SrcPos),
) -> AST
{
    AST::Block(stmts)
}

pub fn __action3<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action4<
>(
    (_, lhs, _): (SrcPos, AST, SrcPos),
    (_, _, _): (SrcPos, Tok, SrcPos),
    (_, rhs, _): (SrcPos, AST, SrcPos),
) -> AST
{
    AST::Def(Box::new(lhs), Box::new(rhs))
}

pub fn __action5<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action6<
>(
    (_, l, _): (SrcPos, AST, SrcPos),
    (_, op, _): (SrcPos, String, SrcPos),
    (_, r, _): (SrcPos, AST, SrcPos),
) -> AST
{
    AST::App(Box::new(AST::Symbol(op)),
                                             Box::new(AST::Tuple(vec![l, r])))
}

pub fn __action7<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action8<
>(
    (_, f, _): (SrcPos, AST, SrcPos),
    (_, arg, _): (SrcPos, AST, SrcPos),
) -> AST
{
    AST::App(Box::new(f), Box::new(arg))
}

pub fn __action9<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action10<
>(
    (_, _, _): (SrcPos, Tok, SrcPos),
    (_, __0, _): (SrcPos, AST, SrcPos),
    (_, _, _): (SrcPos, Tok, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action11<
>(
    (_, _, _): (SrcPos, Tok, SrcPos),
    (_, __0, _): (SrcPos, AST, SrcPos),
    (_, _, _): (SrcPos, Tok, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action12<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action13<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action14<
>(
    (_, _, _): (SrcPos, Tok, SrcPos),
    (_, elems, _): (SrcPos, Vec<AST>, SrcPos),
    (_, _, _): (SrcPos, Tok, SrcPos),
) -> AST
{
    AST::Tuple(elems)
}

pub fn __action15<
>(
    (_, _, _): (SrcPos, Tok, SrcPos),
    (_, elems, _): (SrcPos, Vec<AST>, SrcPos),
    (_, _, _): (SrcPos, Tok, SrcPos),
) -> AST
{
    AST::Array(elems)//,
    //"{" <elems: CommaPlus<Expr>> "}" => AST::Set(elems),
    //"{" <kvs: Comma<MapPair>> "}"    => AST::Map(kvs)
}

pub fn __action16<
>(
    (_, __0, _): (SrcPos, String, SrcPos),
) -> AST
{
    AST::Symbol(__0)
}

pub fn __action17<
>(
    (_, __0, _): (SrcPos, String, SrcPos),
) -> AST
{
    AST::Int(isize::from_str(&__0).unwrap())
}

pub fn __action18<
>(
    (_, v, _): (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    (_, e, _): (SrcPos, ::std::option::Option<AST>, SrcPos),
) -> Vec<AST>
{
    match e {
        None => v,
        Some(e) => {
            let mut v = v;
            v.push(e);
            v
        }
    }
}

pub fn __action19<
>(
    (_, v, _): (SrcPos, ::std::vec::Vec<AST>, SrcPos),
) -> Vec<AST>
{
    v
}

pub fn __action20<
>(
    (_, __0, _): (SrcPos, Tok, SrcPos),
) -> Vec<AST>
{
    vec![]
}

pub fn __action21<
>(
    (_, v, _): (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    (_, e, _): (SrcPos, ::std::option::Option<AST>, SrcPos),
) -> Vec<AST>
{
    match e {
        None => v,
        Some(e) => {
            let mut v = v;
            v.push(e);
            v
        }
    }
}

pub fn __action22<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> ::std::option::Option<AST>
{
    Some(__0)
}

pub fn __action23<
>(
    __lookbehind: &SrcPos,
    __lookahead: &SrcPos,
) -> ::std::option::Option<AST>
{
    None
}

pub fn __action24<
>(
    __lookbehind: &SrcPos,
    __lookahead: &SrcPos,
) -> ::std::vec::Vec<AST>
{
    vec![]
}

pub fn __action25<
>(
    (_, v, _): (SrcPos, ::std::vec::Vec<AST>, SrcPos),
) -> ::std::vec::Vec<AST>
{
    v
}

pub fn __action26<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
    (_, _, _): (SrcPos, Tok, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action27<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> ::std::vec::Vec<AST>
{
    vec![__0]
}

pub fn __action28<
>(
    (_, v, _): (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    (_, e, _): (SrcPos, AST, SrcPos),
) -> ::std::vec::Vec<AST>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action29<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> ::std::option::Option<AST>
{
    Some(__0)
}

pub fn __action30<
>(
    __lookbehind: &SrcPos,
    __lookahead: &SrcPos,
) -> ::std::option::Option<AST>
{
    None
}

pub fn __action31<
>(
    __lookbehind: &SrcPos,
    __lookahead: &SrcPos,
) -> ::std::vec::Vec<AST>
{
    vec![]
}

pub fn __action32<
>(
    (_, v, _): (SrcPos, ::std::vec::Vec<AST>, SrcPos),
) -> ::std::vec::Vec<AST>
{
    v
}

pub fn __action33<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
    (_, _, _): (SrcPos, Tok, SrcPos),
) -> AST
{
    (__0)
}

pub fn __action34<
>(
    (_, __0, _): (SrcPos, AST, SrcPos),
) -> ::std::vec::Vec<AST>
{
    vec![__0]
}

pub fn __action35<
>(
    (_, v, _): (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    (_, e, _): (SrcPos, AST, SrcPos),
) -> ::std::vec::Vec<AST>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action36<
>(
    __0: (SrcPos, AST, SrcPos),
    __1: (SrcPos, Tok, SrcPos),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action33(
        __0,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action27(
        __temp0,
    )
}

pub fn __action37<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    __1: (SrcPos, AST, SrcPos),
    __2: (SrcPos, Tok, SrcPos),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action33(
        __1,
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action28(
        __0,
        __temp0,
    )
}

pub fn __action38<
>(
    __0: (SrcPos, ::std::option::Option<AST>, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action31(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action18(
        __temp0,
        __0,
    )
}

pub fn __action39<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    __1: (SrcPos, ::std::option::Option<AST>, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action32(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action18(
        __temp0,
        __1,
    )
}

pub fn __action40<
>(
    __0: (SrcPos, AST, SrcPos),
    __1: (SrcPos, Tok, SrcPos),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action26(
        __0,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action34(
        __temp0,
    )
}

pub fn __action41<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    __1: (SrcPos, AST, SrcPos),
    __2: (SrcPos, Tok, SrcPos),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action26(
        __1,
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action35(
        __0,
        __temp0,
    )
}

pub fn __action42<
>(
    __0: (SrcPos, ::std::option::Option<AST>, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action24(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        __temp0,
        __0,
    )
}

pub fn __action43<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    __1: (SrcPos, ::std::option::Option<AST>, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action25(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        __temp0,
        __1,
    )
}

pub fn __action44<
>(
    __0: (SrcPos, AST, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action29(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action38(
        __temp0,
    )
}

pub fn __action45<
>(
    __lookbehind: &SrcPos,
    __lookahead: &SrcPos,
) -> Vec<AST>
{
    let __start0 = __lookbehind.clone();
    let __end0 = __lookahead.clone();
    let __temp0 = __action30(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action38(
        __temp0,
    )
}

pub fn __action46<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    __1: (SrcPos, AST, SrcPos),
) -> Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action29(
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action39(
        __0,
        __temp0,
    )
}

pub fn __action47<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.2.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action30(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action39(
        __0,
        __temp0,
    )
}

pub fn __action48<
>(
    __0: (SrcPos, AST, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action22(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action42(
        __temp0,
    )
}

pub fn __action49<
>(
    __lookbehind: &SrcPos,
    __lookahead: &SrcPos,
) -> Vec<AST>
{
    let __start0 = __lookbehind.clone();
    let __end0 = __lookahead.clone();
    let __temp0 = __action23(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action42(
        __temp0,
    )
}

pub fn __action50<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
    __1: (SrcPos, AST, SrcPos),
) -> Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action22(
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action43(
        __0,
        __temp0,
    )
}

pub fn __action51<
>(
    __0: (SrcPos, ::std::vec::Vec<AST>, SrcPos),
) -> Vec<AST>
{
    let __start0 = __0.2.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action23(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action43(
        __0,
        __temp0,
    )
}

pub trait __ToTriple<> {
    type Error;
    fn to_triple(value: Self) -> Result<(SrcPos,Tok,SrcPos),Self::Error>;
}

impl<> __ToTriple<> for (SrcPos, Tok, SrcPos) {
    type Error = LexicalError;
    fn to_triple(value: Self) -> Result<(SrcPos,Tok,SrcPos),LexicalError> {
        Ok(value)
    }
}
impl<> __ToTriple<> for Result<(SrcPos, Tok, SrcPos),LexicalError> {
    type Error = LexicalError;
    fn to_triple(value: Self) -> Result<(SrcPos,Tok,SrcPos),LexicalError> {
        value
    }
}
