use lexer::{Tok, LocTok, LexicalError};
use ast::AST;
use std::str::FromStr;
extern crate lalrpop_util as __lalrpop_util;

mod __parse__BlockBody {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use lexer::{Tok, LocTok, LexicalError};
    use ast::AST;
    use std::str::FromStr;
    extern crate lalrpop_util as __lalrpop_util;
    use super::__ToTriple;
    #[allow(dead_code)]
    pub enum __Symbol<> {
        Term_22_28_22(LocTok),
        Term_22_29_22(LocTok),
        Term_22_2c_22(LocTok),
        Term_22_3b_22(LocTok),
        Term_22_3d_22(LocTok),
        Term_22_5b_22(LocTok),
        Term_22_5d_22(LocTok),
        Term_22_7b_22(LocTok),
        Term_22_7c_22(LocTok),
        Term_22_7d_22(LocTok),
        TermId(String),
        TermIndent(LocTok),
        Termerror(__lalrpop_util::ErrorRecovery<usize, LocTok, LexicalError>),
        Nt_28_3cExpr_3e_20_22_2c_22_29(AST),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2b(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29(AST),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2b(::std::vec::Vec<AST>),
        NtApp(AST),
        NtAtom(AST),
        NtBlock(AST),
        NtBlockBody(AST),
        NtColl(AST),
        NtComma_3cExpr_3e(Vec<AST>),
        NtCommaPlus_3cExpr_3e(Vec<AST>),
        NtDef(AST),
        NtDefPat(AST),
        NtExpr(AST),
        NtExpr_3f(::std::option::Option<AST>),
        NtNonApp(AST),
        NtPat(AST),
        NtPat_2a(::std::vec::Vec<AST>),
        NtPat_2b(::std::vec::Vec<AST>),
        NtSemiColon_3cStmt_3e(Vec<AST>),
        NtStmt(AST),
        NtStmt_3f(::std::option::Option<AST>),
        Nt____BlockBody(AST),
        Nt____Expr(AST),
    }
    const __ACTION: &'static [i32] = &[
        // State 0
        15, 0, 0, 0, 0, 16, 0, 17, 0, 0, 18, 0, 0,
        // State 1
        15, 0, 0, 0, 0, 16, 0, 17, 0, -44, 18, 0, 0,
        // State 2
        -28, 0, 0, -28, -26, -28, 0, -28, 0, -28, -28, 0, 0,
        // State 3
        -34, 0, 0, -34, -36, -34, 0, -34, 0, -34, -34, 0, 0,
        // State 4
        -32, -32, -32, -32, -32, -32, -32, -32, 0, -32, -32, 0, 0,
        // State 5
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 6
        -33, 0, 0, -33, -35, -33, 0, -33, 0, -33, -33, 0, 0,
        // State 7
        0, 0, 0, -46, 0, 0, 0, 0, 0, -46, 0, 0, 0,
        // State 8
        0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 9
        15, 0, 0, -45, 0, 16, 0, 17, 0, -45, 18, 0, 0,
        // State 10
        -29, -29, -29, -29, 0, -29, -29, -29, 0, -29, -29, 0, 0,
        // State 11
        0, 0, 0, 0, -27, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 12
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 13
        0, 0, 0, 24, 0, 0, 0, 0, 0, -41, 0, 0, 0,
        // State 14
        15, -20, 0, 0, 0, 16, 0, 17, 0, 0, 18, 0, 0,
        // State 15
        15, 0, 0, 0, 0, 16, -20, 17, 0, 0, 18, 0, 0,
        // State 16
        15, 0, 0, 0, 0, 16, 0, 17, 33, 0, 18, 0, 0,
        // State 17
        -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, 0, 0,
        // State 18
        0, 0, 0, 34, 0, 0, 0, 0, 0, -43, 0, 0, 0,
        // State 19
        15, 0, 0, 0, 0, 16, 0, 17, 0, 0, 18, 0, 0,
        // State 20
        -34, -34, -34, -34, -34, -34, -34, -34, 0, -34, -34, 0, 0,
        // State 21
        -33, -33, -33, -33, -33, -33, -33, -33, 0, -33, -33, 0, 0,
        // State 22
        -11, -11, -11, -11, -11, -11, -11, -11, 0, -11, -11, 0, 0,
        // State 23
        -9, 0, 0, 0, 0, -9, 0, -9, 0, -9, -9, 0, 0,
        // State 24
        15, -22, 0, 0, 0, 16, -22, 17, 0, 0, 18, 0, 0,
        // State 25
        -28, -28, -28, -28, 0, -28, -28, -28, 0, -28, -28, 0, 0,
        // State 26
        0, 37, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 27
        15, -19, 38, 0, 0, 16, -19, 17, 0, 0, 18, 0, 0,
        // State 28
        0, 0, 0, 0, 0, 0, 39, 0, 0, 0, 0, 0, 0,
        // State 29
        15, 0, 0, 0, 0, 16, 0, 17, 0, 0, 18, 0, 0,
        // State 30
        0, 0, 0, 0, 0, 0, 0, 0, 0, 41, 0, 0, 0,
        // State 31
        15, 0, 38, 0, 0, 16, 0, 17, 0, -23, 18, 0, 0,
        // State 32
        15, 0, 0, 0, 0, 16, 0, 46, 47, 0, 18, 0, 0,
        // State 33
        -10, 0, 0, 0, 0, -10, 0, -10, 0, -10, -10, 0, 0,
        // State 34
        15, 0, 0, -25, 0, 16, 0, 17, 0, -25, 18, 0, 0,
        // State 35
        15, -21, 48, 0, 0, 16, -21, 17, 0, 0, 18, 0, 0,
        // State 36
        -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, 0, 0,
        // State 37
        -4, -4, 0, 0, 0, -4, -4, -4, 0, 0, -4, 0, 0,
        // State 38
        -17, -17, -17, -17, -17, -17, -17, -17, -17, -17, -17, 0, 0,
        // State 39
        15, 0, 48, 0, 0, 16, 0, 17, 0, -24, 18, 0, 0,
        // State 40
        -18, -18, -18, -18, -18, -18, -18, -18, -18, -18, -18, 0, 0,
        // State 41
        -36, 0, 0, 0, 0, -36, 0, -36, -36, 0, -36, 0, 0,
        // State 42
        -35, 0, 0, 0, 0, -35, 0, -35, -35, 0, -35, 0, 0,
        // State 43
        -39, 0, 0, 0, 0, -39, 0, -39, -39, 0, -39, 0, 0,
        // State 44
        15, 0, 0, 0, 0, 16, 0, 46, 50, 0, 18, 0, 0,
        // State 45
        15, 0, 0, 0, 0, 16, 0, 17, 0, 0, 18, 0, 0,
        // State 46
        15, 0, 0, 0, 0, 16, 0, 17, 0, -42, 18, 0, 0,
        // State 47
        -5, -5, 0, 0, 0, -5, -5, -5, 0, 0, -5, 0, 0,
        // State 48
        -40, 0, 0, 0, 0, -40, 0, -40, -40, 0, -40, 0, 0,
        // State 49
        15, 0, 0, 0, 0, 16, 0, 17, 0, -42, 18, 0, 0,
        // State 50
        0, 0, 0, 0, 0, 0, 0, 0, 0, 53, 0, 0, 0,
        // State 51
        0, 0, 0, 0, 0, 0, 0, 0, 0, 54, 0, 0, 0,
        // State 52
        -13, -13, -13, -13, -13, -13, -13, -13, 0, -13, -13, 0, 0,
        // State 53
        -14, -14, -14, -14, -14, -14, -14, -14, 0, -14, -14, 0, 0,
    ];
    const __EOF_ACTION: &'static [i32] = &[
        -42,
        -44,
        -28,
        -34,
        -32,
        -49,
        -33,
        -46,
        0,
        -45,
        -29,
        0,
        -15,
        -41,
        0,
        0,
        0,
        -12,
        -43,
        0,
        -34,
        -33,
        -11,
        -9,
        0,
        -28,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        -10,
        -25,
        0,
        -16,
        0,
        -17,
        0,
        -18,
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
        -13,
        -14,
    ];
    const __GOTO: &'static [i32] = &[
        // State 0
        0, 0, 0, 0, 0, 2, 3, 4, 5, 6, 7, 0, 0, 8, 9, 10, 0, 11, 12, 0, 0, 13, 14, 0, 0, 0,
        // State 1
        0, 0, 0, 0, 0, 0, 3, 4, 5, 0, 7, 0, 0, 8, 9, 10, 0, 11, 12, 0, 0, 0, 19, 0, 0, 0,
        // State 2
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 3
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 5
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 6
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 7
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 8
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 9
        0, 0, 0, 0, 0, 0, 0, 21, 5, 0, 22, 0, 0, 0, 0, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 10
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 11
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 12
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 13
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 14
        0, 0, 25, 0, 0, 0, 26, 21, 5, 0, 22, 27, 0, 0, 0, 28, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 15
        0, 0, 25, 0, 0, 0, 26, 21, 5, 0, 22, 29, 0, 0, 0, 28, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 16
        0, 0, 30, 0, 0, 0, 26, 21, 5, 0, 22, 0, 31, 0, 0, 32, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 17
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 18
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 19
        0, 0, 0, 0, 0, 0, 26, 21, 5, 0, 22, 0, 0, 0, 0, 35, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 20
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 21
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 22
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 23
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 24
        0, 0, 0, 0, 0, 0, 26, 21, 5, 0, 22, 0, 0, 0, 0, 36, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 25
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 26
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 27
        0, 0, 0, 0, 0, 0, 0, 21, 5, 0, 22, 0, 0, 0, 0, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 28
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 29
        0, 0, 0, 0, 0, 0, 26, 21, 5, 0, 22, 0, 0, 0, 0, 40, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 30
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 31
        0, 0, 0, 0, 0, 0, 0, 21, 5, 0, 22, 0, 0, 0, 0, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 32
        0, 0, 0, 0, 0, 0, 0, 42, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 44, 0, 45, 0, 0, 0, 0, 0,
        // State 33
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 34
        0, 0, 0, 0, 0, 0, 0, 21, 5, 0, 22, 0, 0, 0, 0, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 35
        0, 0, 0, 0, 0, 0, 0, 21, 5, 0, 22, 0, 0, 0, 0, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 36
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 37
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 38
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 39
        0, 0, 0, 0, 0, 0, 0, 21, 5, 0, 22, 0, 0, 0, 0, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 40
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 41
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 42
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 43
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 44
        0, 0, 0, 0, 0, 0, 0, 42, 0, 0, 43, 0, 0, 0, 0, 0, 0, 0, 49, 0, 0, 0, 0, 0, 0, 0,
        // State 45
        0, 0, 30, 0, 0, 0, 26, 21, 5, 0, 22, 0, 31, 0, 0, 32, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 46
        0, 0, 0, 0, 0, 2, 3, 4, 5, 0, 7, 0, 0, 8, 9, 10, 0, 11, 12, 0, 0, 51, 14, 0, 0, 0,
        // State 47
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 48
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 49
        0, 0, 0, 0, 0, 2, 3, 4, 5, 0, 7, 0, 0, 8, 9, 10, 0, 11, 12, 0, 0, 52, 14, 0, 0, 0,
        // State 50
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 51
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 52
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 53
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    pub fn parse_BlockBody<
        __TOKEN: __ToTriple<Error=LexicalError>,
        __TOKENS: IntoIterator<Item=__TOKEN>,
    >(
        __tokens0: __TOKENS,
    ) -> Result<AST, __lalrpop_util::ParseError<usize, LocTok, LexicalError>>
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
                LocTok { tok: Tok::LParen, .. } if true => 0,
                LocTok { tok: Tok::RParen, .. } if true => 1,
                LocTok { tok: Tok::Comma, .. } if true => 2,
                LocTok { tok: Tok::Semicolon, .. } if true => 3,
                LocTok { tok: Tok::Eq, .. } if true => 4,
                LocTok { tok: Tok::LBracket, .. } if true => 5,
                LocTok { tok: Tok::RBracket, .. } if true => 6,
                LocTok { tok: Tok::LBrace, .. } if true => 7,
                LocTok { tok: Tok::Pipe, .. } if true => 8,
                LocTok { tok: Tok::RBrace, .. } if true => 9,
                LocTok { tok: Tok::Id(_), .. } if true => 10,
                LocTok { tok: Tok::Indent, .. } if true => 11,
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
                            __tok @ LocTok { tok: Tok::LParen, .. } => __Symbol::Term_22_28_22(__tok),
                            _ => unreachable!(),
                        },
                        1 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::RParen, .. } => __Symbol::Term_22_29_22(__tok),
                            _ => unreachable!(),
                        },
                        2 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Comma, .. } => __Symbol::Term_22_2c_22(__tok),
                            _ => unreachable!(),
                        },
                        3 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Semicolon, .. } => __Symbol::Term_22_3b_22(__tok),
                            _ => unreachable!(),
                        },
                        4 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Eq, .. } => __Symbol::Term_22_3d_22(__tok),
                            _ => unreachable!(),
                        },
                        5 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::LBracket, .. } => __Symbol::Term_22_5b_22(__tok),
                            _ => unreachable!(),
                        },
                        6 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::RBracket, .. } => __Symbol::Term_22_5d_22(__tok),
                            _ => unreachable!(),
                        },
                        7 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::LBrace, .. } => __Symbol::Term_22_7b_22(__tok),
                            _ => unreachable!(),
                        },
                        8 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Pipe, .. } => __Symbol::Term_22_7c_22(__tok),
                            _ => unreachable!(),
                        },
                        9 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::RBrace, .. } => __Symbol::Term_22_7d_22(__tok),
                            _ => unreachable!(),
                        },
                        10 => match __lookahead.1 {
                            LocTok { tok: Tok::Id(__tok0), .. } => __Symbol::TermId(__tok0),
                            _ => unreachable!(),
                        },
                        11 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Indent, .. } => __Symbol::TermIndent(__tok),
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
        __lookahead_start: Option<&usize>,
        __states: &mut ::std::vec::Vec<i32>,
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>,
        _: ::std::marker::PhantomData<()>,
    ) -> Option<Result<AST,__lalrpop_util::ParseError<usize, LocTok, LexicalError>>>
    {
        let __nonterminal = match -__action {
            1 => {
                // (<Expr> ",") = Expr, "," => ActionFn(37);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action37::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__nt), __end));
                0
            }
            2 => {
                // (<Expr> ",")* =  => ActionFn(35);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action35::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            3 => {
                // (<Expr> ",")* = (<Expr> ",")+ => ActionFn(36);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action36::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            4 => {
                // (<Expr> ",")+ = Expr, "," => ActionFn(42);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action42::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            5 => {
                // (<Expr> ",")+ = (<Expr> ",")+, Expr, "," => ActionFn(43);
                let __sym2 = __pop_Term_22_2c_22(__symbols);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action43::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            6 => {
                // (<Stmt> ";") = Stmt, ";" => ActionFn(32);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action32::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__nt), __end));
                3
            }
            7 => {
                // (<Stmt> ";")* =  => ActionFn(30);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action30::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            8 => {
                // (<Stmt> ";")* = (<Stmt> ";")+ => ActionFn(31);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action31::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            9 => {
                // (<Stmt> ";")+ = Stmt, ";" => ActionFn(48);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action48::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            10 => {
                // (<Stmt> ";")+ = (<Stmt> ";")+, Stmt, ";" => ActionFn(49);
                let __sym2 = __pop_Term_22_3b_22(__symbols);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action49::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            11 => {
                // App = Expr, NonApp => ActionFn(14);
                let __sym1 = __pop_NtNonApp(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action14::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtApp(__nt), __end));
                6
            }
            12 => {
                // Atom = Id => ActionFn(13);
                let __sym0 = __pop_TermId(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtAtom(__nt), __end));
                7
            }
            13 => {
                // Block = "{", "|", "|", SemiColon<Stmt>, "}" => ActionFn(56);
                let __sym4 = __pop_Term_22_7d_22(__symbols);
                let __sym3 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __sym2 = __pop_Term_22_7c_22(__symbols);
                let __sym1 = __pop_Term_22_7c_22(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action56::<>(__sym0, __sym1, __sym2, __sym3, __sym4);
                let __states_len = __states.len();
                __states.truncate(__states_len - 5);
                __symbols.push((__start, __Symbol::NtBlock(__nt), __end));
                8
            }
            14 => {
                // Block = "{", "|", Pat+, "|", SemiColon<Stmt>, "}" => ActionFn(57);
                let __sym5 = __pop_Term_22_7d_22(__symbols);
                let __sym4 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __sym3 = __pop_Term_22_7c_22(__symbols);
                let __sym2 = __pop_NtPat_2b(__symbols);
                let __sym1 = __pop_Term_22_7c_22(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym5.2.clone();
                let __nt = super::__action57::<>(__sym0, __sym1, __sym2, __sym3, __sym4, __sym5);
                let __states_len = __states.len();
                __states.truncate(__states_len - 6);
                __symbols.push((__start, __Symbol::NtBlock(__nt), __end));
                8
            }
            15 => {
                // BlockBody = SemiColon<Stmt> => ActionFn(16);
                let __sym0 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action16::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtBlockBody(__nt), __end));
                9
            }
            16 => {
                // Coll = "(", Comma<Expr>, ")" => ActionFn(18);
                let __sym2 = __pop_Term_22_29_22(__symbols);
                let __sym1 = __pop_NtComma_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_28_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action18::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                10
            }
            17 => {
                // Coll = "[", Comma<Expr>, "]" => ActionFn(19);
                let __sym2 = __pop_Term_22_5d_22(__symbols);
                let __sym1 = __pop_NtComma_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_5b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action19::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                10
            }
            18 => {
                // Coll = "{", CommaPlus<Expr>, "}" => ActionFn(20);
                let __sym2 = __pop_Term_22_7d_22(__symbols);
                let __sym1 = __pop_NtCommaPlus_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action20::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                10
            }
            19 => {
                // Comma<Expr> = Expr => ActionFn(52);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action52::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            20 => {
                // Comma<Expr> =  => ActionFn(53);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action53::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            21 => {
                // Comma<Expr> = (<Expr> ",")+, Expr => ActionFn(54);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action54::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            22 => {
                // Comma<Expr> = (<Expr> ",")+ => ActionFn(55);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action55::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            23 => {
                // CommaPlus<Expr> = Expr => ActionFn(46);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action46::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtCommaPlus_3cExpr_3e(__nt), __end));
                12
            }
            24 => {
                // CommaPlus<Expr> = (<Expr> ",")+, Expr => ActionFn(47);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action47::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtCommaPlus_3cExpr_3e(__nt), __end));
                12
            }
            25 => {
                // Def = DefPat, "=", Expr => ActionFn(17);
                let __sym2 = __pop_NtExpr(__symbols);
                let __sym1 = __pop_Term_22_3d_22(__symbols);
                let __sym0 = __pop_NtDefPat(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action17::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtDef(__nt), __end));
                13
            }
            26 => {
                // DefPat = App => ActionFn(9);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtDefPat(__nt), __end));
                14
            }
            27 => {
                // DefPat = Pat => ActionFn(10);
                let __sym0 = __pop_NtPat(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action10::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtDefPat(__nt), __end));
                14
            }
            28 => {
                // Expr = App => ActionFn(4);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action4::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr(__nt), __end));
                15
            }
            29 => {
                // Expr = NonApp => ActionFn(5);
                let __sym0 = __pop_NtNonApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action5::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr(__nt), __end));
                15
            }
            30 => {
                // Expr? = Expr => ActionFn(33);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action33::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                16
            }
            31 => {
                // Expr? =  => ActionFn(34);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action34::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                16
            }
            32 => {
                // NonApp = Block => ActionFn(6);
                let __sym0 = __pop_NtBlock(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action6::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtNonApp(__nt), __end));
                17
            }
            33 => {
                // NonApp = Coll => ActionFn(7);
                let __sym0 = __pop_NtColl(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action7::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtNonApp(__nt), __end));
                17
            }
            34 => {
                // NonApp = Atom => ActionFn(8);
                let __sym0 = __pop_NtAtom(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action8::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtNonApp(__nt), __end));
                17
            }
            35 => {
                // Pat = Coll => ActionFn(11);
                let __sym0 = __pop_NtColl(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action11::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat(__nt), __end));
                18
            }
            36 => {
                // Pat = Atom => ActionFn(12);
                let __sym0 = __pop_NtAtom(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action12::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat(__nt), __end));
                18
            }
            37 => {
                // Pat* =  => ActionFn(24);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action24::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtPat_2a(__nt), __end));
                19
            }
            38 => {
                // Pat* = Pat+ => ActionFn(25);
                let __sym0 = __pop_NtPat_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action25::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat_2a(__nt), __end));
                19
            }
            39 => {
                // Pat+ = Pat => ActionFn(26);
                let __sym0 = __pop_NtPat(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action26::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat_2b(__nt), __end));
                20
            }
            40 => {
                // Pat+ = Pat+, Pat => ActionFn(27);
                let __sym1 = __pop_NtPat(__symbols);
                let __sym0 = __pop_NtPat_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action27::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtPat_2b(__nt), __end));
                20
            }
            41 => {
                // SemiColon<Stmt> = Stmt => ActionFn(58);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action58::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            42 => {
                // SemiColon<Stmt> =  => ActionFn(59);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action59::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            43 => {
                // SemiColon<Stmt> = (<Stmt> ";")+, Stmt => ActionFn(60);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action60::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            44 => {
                // SemiColon<Stmt> = (<Stmt> ";")+ => ActionFn(61);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action61::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            45 => {
                // Stmt = Expr => ActionFn(2);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                22
            }
            46 => {
                // Stmt = Def => ActionFn(3);
                let __sym0 = __pop_NtDef(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                22
            }
            47 => {
                // Stmt? = Stmt => ActionFn(28);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action28::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                23
            }
            48 => {
                // Stmt? =  => ActionFn(29);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action29::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                23
            }
            49 => {
                // __BlockBody = BlockBody => ActionFn(1);
                let __sym0 = __pop_NtBlockBody(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action1::<>(__sym0);
                return Some(Ok(__nt));
            }
            50 => {
                // __Expr = Expr => ActionFn(0);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt____Expr(__nt), __end));
                25
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __state = *__states.last().unwrap() as usize;
        let __next_state = __GOTO[__state * 26 + __nonterminal] - 1;
        __states.push(__next_state);
        None
    }
    fn __pop_Term_22_28_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_28_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_29_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_29_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_2c_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_2c_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7c_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7c_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermId<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, String, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermId(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermIndent<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermIndent(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Termerror<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, __lalrpop_util::ErrorRecovery<usize, LocTok, LexicalError>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Termerror(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtApp<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtApp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtAtom<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtAtom(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtBlock<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtBlock(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtBlockBody<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtBlockBody(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtColl<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtColl(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtComma_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtComma_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtCommaPlus_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtCommaPlus_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtDef<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtDef(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtDefPat<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtDefPat(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::option::Option<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtNonApp<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtNonApp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPat<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPat(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPat_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPat_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPat_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPat_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtSemiColon_3cStmt_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtSemiColon_3cStmt_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::option::Option<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____BlockBody<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____BlockBody(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Expr<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Expr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
}
pub use self::__parse__BlockBody::parse_BlockBody;

mod __parse__Expr {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use lexer::{Tok, LocTok, LexicalError};
    use ast::AST;
    use std::str::FromStr;
    extern crate lalrpop_util as __lalrpop_util;
    use super::__ToTriple;
    #[allow(dead_code)]
    pub enum __Symbol<> {
        Term_22_28_22(LocTok),
        Term_22_29_22(LocTok),
        Term_22_2c_22(LocTok),
        Term_22_3b_22(LocTok),
        Term_22_3d_22(LocTok),
        Term_22_5b_22(LocTok),
        Term_22_5d_22(LocTok),
        Term_22_7b_22(LocTok),
        Term_22_7c_22(LocTok),
        Term_22_7d_22(LocTok),
        TermId(String),
        TermIndent(LocTok),
        Termerror(__lalrpop_util::ErrorRecovery<usize, LocTok, LexicalError>),
        Nt_28_3cExpr_3e_20_22_2c_22_29(AST),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cExpr_3e_20_22_2c_22_29_2b(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29(AST),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2a(::std::vec::Vec<AST>),
        Nt_28_3cStmt_3e_20_22_3b_22_29_2b(::std::vec::Vec<AST>),
        NtApp(AST),
        NtAtom(AST),
        NtBlock(AST),
        NtBlockBody(AST),
        NtColl(AST),
        NtComma_3cExpr_3e(Vec<AST>),
        NtCommaPlus_3cExpr_3e(Vec<AST>),
        NtDef(AST),
        NtDefPat(AST),
        NtExpr(AST),
        NtExpr_3f(::std::option::Option<AST>),
        NtNonApp(AST),
        NtPat(AST),
        NtPat_2a(::std::vec::Vec<AST>),
        NtPat_2b(::std::vec::Vec<AST>),
        NtSemiColon_3cStmt_3e(Vec<AST>),
        NtStmt(AST),
        NtStmt_3f(::std::option::Option<AST>),
        Nt____BlockBody(AST),
        Nt____Expr(AST),
    }
    const __ACTION: &'static [i32] = &[
        // State 0
        8, 0, 0, 0, 0, 9, 0, 10, 0, 0, 11, 0, 0,
        // State 1
        -28, -28, -28, -28, 0, -28, -28, -28, 0, -28, -28, 0, 0,
        // State 2
        -34, -34, -34, -34, -34, -34, -34, -34, 0, -34, -34, 0, 0,
        // State 3
        -32, -32, -32, -32, -32, -32, -32, -32, 0, -32, -32, 0, 0,
        // State 4
        -33, -33, -33, -33, -33, -33, -33, -33, 0, -33, -33, 0, 0,
        // State 5
        8, 0, 0, 0, 0, 9, 0, 10, 0, 0, 11, 0, 0,
        // State 6
        -29, -29, -29, -29, 0, -29, -29, -29, 0, -29, -29, 0, 0,
        // State 7
        8, -20, 0, 0, 0, 9, 0, 10, 0, 0, 11, 0, 0,
        // State 8
        8, 0, 0, 0, 0, 9, -20, 10, 0, 0, 11, 0, 0,
        // State 9
        8, 0, 0, 0, 0, 9, 0, 10, 20, 0, 11, 0, 0,
        // State 10
        -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, -12, 0, 0,
        // State 11
        -11, -11, -11, -11, -11, -11, -11, -11, 0, -11, -11, 0, 0,
        // State 12
        8, -22, 0, 0, 0, 9, -22, 10, 0, 0, 11, 0, 0,
        // State 13
        0, 22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 14
        8, -19, 23, 0, 0, 9, -19, 10, 0, 0, 11, 0, 0,
        // State 15
        0, 0, 0, 0, 0, 0, 24, 0, 0, 0, 0, 0, 0,
        // State 16
        8, 0, 0, 0, 0, 9, 0, 10, 0, 0, 11, 0, 0,
        // State 17
        0, 0, 0, 0, 0, 0, 0, 0, 0, 26, 0, 0, 0,
        // State 18
        8, 0, 23, 0, 0, 9, 0, 10, 0, -23, 11, 0, 0,
        // State 19
        8, 0, 0, 0, 0, 9, 0, 31, 32, 0, 11, 0, 0,
        // State 20
        8, -21, 33, 0, 0, 9, -21, 10, 0, 0, 11, 0, 0,
        // State 21
        -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, 0, 0,
        // State 22
        -4, -4, 0, 0, 0, -4, -4, -4, 0, 0, -4, 0, 0,
        // State 23
        -17, -17, -17, -17, -17, -17, -17, -17, -17, -17, -17, 0, 0,
        // State 24
        8, 0, 33, 0, 0, 9, 0, 10, 0, -24, 11, 0, 0,
        // State 25
        -18, -18, -18, -18, -18, -18, -18, -18, -18, -18, -18, 0, 0,
        // State 26
        -36, 0, 0, 0, 0, -36, 0, -36, -36, 0, -36, 0, 0,
        // State 27
        -35, 0, 0, 0, 0, -35, 0, -35, -35, 0, -35, 0, 0,
        // State 28
        -39, 0, 0, 0, 0, -39, 0, -39, -39, 0, -39, 0, 0,
        // State 29
        8, 0, 0, 0, 0, 9, 0, 31, 35, 0, 11, 0, 0,
        // State 30
        8, 0, 0, 0, 0, 9, 0, 10, 0, 0, 11, 0, 0,
        // State 31
        8, 0, 0, 0, 0, 9, 0, 10, 0, -42, 11, 0, 0,
        // State 32
        -5, -5, 0, 0, 0, -5, -5, -5, 0, 0, -5, 0, 0,
        // State 33
        -40, 0, 0, 0, 0, -40, 0, -40, -40, 0, -40, 0, 0,
        // State 34
        8, 0, 0, 0, 0, 9, 0, 10, 0, -42, 11, 0, 0,
        // State 35
        8, 0, 0, 0, 0, 9, 0, 10, 0, -44, 11, 0, 0,
        // State 36
        -28, 0, 0, -28, -26, -28, 0, -28, 0, -28, -28, 0, 0,
        // State 37
        -34, 0, 0, -34, -36, -34, 0, -34, 0, -34, -34, 0, 0,
        // State 38
        -33, 0, 0, -33, -35, -33, 0, -33, 0, -33, -33, 0, 0,
        // State 39
        0, 0, 0, -46, 0, 0, 0, 0, 0, -46, 0, 0, 0,
        // State 40
        0, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 41
        8, 0, 0, -45, 0, 9, 0, 10, 0, -45, 11, 0, 0,
        // State 42
        0, 0, 0, 0, -27, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 43
        0, 0, 0, 0, 0, 0, 0, 0, 0, 49, 0, 0, 0,
        // State 44
        0, 0, 0, 50, 0, 0, 0, 0, 0, -41, 0, 0, 0,
        // State 45
        0, 0, 0, 0, 0, 0, 0, 0, 0, 51, 0, 0, 0,
        // State 46
        0, 0, 0, 52, 0, 0, 0, 0, 0, -43, 0, 0, 0,
        // State 47
        8, 0, 0, 0, 0, 9, 0, 10, 0, 0, 11, 0, 0,
        // State 48
        -13, -13, -13, -13, -13, -13, -13, -13, 0, -13, -13, 0, 0,
        // State 49
        -9, 0, 0, 0, 0, -9, 0, -9, 0, -9, -9, 0, 0,
        // State 50
        -14, -14, -14, -14, -14, -14, -14, -14, 0, -14, -14, 0, 0,
        // State 51
        -10, 0, 0, 0, 0, -10, 0, -10, 0, -10, -10, 0, 0,
        // State 52
        8, 0, 0, -25, 0, 9, 0, 10, 0, -25, 11, 0, 0,
    ];
    const __EOF_ACTION: &'static [i32] = &[
        0,
        -28,
        -34,
        -32,
        -33,
        -50,
        -29,
        0,
        0,
        0,
        -12,
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
        -16,
        0,
        -17,
        0,
        -18,
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
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        -13,
        0,
        -14,
        0,
        0,
    ];
    const __GOTO: &'static [i32] = &[
        // State 0
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 5, 0, 0, 0, 0, 6, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 1
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 2
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 3
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 5
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 6
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 7
        0, 0, 13, 0, 0, 0, 2, 3, 4, 0, 5, 14, 0, 0, 0, 15, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 8
        0, 0, 13, 0, 0, 0, 2, 3, 4, 0, 5, 16, 0, 0, 0, 15, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 9
        0, 0, 17, 0, 0, 0, 2, 3, 4, 0, 5, 0, 18, 0, 0, 19, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 10
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 11
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 12
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 5, 0, 0, 0, 0, 21, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 13
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 14
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 15
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 16
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 5, 0, 0, 0, 0, 25, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 17
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 18
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 19
        0, 0, 0, 0, 0, 0, 0, 27, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 29, 0, 30, 0, 0, 0, 0, 0,
        // State 20
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 21
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 22
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 23
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 24
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 25
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 26
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 27
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 28
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 29
        0, 0, 0, 0, 0, 0, 0, 27, 0, 0, 28, 0, 0, 0, 0, 0, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0,
        // State 30
        0, 0, 17, 0, 0, 0, 2, 3, 4, 0, 5, 0, 18, 0, 0, 19, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 31
        0, 0, 0, 0, 0, 36, 37, 38, 4, 0, 39, 0, 0, 40, 41, 42, 0, 7, 43, 0, 0, 44, 45, 0, 0, 0,
        // State 32
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 33
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 34
        0, 0, 0, 0, 0, 36, 37, 38, 4, 0, 39, 0, 0, 40, 41, 42, 0, 7, 43, 0, 0, 46, 45, 0, 0, 0,
        // State 35
        0, 0, 0, 0, 0, 0, 37, 38, 4, 0, 39, 0, 0, 40, 41, 42, 0, 7, 43, 0, 0, 0, 47, 0, 0, 0,
        // State 36
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 37
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 38
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 39
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 40
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 41
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 42
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 43
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 44
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 45
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 46
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 47
        0, 0, 0, 0, 0, 0, 2, 3, 4, 0, 5, 0, 0, 0, 0, 53, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 48
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 49
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 50
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 51
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 52
        0, 0, 0, 0, 0, 0, 0, 3, 4, 0, 5, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    pub fn parse_Expr<
        __TOKEN: __ToTriple<Error=LexicalError>,
        __TOKENS: IntoIterator<Item=__TOKEN>,
    >(
        __tokens0: __TOKENS,
    ) -> Result<AST, __lalrpop_util::ParseError<usize, LocTok, LexicalError>>
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
                LocTok { tok: Tok::LParen, .. } if true => 0,
                LocTok { tok: Tok::RParen, .. } if true => 1,
                LocTok { tok: Tok::Comma, .. } if true => 2,
                LocTok { tok: Tok::Semicolon, .. } if true => 3,
                LocTok { tok: Tok::Eq, .. } if true => 4,
                LocTok { tok: Tok::LBracket, .. } if true => 5,
                LocTok { tok: Tok::RBracket, .. } if true => 6,
                LocTok { tok: Tok::LBrace, .. } if true => 7,
                LocTok { tok: Tok::Pipe, .. } if true => 8,
                LocTok { tok: Tok::RBrace, .. } if true => 9,
                LocTok { tok: Tok::Id(_), .. } if true => 10,
                LocTok { tok: Tok::Indent, .. } if true => 11,
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
                            __tok @ LocTok { tok: Tok::LParen, .. } => __Symbol::Term_22_28_22(__tok),
                            _ => unreachable!(),
                        },
                        1 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::RParen, .. } => __Symbol::Term_22_29_22(__tok),
                            _ => unreachable!(),
                        },
                        2 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Comma, .. } => __Symbol::Term_22_2c_22(__tok),
                            _ => unreachable!(),
                        },
                        3 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Semicolon, .. } => __Symbol::Term_22_3b_22(__tok),
                            _ => unreachable!(),
                        },
                        4 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Eq, .. } => __Symbol::Term_22_3d_22(__tok),
                            _ => unreachable!(),
                        },
                        5 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::LBracket, .. } => __Symbol::Term_22_5b_22(__tok),
                            _ => unreachable!(),
                        },
                        6 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::RBracket, .. } => __Symbol::Term_22_5d_22(__tok),
                            _ => unreachable!(),
                        },
                        7 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::LBrace, .. } => __Symbol::Term_22_7b_22(__tok),
                            _ => unreachable!(),
                        },
                        8 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Pipe, .. } => __Symbol::Term_22_7c_22(__tok),
                            _ => unreachable!(),
                        },
                        9 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::RBrace, .. } => __Symbol::Term_22_7d_22(__tok),
                            _ => unreachable!(),
                        },
                        10 => match __lookahead.1 {
                            LocTok { tok: Tok::Id(__tok0), .. } => __Symbol::TermId(__tok0),
                            _ => unreachable!(),
                        },
                        11 => match __lookahead.1 {
                            __tok @ LocTok { tok: Tok::Indent, .. } => __Symbol::TermIndent(__tok),
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
        __lookahead_start: Option<&usize>,
        __states: &mut ::std::vec::Vec<i32>,
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>,
        _: ::std::marker::PhantomData<()>,
    ) -> Option<Result<AST,__lalrpop_util::ParseError<usize, LocTok, LexicalError>>>
    {
        let __nonterminal = match -__action {
            1 => {
                // (<Expr> ",") = Expr, "," => ActionFn(37);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action37::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__nt), __end));
                0
            }
            2 => {
                // (<Expr> ",")* =  => ActionFn(35);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action35::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            3 => {
                // (<Expr> ",")* = (<Expr> ",")+ => ActionFn(36);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action36::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__nt), __end));
                1
            }
            4 => {
                // (<Expr> ",")+ = Expr, "," => ActionFn(42);
                let __sym1 = __pop_Term_22_2c_22(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action42::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            5 => {
                // (<Expr> ",")+ = (<Expr> ",")+, Expr, "," => ActionFn(43);
                let __sym2 = __pop_Term_22_2c_22(__symbols);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action43::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__nt), __end));
                2
            }
            6 => {
                // (<Stmt> ";") = Stmt, ";" => ActionFn(32);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action32::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__nt), __end));
                3
            }
            7 => {
                // (<Stmt> ";")* =  => ActionFn(30);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action30::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            8 => {
                // (<Stmt> ";")* = (<Stmt> ";")+ => ActionFn(31);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action31::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__nt), __end));
                4
            }
            9 => {
                // (<Stmt> ";")+ = Stmt, ";" => ActionFn(48);
                let __sym1 = __pop_Term_22_3b_22(__symbols);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action48::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            10 => {
                // (<Stmt> ";")+ = (<Stmt> ";")+, Stmt, ";" => ActionFn(49);
                let __sym2 = __pop_Term_22_3b_22(__symbols);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action49::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__nt), __end));
                5
            }
            11 => {
                // App = Expr, NonApp => ActionFn(14);
                let __sym1 = __pop_NtNonApp(__symbols);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action14::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtApp(__nt), __end));
                6
            }
            12 => {
                // Atom = Id => ActionFn(13);
                let __sym0 = __pop_TermId(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtAtom(__nt), __end));
                7
            }
            13 => {
                // Block = "{", "|", "|", SemiColon<Stmt>, "}" => ActionFn(56);
                let __sym4 = __pop_Term_22_7d_22(__symbols);
                let __sym3 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __sym2 = __pop_Term_22_7c_22(__symbols);
                let __sym1 = __pop_Term_22_7c_22(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action56::<>(__sym0, __sym1, __sym2, __sym3, __sym4);
                let __states_len = __states.len();
                __states.truncate(__states_len - 5);
                __symbols.push((__start, __Symbol::NtBlock(__nt), __end));
                8
            }
            14 => {
                // Block = "{", "|", Pat+, "|", SemiColon<Stmt>, "}" => ActionFn(57);
                let __sym5 = __pop_Term_22_7d_22(__symbols);
                let __sym4 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __sym3 = __pop_Term_22_7c_22(__symbols);
                let __sym2 = __pop_NtPat_2b(__symbols);
                let __sym1 = __pop_Term_22_7c_22(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym5.2.clone();
                let __nt = super::__action57::<>(__sym0, __sym1, __sym2, __sym3, __sym4, __sym5);
                let __states_len = __states.len();
                __states.truncate(__states_len - 6);
                __symbols.push((__start, __Symbol::NtBlock(__nt), __end));
                8
            }
            15 => {
                // BlockBody = SemiColon<Stmt> => ActionFn(16);
                let __sym0 = __pop_NtSemiColon_3cStmt_3e(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action16::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtBlockBody(__nt), __end));
                9
            }
            16 => {
                // Coll = "(", Comma<Expr>, ")" => ActionFn(18);
                let __sym2 = __pop_Term_22_29_22(__symbols);
                let __sym1 = __pop_NtComma_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_28_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action18::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                10
            }
            17 => {
                // Coll = "[", Comma<Expr>, "]" => ActionFn(19);
                let __sym2 = __pop_Term_22_5d_22(__symbols);
                let __sym1 = __pop_NtComma_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_5b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action19::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                10
            }
            18 => {
                // Coll = "{", CommaPlus<Expr>, "}" => ActionFn(20);
                let __sym2 = __pop_Term_22_7d_22(__symbols);
                let __sym1 = __pop_NtCommaPlus_3cExpr_3e(__symbols);
                let __sym0 = __pop_Term_22_7b_22(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action20::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtColl(__nt), __end));
                10
            }
            19 => {
                // Comma<Expr> = Expr => ActionFn(52);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action52::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            20 => {
                // Comma<Expr> =  => ActionFn(53);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action53::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            21 => {
                // Comma<Expr> = (<Expr> ",")+, Expr => ActionFn(54);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action54::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            22 => {
                // Comma<Expr> = (<Expr> ",")+ => ActionFn(55);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action55::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtComma_3cExpr_3e(__nt), __end));
                11
            }
            23 => {
                // CommaPlus<Expr> = Expr => ActionFn(46);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action46::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtCommaPlus_3cExpr_3e(__nt), __end));
                12
            }
            24 => {
                // CommaPlus<Expr> = (<Expr> ",")+, Expr => ActionFn(47);
                let __sym1 = __pop_NtExpr(__symbols);
                let __sym0 = __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action47::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtCommaPlus_3cExpr_3e(__nt), __end));
                12
            }
            25 => {
                // Def = DefPat, "=", Expr => ActionFn(17);
                let __sym2 = __pop_NtExpr(__symbols);
                let __sym1 = __pop_Term_22_3d_22(__symbols);
                let __sym0 = __pop_NtDefPat(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action17::<>(__sym0, __sym1, __sym2);
                let __states_len = __states.len();
                __states.truncate(__states_len - 3);
                __symbols.push((__start, __Symbol::NtDef(__nt), __end));
                13
            }
            26 => {
                // DefPat = App => ActionFn(9);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtDefPat(__nt), __end));
                14
            }
            27 => {
                // DefPat = Pat => ActionFn(10);
                let __sym0 = __pop_NtPat(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action10::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtDefPat(__nt), __end));
                14
            }
            28 => {
                // Expr = App => ActionFn(4);
                let __sym0 = __pop_NtApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action4::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr(__nt), __end));
                15
            }
            29 => {
                // Expr = NonApp => ActionFn(5);
                let __sym0 = __pop_NtNonApp(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action5::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr(__nt), __end));
                15
            }
            30 => {
                // Expr? = Expr => ActionFn(33);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action33::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                16
            }
            31 => {
                // Expr? =  => ActionFn(34);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action34::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtExpr_3f(__nt), __end));
                16
            }
            32 => {
                // NonApp = Block => ActionFn(6);
                let __sym0 = __pop_NtBlock(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action6::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtNonApp(__nt), __end));
                17
            }
            33 => {
                // NonApp = Coll => ActionFn(7);
                let __sym0 = __pop_NtColl(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action7::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtNonApp(__nt), __end));
                17
            }
            34 => {
                // NonApp = Atom => ActionFn(8);
                let __sym0 = __pop_NtAtom(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action8::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtNonApp(__nt), __end));
                17
            }
            35 => {
                // Pat = Coll => ActionFn(11);
                let __sym0 = __pop_NtColl(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action11::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat(__nt), __end));
                18
            }
            36 => {
                // Pat = Atom => ActionFn(12);
                let __sym0 = __pop_NtAtom(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action12::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat(__nt), __end));
                18
            }
            37 => {
                // Pat* =  => ActionFn(24);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action24::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtPat_2a(__nt), __end));
                19
            }
            38 => {
                // Pat* = Pat+ => ActionFn(25);
                let __sym0 = __pop_NtPat_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action25::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat_2a(__nt), __end));
                19
            }
            39 => {
                // Pat+ = Pat => ActionFn(26);
                let __sym0 = __pop_NtPat(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action26::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtPat_2b(__nt), __end));
                20
            }
            40 => {
                // Pat+ = Pat+, Pat => ActionFn(27);
                let __sym1 = __pop_NtPat(__symbols);
                let __sym0 = __pop_NtPat_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action27::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtPat_2b(__nt), __end));
                20
            }
            41 => {
                // SemiColon<Stmt> = Stmt => ActionFn(58);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action58::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            42 => {
                // SemiColon<Stmt> =  => ActionFn(59);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action59::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            43 => {
                // SemiColon<Stmt> = (<Stmt> ";")+, Stmt => ActionFn(60);
                let __sym1 = __pop_NtStmt(__symbols);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action60::<>(__sym0, __sym1);
                let __states_len = __states.len();
                __states.truncate(__states_len - 2);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            44 => {
                // SemiColon<Stmt> = (<Stmt> ";")+ => ActionFn(61);
                let __sym0 = __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action61::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtSemiColon_3cStmt_3e(__nt), __end));
                21
            }
            45 => {
                // Stmt = Expr => ActionFn(2);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                22
            }
            46 => {
                // Stmt = Def => ActionFn(3);
                let __sym0 = __pop_NtDef(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt(__nt), __end));
                22
            }
            47 => {
                // Stmt? = Stmt => ActionFn(28);
                let __sym0 = __pop_NtStmt(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action28::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                23
            }
            48 => {
                // Stmt? =  => ActionFn(29);
                let __start = __symbols.last().map(|s| s.2.clone()).unwrap_or_default();
                let __end = __lookahead_start.cloned().unwrap_or_else(|| __start.clone());
                let __nt = super::__action29::<>(&__start, &__end);
                let __states_len = __states.len();
                __states.truncate(__states_len - 0);
                __symbols.push((__start, __Symbol::NtStmt_3f(__nt), __end));
                23
            }
            49 => {
                // __BlockBody = BlockBody => ActionFn(1);
                let __sym0 = __pop_NtBlockBody(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action1::<>(__sym0);
                let __states_len = __states.len();
                __states.truncate(__states_len - 1);
                __symbols.push((__start, __Symbol::Nt____BlockBody(__nt), __end));
                24
            }
            50 => {
                // __Expr = Expr => ActionFn(0);
                let __sym0 = __pop_NtExpr(__symbols);
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0::<>(__sym0);
                return Some(Ok(__nt));
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __state = *__states.last().unwrap() as usize;
        let __next_state = __GOTO[__state * 26 + __nonterminal] - 1;
        __states.push(__next_state);
        None
    }
    fn __pop_Term_22_28_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_28_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_29_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_29_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_2c_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_2c_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_3d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_3d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_5d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_5d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7b_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7b_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7c_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7c_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Term_22_7d_22<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Term_22_7d_22(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermId<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, String, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermId(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_TermIndent<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, LocTok, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::TermIndent(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Termerror<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, __lalrpop_util::ErrorRecovery<usize, LocTok, LexicalError>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Termerror(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cExpr_3e_20_22_2c_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cExpr_3e_20_22_2c_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt_28_3cStmt_3e_20_22_3b_22_29_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt_28_3cStmt_3e_20_22_3b_22_29_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtApp<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtApp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtAtom<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtAtom(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtBlock<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtBlock(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtBlockBody<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtBlockBody(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtColl<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtColl(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtComma_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtComma_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtCommaPlus_3cExpr_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtCommaPlus_3cExpr_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtDef<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtDef(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtDefPat<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtDefPat(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtExpr_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::option::Option<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtExpr_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtNonApp<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtNonApp(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPat<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPat(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPat_2a<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPat_2a(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtPat_2b<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::vec::Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtPat_2b(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtSemiColon_3cStmt_3e<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, Vec<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtSemiColon_3cStmt_3e(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_NtStmt_3f<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, ::std::option::Option<AST>, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::NtStmt_3f(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____BlockBody<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____BlockBody(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
    fn __pop_Nt____Expr<
    >(
        __symbols: &mut ::std::vec::Vec<(usize,__Symbol<>,usize)>
    ) -> (usize, AST, usize) {
        match __symbols.pop().unwrap() {
            (__l, __Symbol::Nt____Expr(__v), __r) => (__l, __v, __r),
            _ => panic!("symbol type mismatch")
        }
    }
}
pub use self::__parse__Expr::parse_Expr;

pub fn __action0<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action1<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action2<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action3<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action4<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action5<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action6<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action7<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action8<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action9<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action10<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action11<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action12<
>(
    (_, __0, _): (usize, AST, usize),
) -> AST
{
    (__0)
}

pub fn __action13<
>(
    (_, __0, _): (usize, String, usize),
) -> AST
{
    AST::Symbol(__0)
}

pub fn __action14<
>(
    (_, f, _): (usize, AST, usize),
    (_, arg, _): (usize, AST, usize),
) -> AST
{
    AST::App(Box::new(f), Box::new(arg))
}

pub fn __action15<
>(
    (_, _, _): (usize, LocTok, usize),
    (_, _, _): (usize, LocTok, usize),
    (_, formals, _): (usize, ::std::vec::Vec<AST>, usize),
    (_, _, _): (usize, LocTok, usize),
    (_, stmts, _): (usize, Vec<AST>, usize),
    (_, _, _): (usize, LocTok, usize),
) -> AST
{
    AST::Block(formals, stmts)
}

pub fn __action16<
>(
    (_, stmts, _): (usize, Vec<AST>, usize),
) -> AST
{
    AST::Block(vec![], stmts)
}

pub fn __action17<
>(
    (_, pat, _): (usize, AST, usize),
    (_, _, _): (usize, LocTok, usize),
    (_, value, _): (usize, AST, usize),
) -> AST
{
    AST::Def(Box::new(pat), Box::new(value))
}

pub fn __action18<
>(
    (_, _, _): (usize, LocTok, usize),
    (_, elems, _): (usize, Vec<AST>, usize),
    (_, _, _): (usize, LocTok, usize),
) -> AST
{
    AST::Tuple(elems)
}

pub fn __action19<
>(
    (_, _, _): (usize, LocTok, usize),
    (_, elems, _): (usize, Vec<AST>, usize),
    (_, _, _): (usize, LocTok, usize),
) -> AST
{
    AST::Array(elems)
}

pub fn __action20<
>(
    (_, _, _): (usize, LocTok, usize),
    (_, elems, _): (usize, Vec<AST>, usize),
    (_, _, _): (usize, LocTok, usize),
) -> AST
{
    AST::Set(elems)
}

pub fn __action21<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
    (_, e, _): (usize, AST, usize),
) -> Vec<AST>
{
    {
        let mut v = v;
        v.push(e);
        v
    }
}

pub fn __action22<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
    (_, e, _): (usize, ::std::option::Option<AST>, usize),
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

pub fn __action23<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
    (_, e, _): (usize, ::std::option::Option<AST>, usize),
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

pub fn __action24<
>(
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::vec::Vec<AST>
{
    vec![]
}

pub fn __action25<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
) -> ::std::vec::Vec<AST>
{
    v
}

pub fn __action26<
>(
    (_, __0, _): (usize, AST, usize),
) -> ::std::vec::Vec<AST>
{
    vec![__0]
}

pub fn __action27<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
    (_, e, _): (usize, AST, usize),
) -> ::std::vec::Vec<AST>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action28<
>(
    (_, __0, _): (usize, AST, usize),
) -> ::std::option::Option<AST>
{
    Some(__0)
}

pub fn __action29<
>(
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::option::Option<AST>
{
    None
}

pub fn __action30<
>(
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::vec::Vec<AST>
{
    vec![]
}

pub fn __action31<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
) -> ::std::vec::Vec<AST>
{
    v
}

pub fn __action32<
>(
    (_, __0, _): (usize, AST, usize),
    (_, _, _): (usize, LocTok, usize),
) -> AST
{
    (__0)
}

pub fn __action33<
>(
    (_, __0, _): (usize, AST, usize),
) -> ::std::option::Option<AST>
{
    Some(__0)
}

pub fn __action34<
>(
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::option::Option<AST>
{
    None
}

pub fn __action35<
>(
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::vec::Vec<AST>
{
    vec![]
}

pub fn __action36<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
) -> ::std::vec::Vec<AST>
{
    v
}

pub fn __action37<
>(
    (_, __0, _): (usize, AST, usize),
    (_, _, _): (usize, LocTok, usize),
) -> AST
{
    (__0)
}

pub fn __action38<
>(
    (_, __0, _): (usize, AST, usize),
) -> ::std::vec::Vec<AST>
{
    vec![__0]
}

pub fn __action39<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
    (_, e, _): (usize, AST, usize),
) -> ::std::vec::Vec<AST>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action40<
>(
    (_, __0, _): (usize, AST, usize),
) -> ::std::vec::Vec<AST>
{
    vec![__0]
}

pub fn __action41<
>(
    (_, v, _): (usize, ::std::vec::Vec<AST>, usize),
    (_, e, _): (usize, AST, usize),
) -> ::std::vec::Vec<AST>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action42<
>(
    __0: (usize, AST, usize),
    __1: (usize, LocTok, usize),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action37(
        __0,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action38(
        __temp0,
    )
}

pub fn __action43<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
    __1: (usize, AST, usize),
    __2: (usize, LocTok, usize),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action37(
        __1,
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action39(
        __0,
        __temp0,
    )
}

pub fn __action44<
>(
    __0: (usize, ::std::option::Option<AST>, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action35(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action22(
        __temp0,
        __0,
    )
}

pub fn __action45<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
    __1: (usize, ::std::option::Option<AST>, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action36(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action22(
        __temp0,
        __1,
    )
}

pub fn __action46<
>(
    __0: (usize, AST, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action35(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        __temp0,
        __0,
    )
}

pub fn __action47<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
    __1: (usize, AST, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action36(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        __temp0,
        __1,
    )
}

pub fn __action48<
>(
    __0: (usize, AST, usize),
    __1: (usize, LocTok, usize),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action32(
        __0,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action40(
        __temp0,
    )
}

pub fn __action49<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
    __1: (usize, AST, usize),
    __2: (usize, LocTok, usize),
) -> ::std::vec::Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action32(
        __1,
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action41(
        __0,
        __temp0,
    )
}

pub fn __action50<
>(
    __0: (usize, ::std::option::Option<AST>, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action30(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action23(
        __temp0,
        __0,
    )
}

pub fn __action51<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
    __1: (usize, ::std::option::Option<AST>, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action31(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action23(
        __temp0,
        __1,
    )
}

pub fn __action52<
>(
    __0: (usize, AST, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action33(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action44(
        __temp0,
    )
}

pub fn __action53<
>(
    __lookbehind: &usize,
    __lookahead: &usize,
) -> Vec<AST>
{
    let __start0 = __lookbehind.clone();
    let __end0 = __lookahead.clone();
    let __temp0 = __action34(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action44(
        __temp0,
    )
}

pub fn __action54<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
    __1: (usize, AST, usize),
) -> Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action33(
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action45(
        __0,
        __temp0,
    )
}

pub fn __action55<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
) -> Vec<AST>
{
    let __start0 = __0.2.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action34(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action45(
        __0,
        __temp0,
    )
}

pub fn __action56<
>(
    __0: (usize, LocTok, usize),
    __1: (usize, LocTok, usize),
    __2: (usize, LocTok, usize),
    __3: (usize, Vec<AST>, usize),
    __4: (usize, LocTok, usize),
) -> AST
{
    let __start0 = __1.2.clone();
    let __end0 = __2.0.clone();
    let __temp0 = __action24(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action15(
        __0,
        __1,
        __temp0,
        __2,
        __3,
        __4,
    )
}

pub fn __action57<
>(
    __0: (usize, LocTok, usize),
    __1: (usize, LocTok, usize),
    __2: (usize, ::std::vec::Vec<AST>, usize),
    __3: (usize, LocTok, usize),
    __4: (usize, Vec<AST>, usize),
    __5: (usize, LocTok, usize),
) -> AST
{
    let __start0 = __2.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action25(
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action15(
        __0,
        __1,
        __temp0,
        __3,
        __4,
        __5,
    )
}

pub fn __action58<
>(
    __0: (usize, AST, usize),
) -> Vec<AST>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action28(
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action50(
        __temp0,
    )
}

pub fn __action59<
>(
    __lookbehind: &usize,
    __lookahead: &usize,
) -> Vec<AST>
{
    let __start0 = __lookbehind.clone();
    let __end0 = __lookahead.clone();
    let __temp0 = __action29(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action50(
        __temp0,
    )
}

pub fn __action60<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
    __1: (usize, AST, usize),
) -> Vec<AST>
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action28(
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action51(
        __0,
        __temp0,
    )
}

pub fn __action61<
>(
    __0: (usize, ::std::vec::Vec<AST>, usize),
) -> Vec<AST>
{
    let __start0 = __0.2.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action29(
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action51(
        __0,
        __temp0,
    )
}

pub trait __ToTriple<> {
    type Error;
    fn to_triple(value: Self) -> Result<(usize,LocTok,usize),Self::Error>;
}

impl<> __ToTriple<> for (usize, LocTok, usize) {
    type Error = LexicalError;
    fn to_triple(value: Self) -> Result<(usize,LocTok,usize),LexicalError> {
        Ok(value)
    }
}
impl<> __ToTriple<> for Result<(usize, LocTok, usize),LexicalError> {
    type Error = LexicalError;
    fn to_triple(value: Self) -> Result<(usize,LocTok,usize),LexicalError> {
        value
    }
}
