structure AStmt :> sig
    datatype 'expr t = Def of Pos.t * CVar.t * 'expr
                     | AugDef of Pos.t * CVar.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    val pos : ('e -> Pos.t) -> 'e t -> Pos.t
    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    datatype 'expr t = Def of Pos.t * CVar.t * 'expr
                     | AugDef of Pos.t * CVar.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    fun pos exprPos =
        fn Def (pos, _, _) => pos
         | AugDef (pos, _, _) => pos
         | Guard (pos, _) => pos
         | Expr expr => exprPos expr

    fun toDoc exprToDoc =
        fn Def (_, var, expr) => CVar.toDoc var <+> PP.text "=" <+> exprToDoc expr
         | AugDef (_, var, expr) => CVar.toDoc var <+> PP.text "+=" <+> exprToDoc expr
         | Guard (_, dnf) => PP.text "@guard" <+> PP.parens (DNF.toDoc exprToDoc dnf)
         | Expr expr => exprToDoc expr
end

signature AUGLESS_STMT = sig
    structure Var : TO_DOC

    datatype 'expr t = Def of Pos.t * Var.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    val pos : ('e -> Pos.t) -> 'e t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end

functor AuglessStmtFn(V : TO_DOC) :> AUGLESS_STMT where type Var.t = V.t = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    structure Var = V

    datatype 'expr t = Def of Pos.t * Var.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    fun pos exprPos =
        fn Def (pos, _, _) => pos
         | Guard (pos, _) => pos
         | Expr expr => exprPos expr

    fun toDoc exprToDoc =
        fn Def (_, var, expr) => Var.toDoc var <+> PP.text "=" <+> exprToDoc expr
         | Guard (_, dnf) => PP.text "@guard" <+> PP.parens (DNF.toDoc exprToDoc dnf)
         | Expr expr => exprToDoc expr
end

structure AuglessStmt = AuglessStmtFn(CVar)
structure FlatStmt0 = AuglessStmt
structure FlatStmt1 = AuglessStmtFn(Name)
