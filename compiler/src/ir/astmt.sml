signature ASTMT = sig
    structure LVar : TO_DOC

    datatype 'expr t = Def of Pos.t * LVar.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    val mapExprs : ('e -> 'e) -> 'e t -> 'e t

    val pos : ('e -> Pos.t) -> 'e t -> Pos.t
    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end

functor StmtFn(LV : TO_DOC) : ASTMT = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    structure LVar = LV

    datatype 'expr t = Def of Pos.t * LVar.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    fun mapExprs f =
        fn Def (pos, var, expr) => Def (pos, var, f expr)
         | Guard (pos, dnf) => Guard (pos, DNF.map f dnf)
         | Expr expr => Expr (f expr)

    fun pos exprPos =
        fn Def (pos, _, _) => pos
         | Guard (pos, _) => pos
         | Expr expr => exprPos expr

    fun toDoc exprToDoc =
        fn Def (_, var, expr) => LVar.toDoc var <+> PP.text "=" <+> exprToDoc expr
         | Guard (_, dnf) => PP.text "@guard" <+> PP.parens (DNF.toDoc exprToDoc dnf)
         | Expr expr => exprToDoc expr
end
