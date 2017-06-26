structure Stmt0 :> sig
    datatype 'expr t = Def of 'expr * 'expr
                     | AugDef of 'expr * 'expr
                     | Expr of 'expr

    val pos : ('e -> Pos.t) -> 'e t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    datatype 'expr t = Def of 'expr * 'expr
                     | AugDef of 'expr * 'expr
                     | Expr of 'expr

    fun pos exprPos (Def (pat, _)) = exprPos pat
      | pos exprPos (AugDef (pat, _)) = exprPos pat
      | pos exprPos (Expr expr) = exprPos expr

    fun toDoc exprToDoc (Def (pat, expr)) =
        exprToDoc pat <+> PP.text "=" <+> exprToDoc expr
      | toDoc exprToDoc (AugDef (pat, expr)) =
        exprToDoc pat <+> PP.text "+=" <+> exprToDoc expr
      | toDoc exprToDoc (Expr expr) = exprToDoc expr
end
