structure AStmt :> sig
    datatype 'expr t = Def of Pos.t * Var.t * 'expr
                     | AugDef of Pos.t * Var.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    val pos : ('e -> Pos.t) -> 'e t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    datatype 'expr t = Def of Pos.t * Var.t * 'expr
                     | AugDef of Pos.t * Var.t * 'expr
                     | Guard of Pos.t * 'expr DNF.t
                     | Expr of 'expr

    fun pos _ (Def (pos, _, _)) = pos
      | pos _ (AugDef (pos, _, _)) = pos
      | pos _ (Guard (pos, _)) = pos
      | pos exprPos (Expr expr) = exprPos expr

    fun toDoc exprToDoc (Def (_, var, expr)) =
        Var.toDoc var <+> PP.text "=" <+> exprToDoc expr
      | toDoc exprToDoc (AugDef (_, var, expr)) =
        Var.toDoc var <+> PP.text "+=" <+> exprToDoc expr
      | toDoc exprToDoc (Guard (_, dnf)) =
        PP.text "@guard" <+> PP.parens (DNF.toDoc exprToDoc dnf)
      | toDoc exprToDoc (Expr expr) = exprToDoc expr
end
