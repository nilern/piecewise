structure CStmt :> sig
    datatype ('expr, 'bind) t = Def of 'bind * 'expr
                              | AugDef of 'bind * 'expr
                              | Expr of 'expr

    val pos : ('e -> Pos.t) -> ('b -> Pos.t) -> ('e, 'b) t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> ('b -> PPrint.doc) -> ('e, 'b) t
              -> PPrint.doc
end = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    datatype ('expr, 'bind) t = Def of 'bind * 'expr
                              | AugDef of 'bind * 'expr
                              | Expr of 'expr

    fun pos _ bindPos (Def (bind, _)) = bindPos bind
      | pos _ bindPos (AugDef (bind, _)) = bindPos bind
      | pos exprPos _ (Expr expr) = exprPos expr

    fun toDoc exprToDoc bindToDoc (Def (pat, expr)) =
        bindToDoc pat <+> PP.text "=" <+> exprToDoc expr
      | toDoc exprToDoc bindToDoc (AugDef (pat, expr)) =
        bindToDoc pat <+> PP.text "+=" <+> exprToDoc expr
      | toDoc exprToDoc _ (Expr expr) = exprToDoc expr
end