structure Stmt1 :> sig
    datatype ('expr, 'bind) t = Def of Name.t * 'bind * 'expr
                              | Expr of 'expr

    val pos : ('e -> Pos.t) -> ('b -> Pos.t) -> ('e, 'b) t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> ('b -> PPrint.doc) -> ('e, 'b) t
              -> PPrint.doc
end = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    datatype ('expr, 'bind) t = Def of Name.t * 'bind * 'expr
                              | Expr of 'expr

    fun pos _ bindPos (Def (_, bind, _)) = bindPos bind
      | pos exprPos _ (Expr expr) = exprPos expr

    fun toDoc exprToDoc bindToDoc (Def (triv, pat, expr)) =
        bindToDoc pat <+> PP.text "=" <+> Name.toDoc triv <+> PP.text "=" <+>
            exprToDoc expr
      | toDoc exprToDoc _ (Expr expr) = exprToDoc expr
end
