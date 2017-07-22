structure Block :> sig
    type 'stmt stmts = 'stmt vector
    type ('expr, 'stmt) t = 'stmt stmts * 'expr

    val stmtsToDoc : ('s -> PPrint.doc) -> 's stmts -> PPrint.doc
    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('e, 's) t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<$> = PP.<$>

    type 'stmt stmts = 'stmt vector
    type ('expr, 'stmt) t = 'stmt stmts * 'expr

    fun stmtsToDoc stmtToDoc = PP.punctuate (PP.semi ^^ PP.line) o Vector.map stmtToDoc

    fun toDoc exprToDoc stmtToDoc (stmts, expr) =
        stmtsToDoc stmtToDoc stmts ^^ PP.semi <$> exprToDoc expr
end
