structure Block :> sig
    type 'stmt stmts = 'stmt vector
    type ('expr, 'stmt) t = 'stmt stmts * 'expr

    val mapExprs : (('e -> 'e) -> 's -> 's) -> ('e -> 'e) -> ('e, 's) t -> ('e, 's) t

    val pos : ('e -> Pos.t) -> ('s -> Pos.t) -> ('e, 's) t -> Pos.t
    val stmtsToDoc : ('s -> PPrint.doc) -> 's stmts -> PPrint.doc
    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('e, 's) t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<$> = PP.<$>

    type 'stmt stmts = 'stmt vector
    type ('expr, 'stmt) t = 'stmt stmts * 'expr

    fun mapExprs mapStmtExprs f (stmts, expr) = (Vector.map (mapStmtExprs f) stmts, f expr)

    fun pos exprPos stmtPos (stmts, expr) =
        if Vector.length stmts > 0
        then stmtPos (Vector.sub (stmts, 0))
        else exprPos expr

    fun stmtsToDoc stmtToDoc = PP.punctuate (PP.semi ^^ PP.line) o Vector.map stmtToDoc

    fun toDoc exprToDoc stmtToDoc (stmts, expr) =
        stmtsToDoc stmtToDoc stmts ^^ PP.semi <$> exprToDoc expr
end
