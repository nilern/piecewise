structure CST0 :> sig
    datatype expr = FixE of (expr, stmt, bind) Expr0.t
    and stmt = FixS of (expr, bind) Stmt0.t
    and bind = Bind of expr * expr option

    val wrapE : (expr, stmt, bind) Expr0.t -> expr
    val wrapS : (expr, bind) Stmt0.t -> stmt

    val unwrapE : expr -> (expr, stmt, bind) Expr0.t
    val unwrapS : stmt -> (expr, bind) Stmt0.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>

    datatype expr = FixE of (expr, stmt, bind) Expr0.t
    and stmt = FixS of (expr, bind) Stmt0.t
    and bind = Bind of expr * expr option

    val wrapE = FixE
    val wrapS = FixS

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = Expr0.pos o unwrapE
    fun bindPos (Bind (pat, _)) = exprPos pat
    val stmtPos = Stmt0.pos exprPos bindPos o unwrapS

    fun exprToDoc expr =
        Expr0.toDoc exprToDoc stmtToDoc bindToDoc (unwrapE expr)
    and stmtToDoc stmt =
        Stmt0.toDoc exprToDoc bindToDoc (unwrapS stmt)
    and bindToDoc (Bind (pat, cond)) =
        exprToDoc pat ^^ (case cond
                          of SOME ce => PP.space ^^ PP.text "|" <+> exprToDoc ce
                           | NONE => PP.empty)
end
