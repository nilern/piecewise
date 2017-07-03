structure Cst :> sig
    datatype expr = FixE of (expr, stmt, bind) Expr.t
    and stmt = FixS of (expr, bind) CStmt.t
    and bind = Bind of expr * expr option

    val wrapE : (expr, stmt, bind) Expr.t -> expr
    val wrapS : (expr, bind) CStmt.t -> stmt

    val unwrapE : expr -> (expr, stmt, bind) Expr.t
    val unwrapS : stmt -> (expr, bind) CStmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>

    datatype expr = FixE of (expr, stmt, bind) Expr.t
    and stmt = FixS of (expr, bind) CStmt.t
    and bind = Bind of expr * expr option

    val wrapE = FixE
    val wrapS = FixS

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = Expr.pos o unwrapE
    fun bindPos (Bind (pat, _)) = exprPos pat
    val stmtPos = CStmt.pos exprPos bindPos o unwrapS

    fun exprToDoc expr =
        Expr.toDoc exprToDoc stmtToDoc bindToDoc (unwrapE expr)
    and stmtToDoc stmt =
        CStmt.toDoc exprToDoc bindToDoc (unwrapS stmt)
    and bindToDoc (Bind (pat, cond)) =
        exprToDoc pat ^^ (case cond
                          of SOME ce => PP.space ^^ PP.text "|" <+> exprToDoc ce
                           | NONE => PP.empty)
end
