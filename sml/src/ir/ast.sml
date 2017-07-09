structure Ast :> sig
    datatype expr = FixE of (expr, stmt, Var.t, stmt vector) Expr.t
    and stmt = FixS of expr AStmt.t

    val unwrapE : expr -> (expr, stmt, Var.t, stmt vector) Expr.t
    val unwrapS : stmt -> expr AStmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val toDoc : stmt vector -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    datatype expr = FixE of (expr, stmt, Var.t, stmt vector) Expr.t
    and stmt = FixS of expr AStmt.t

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = Expr.pos o unwrapE
    val stmtPos = AStmt.pos exprPos o unwrapS

    fun exprToDoc (FixE expr) =
        Expr.toDoc exprToDoc stmtToDoc Var.toDoc toDoc expr
    and stmtToDoc (FixS stmt) = AStmt.toDoc exprToDoc stmt
    and toDoc stmts = Expr.stmtsToDoc stmtToDoc stmts
end
