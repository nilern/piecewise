(* HACK: mostly copypasted from Ast. A functor should be used instead. *)

structure AuglessAst :> sig
    structure Stmt : AUGLESS_STMT where type Var.t = Var.t

    datatype expr = FixE of (expr, stmt, Var.t, stmt vector) Expr.t
    and stmt = FixS of expr Stmt.t

    val unwrapE : expr -> (expr, stmt, Var.t, stmt vector) Expr.t
    val unwrapS : stmt -> expr Stmt.t

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

    structure Stmt = AuglessStmt(Var)

    datatype expr = FixE of (expr, stmt, Var.t, stmt vector) Expr.t
    and stmt = FixS of expr Stmt.t

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = Expr.pos o unwrapE
    val stmtPos = Stmt.pos exprPos o unwrapS

    fun exprToDoc (FixE expr) =
        Expr.toDoc exprToDoc stmtToDoc Var.toDoc toDoc expr
    and stmtToDoc (FixS stmt) = Stmt.toDoc exprToDoc stmt
    and toDoc stmts = Expr.stmtsToDoc stmtToDoc stmts
end
