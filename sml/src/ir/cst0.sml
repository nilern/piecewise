(* TODO: use plain strings instead of Name.t:s *)

structure CST0 :> sig
    datatype expr = FixE of (expr, stmt) Expr0.t
    and stmt = FixS of expr Stmt0.t

    val wrapE : (expr, stmt) Expr0.t -> expr
    val wrapS : expr Stmt0.t -> stmt

    val unwrapE : expr -> (expr, stmt) Expr0.t
    val unwrapS : stmt -> expr Stmt0.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
end = struct
    datatype expr = FixE of (expr, stmt) Expr0.t
    and stmt = FixS of expr Stmt0.t

    val wrapE = FixE
    val wrapS = FixS

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = Expr0.pos o unwrapE
    val stmtPos = Stmt0.pos exprPos o unwrapS

    fun exprToDoc expr = Expr0.toDoc exprToDoc stmtToDoc (unwrapE expr)
    and stmtToDoc stmt = Stmt0.toDoc exprToDoc (unwrapS stmt)
end
