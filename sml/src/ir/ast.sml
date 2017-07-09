signature STMT = sig
    type 'expr t
    val pos : ('e -> Pos.t) -> 'e t -> Pos.t
    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end

signature AST = sig
    structure Stmt : STMT

    datatype expr = FixE of (expr, stmt, Var.t, stmt vector) Expr.t
    and stmt = FixS of expr Stmt.t

    val unwrapE : expr -> (expr, stmt, Var.t, stmt vector) Expr.t
    val unwrapS : stmt -> expr Stmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val toDoc : stmt vector -> PPrint.doc
end

functor AstF(S : STMT) :> AST where type 'expr Stmt.t = 'expr S.t = struct
    structure Stmt = S

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

structure Ast = AstF(AStmt)
structure AuglessVarStmt = AuglessStmt(Var)
structure AuglessAst = AstF(AuglessVarStmt)
