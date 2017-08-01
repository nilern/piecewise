structure Cst :> sig
    datatype expr = FixE of (expr, stmt, prologue) CExpr.t
    and stmt = FixS of (expr, bind) CStmt.t
    and bind = Bind of expr * expr option
    and prologue = Prolog of expr vector * expr option

    val wrapE : (expr, stmt, prologue) CExpr.t -> expr
    val wrapS : (expr, bind) CStmt.t -> stmt

    val unwrapE : expr -> (expr, stmt, prologue) CExpr.t
    val unwrapS : stmt -> (expr, bind) CStmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val toDoc : (expr, stmt) Block.t -> PPrint.doc
end = struct
    val PrimApp = CExpr.PrimApp
    val Triv = CExpr.Triv
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>

    datatype expr = FixE of (expr, stmt, prologue) CExpr.t
    and stmt = FixS of (expr, bind) CStmt.t
    and bind = Bind of expr * expr option
    and prologue = Prolog of expr vector * expr option

    val wrapE = FixE
    val wrapS = FixS

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = CExpr.pos o unwrapE
    fun bindPos (Bind (pat, _)) = exprPos pat
    val stmtPos = CStmt.pos exprPos bindPos o unwrapS

    fun exprToDoc expr = CExpr.toDoc exprToDoc stmtToDoc prologueToDoc (unwrapE expr)
    and stmtToDoc stmt = CStmt.toDoc exprToDoc bindToDoc (unwrapS stmt)
    and bindToDoc (Bind (pat, cond)) =
        exprToDoc pat ^^
            (OptionExt.toDoc (fn ce => PP.space ^^ PP.text "|" <+> exprToDoc ce) cond)
    and prologueToDoc (Prolog (pats, cond)) =
        PP.punctuate PP.space (Vector.map exprToDoc pats) ^^
            (OptionExt.toDoc (fn ce => PP.space ^^ PP.text "|" <+> exprToDoc ce) cond)

    val toDoc = Block.toDoc exprToDoc stmtToDoc
end
