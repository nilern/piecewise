structure DnfCst :> sig
    datatype expr = FixE of (expr, stmt, bind) Expr0.t
    and stmt = FixS of (expr, bind) Stmt1.t
    and bind = Bind of Pos.t * expr DNF.t * bind_stmt vector
    and bind_stmt = FixBS of (expr, Expr0.Var.t) Stmt0.t

    val unwrapE : expr -> (expr, stmt, bind) Expr0.t
    val unwrapS : stmt -> (expr, bind) Stmt1.t
    val unwrapBS : bind_stmt -> (expr, Expr0.Var.t) Stmt0.t

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

    datatype expr = FixE of (expr, stmt, bind) Expr0.t
    and stmt = FixS of (expr, bind) Stmt1.t
    and bind = Bind of Pos.t * expr DNF.t * bind_stmt vector
    and bind_stmt = FixBS of (expr, Expr0.Var.t) Stmt0.t

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt
    fun unwrapBS (FixBS stmt) = stmt

    val exprPos = Expr0.pos o unwrapE
    fun bindPos (Bind (pos, _, _)) = pos
    val stmtPos = Stmt1.pos exprPos bindPos o unwrapS

    fun exprToDoc (FixE expr) = Expr0.toDoc exprToDoc stmtToDoc bindToDoc expr
    and stmtToDoc (FixS stmt) = Stmt1.toDoc exprToDoc bindToDoc stmt
    and bindToDoc (Bind (_, cond, bs)) =
        let val bindingToDoc = Stmt0.toDoc exprToDoc Expr0.Var.toDoc o unwrapBS
            val bindingDocs =
                case Vector.length bs
                of 0 => PP.text "{}"
                 | 1 => PP.braces (bindingToDoc (Vector.sub (bs, 0)))
                 | _ => let fun step (binding, acc) =
                                acc ^^ PP.semi <$> bindingToDoc binding
                            val bDoc = bindingToDoc (Vector.sub (bs, 0))
                            val rbs = VectorSlice.slice(bs, 1, NONE)
                            val bDocs = VectorSlice.foldl step bDoc rbs
                        in
                            PP.braces (PP.align bDocs)
                        end
        in
            bindingDocs <+> PP.text "|" <+> DNF.toDoc exprToDoc cond
        end

    fun toDoc stmts =
        let val stmt = Vector.sub (stmts, 0)
            val rstmts = VectorSlice.slice (stmts, 1, NONE)
            fun step (stmt, acc) = acc ^^ PP.semi <$> stmtToDoc stmt
        in
            VectorSlice.foldl step (stmtToDoc stmt) rstmts
        end
end
