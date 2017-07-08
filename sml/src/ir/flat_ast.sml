(* TODO: Add Var.t variant (or something like that) for fn ptrs *)

(* Like AuglessAst, but alphatized and closure converted. *)
functor FlatAst(V : TO_DOC) :> sig
    structure Expr : FLAT_EXPR

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of (expr, bind) AuglessStmt.t
    and bind = Bind of Pos.t * expr DNF.t * bind_stmt vector
    and bind_stmt = FixBS of (expr, V.t) BindStmt1.t

    (* TODO: replace case-Name.t:s with proc-wide formals record *)
    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * Name.t * Name.t * bind * expr) vector }

    type program = { procs: proc vector (* TODO: use a map *)
                   , main: stmt vector }

    val unwrapE : expr -> (expr, stmt) Expr.t
    val unwrapS : stmt -> (expr, bind) AuglessStmt.t
    val unwrapBS : bind_stmt -> (expr, V.t) BindStmt1.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val procToDoc : proc -> PPrint.doc
    val toDoc : program -> PPrint.doc
end where type Expr.Var.t = V.t = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = FlatExpr(V)

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of (expr, bind) AuglessStmt.t
    and bind = Bind of Pos.t * expr DNF.t * bind_stmt vector
    and bind_stmt = FixBS of (expr, V.t) BindStmt1.t

    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * Name.t * Name.t * bind * expr) vector }

    type program = { procs: proc vector
                   , main: stmt vector }

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt
    fun unwrapBS (FixBS bstmt) = bstmt

    fun exprToDoc expr = Expr.toDoc exprToDoc stmtToDoc (unwrapE expr)
    and stmtToDoc stmt = AuglessStmt.toDoc exprToDoc bindToDoc (unwrapS stmt)
    and bindToDoc (Bind (_, cond, bs)) =
        let val bindingToDoc = BindStmt1.toDoc exprToDoc V.toDoc
                             o unwrapBS
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

    fun stmtsToDoc stmts =
        (case Vector.length stmts
         of 1 => stmtToDoc (Vector.sub (stmts, 0))
          | _ => let fun step (stmt, acc) =
                             acc ^^ PP.semi <$> stmtToDoc stmt
                     val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                     val rstmts = VectorSlice.slice(stmts, 1, NONE)
                 in VectorSlice.foldl step stmtDoc rstmts end)

    fun procToDoc {name = name, clovers = clovers, cases = cases} =
        let fun caseToDoc (_, _, _, bind, body) = (* FIXME: print everything *)
                bindToDoc bind <+> PP.text "=>" <+> exprToDoc body
            fun caseStep (cs, acc) = acc ^^ PP.semi <$> caseToDoc cs
            val c = Vector.sub (cases, 0)
            val rcs = VectorSlice.slice(cases, 1, NONE)
            fun cloverStep (clover, acc) = acc <+> Name.toDoc clover ^^ PP.text ","
        in
            Name.toDoc name ^^
                PP.braces (Vector.foldl cloverStep PP.empty clovers) <+>
                    PP.text "=" <+>
                        PP.braces (VectorSlice.foldl caseStep (caseToDoc c) rcs)
        end

    fun toDoc { procs = procs, main = main } =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in Vector.foldl step PP.empty procs <$> stmtsToDoc main
        end
end (* structure FlatAst *)
