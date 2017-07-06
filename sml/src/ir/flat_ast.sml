(* TODO: Add Var.t variant (or something like that) for fn ptrs *)

(* Like CST0, but alphatized and closure converted. *)
structure FlatAst :> sig
    datatype expr = FixE of (expr, stmt) FlatExpr.t
    and stmt = FixS of (expr, bind) AuglessStmt.t
    and bind = Bind of Pos.t * expr DNF.t * bind_stmt vector
    and bind_stmt = FixBS of (expr, Var.t) BindStmt1.t

    (* TODO: use record type for cases to disambiguate the Name.t:s *)
    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * Name.t * Name.t * bind * expr) vector }

    type 'a program = { procs: proc vector, main: 'a }

    val mapMain : ('a -> 'b) -> 'a program -> 'b program
    val append : ('a -> 'b -> 'c) -> 'a program -> 'b program -> 'c program
    val map : ('a -> 'b program) -> 'a vector -> 'b vector program
    val flatMap : ('a -> 'b vector program) -> 'a vector -> 'b vector program

    val unwrapE : expr -> (expr, stmt) FlatExpr.t
    val unwrapS : stmt -> (expr, bind) AuglessStmt.t
    val unwrapBS : bind_stmt -> (expr, Var.t) BindStmt1.t

    val trivial : 'a -> 'a program
    val trivialE : (expr, stmt) FlatExpr.t -> expr program
    val trivialS : (expr, bind) AuglessStmt.t -> stmt program

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val procToDoc : proc -> PPrint.doc
    val toDoc : stmt vector program -> PPrint.doc
end = struct

structure PP = PPrint
val op^^ = PP.^^
val op<+> = PP.<+>
val op<$> = PP.<$>

datatype expr = FixE of (expr, stmt) FlatExpr.t
and stmt = FixS of (expr, bind) AuglessStmt.t
and bind = Bind of Pos.t * expr DNF.t * bind_stmt vector
and bind_stmt = FixBS of (expr, Var.t) BindStmt1.t

type proc = { name: Name.t
            , clovers: Name.t vector
            , cases: (Name.t * Name.t * Name.t * bind * expr) vector }

type 'a program = { procs: proc vector, main: 'a }

fun mapMain f {procs = procs, main = main} = { procs = procs, main = f main}

fun append f {procs = procs, main = main} {procs = procs', main = main'} =
    { procs = Vector.concat [procs, procs']
    , main = f main main' }

fun map (f: 'a -> 'b program) (vs: 'a vector) : 'b vector program =
    let val vs' = Vector.map f vs
    in
        { procs = VectorExt.flatMap #procs vs'
        , main = Vector.map #main vs' }
    end

fun flatMap (f: 'a -> 'b vector program) (vs: 'a vector) : 'b vector program =
    let val vs' = Vector.map f vs
    in
        { procs = VectorExt.flatMap #procs vs'
        , main = VectorExt.flatMap #main vs' }
    end

fun unwrapE (FixE expr) = expr
fun unwrapS (FixS stmt) = stmt
fun unwrapBS (FixBS bstmt) = bstmt

fun trivial v = { procs = VectorExt.empty (), main = v }
val trivialE = mapMain FixE o trivial
val trivialS = mapMain FixS o trivial

fun exprToDoc expr = FlatExpr.toDoc exprToDoc stmtToDoc (unwrapE expr)
and stmtToDoc stmt = AuglessStmt.toDoc exprToDoc bindToDoc (unwrapS stmt)
and bindToDoc (Bind (_, cond, bs)) =
    let val bindingToDoc = BindStmt1.toDoc exprToDoc Var.toDoc
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
    let fun caseToDoc (_, _, _, bind, body) =
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

fun toDoc (prog : (stmt vector) program) =
    let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
    in
        Vector.foldl step PP.empty (#procs prog) <$>
            stmtsToDoc (#main prog)
    end

end (* structure FlatCST0 *)
