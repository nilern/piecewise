(* TODO: Add Var.t variant (or something like that) for fn ptrs *)

functor FlatAst(V : TO_DOC) :> sig
    structure Expr : FLAT_EXPR
    structure Stmt : AUGLESS_STMT

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of expr Stmt.t

    (* TODO: replace case-Name.t:s with proc-wide formals record *)
    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * Name.t * Name.t * stmt vector * expr) vector }

    type program = { procs: proc vector (* TODO: use a map *)
                   , main: stmt vector }

    val unwrapE : expr -> (expr, stmt) Expr.t
    val unwrapS : stmt -> expr Stmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val procToDoc : proc -> PPrint.doc
    val toDoc : program -> PPrint.doc
end where type Expr.Var.t = V.t and type Stmt.Var.t = V.t = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = FlatExpr(V)
    structure Stmt = AuglessStmt(V)

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of expr Stmt.t

    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * Name.t * Name.t * stmt vector * expr) vector }

    type program = { procs: proc vector
                   , main: stmt vector }

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    fun exprPos (FixE expr) = Expr.pos expr
    and stmtPos (FixS stmt) = Stmt.pos exprPos stmt

    fun exprToDoc (FixE expr) = Expr.toDoc exprToDoc stmtToDoc expr
    and stmtToDoc (FixS stmt) = Stmt.toDoc exprToDoc stmt

    and stmtsToDoc stmts =
        case Vector.length stmts
        of 1 => stmtToDoc (Vector.sub (stmts, 0))
         | _ => let fun step (stmt, acc) =
                        acc ^^ PP.semi <$> stmtToDoc stmt
                    val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                    val rstmts = VectorSlice.slice(stmts, 1, NONE)
                in VectorSlice.foldl step stmtDoc rstmts end

    fun procToDoc {name = name, clovers = clovers, cases = cases} =
        let fun caseToDoc (_, _, _, prologue, body) = (* FIXME: print everything *)
                stmtsToDoc prologue <+> PP.text "=>" <+> exprToDoc body
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
