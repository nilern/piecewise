
functor FlatAst(T : TRIV) :> sig
    structure Expr : FLAT_EXPR
    structure Stmt : AUGLESS_STMT

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of expr Stmt.t

    (* TODO: replace case-Name.t:s with proc-wide formals record *)
    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * Name.t * Name.t * stmt vector * expr) vector }

    type program = { procs: proc vector (* TODO: use a map *)
                   , main: (expr, stmt) Block.t }

    val unwrapE : expr -> (expr, stmt) Expr.t
    val unwrapS : stmt -> expr Stmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val procToDoc : proc -> PPrint.doc
    val toDoc : program -> PPrint.doc
end where type Expr.Triv.t = T.t and type Stmt.Var.t = T.Var.t = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = FlatExpr(T)
    structure Stmt = AuglessStmt(T.Var)

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of expr Stmt.t

    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * Name.t * Name.t * stmt vector * expr) vector }

    type program = { procs: proc vector
                   , main: (expr, stmt) Block.t }

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    fun exprPos (FixE expr) = Expr.pos expr
    and stmtPos (FixS stmt) = Stmt.pos exprPos stmt

    fun exprToDoc (FixE expr) = Expr.toDoc exprToDoc stmtToDoc expr
    and stmtToDoc (FixS stmt) = Stmt.toDoc exprToDoc stmt

    fun procToDoc {name = name, clovers = clovers, cases = cases} =
        let fun caseToDoc (_, _, _, prologue, body) = (* FIXME: print everything *)
                let val prologueToDoc = PP.punctuate (PP.semi ^^ PP.line) o Vector.map stmtToDoc
                in prologueToDoc prologue <+> PP.text "=>" <+> exprToDoc body
                end
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
        in Vector.foldl step PP.empty procs <$> Block.toDoc exprToDoc stmtToDoc main
        end
end (* structure FlatAst *)
