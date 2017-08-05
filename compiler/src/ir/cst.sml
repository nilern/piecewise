structure Cst :> sig
    structure Expr : sig
        datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * CVar.t option
                                                   * ('prologue * 'expr) vector
                                             | Block of Pos.t * ('expr, 'stmt) Block.t
                                             | App of Pos.t * 'expr * 'expr vector
                                             | PrimApp of Pos.t * Primop.t * 'expr vector
                                             | Triv of Pos.t * CTriv.t

        val pos : ('e, 's, 'p) t -> Pos.t
        val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('p -> PPrint.doc) -> ('e, 's, 'p) t
                  -> PPrint.doc
    end

    structure Stmt : sig
        datatype ('expr, 'bind) t = Def of 'bind * 'expr
                                  | AugDef of 'bind * 'expr
                                  | Expr of 'expr

        val pos : ('e -> Pos.t) -> ('b -> Pos.t) -> ('e, 'b) t -> Pos.t
        val toDoc : ('e -> PPrint.doc) -> ('b -> PPrint.doc) -> ('e, 'b) t -> PPrint.doc
    end

    datatype expr = FixE of (expr, stmt, prologue) Expr.t
    and stmt = FixS of (expr, bind) Stmt.t
    and bind = Bind of expr * expr option
    and prologue = Prolog of expr vector * expr option

    val wrapE : (expr, stmt, prologue) Expr.t -> expr
    val wrapS : (expr, bind) Stmt.t -> stmt

    val unwrapE : expr -> (expr, stmt, prologue) Expr.t
    val unwrapS : stmt -> (expr, bind) Stmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val toDoc : (expr, stmt) Block.t -> PPrint.doc
end = struct
    val PrimApp = Expr.PrimApp
    val Triv = Expr.Triv
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = struct
        datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * CVar.t option
                                                   * ('prologue * 'expr) vector
                                             | Block of Pos.t * ('expr, 'stmt) Block.t
                                             | App of Pos.t * 'expr * 'expr vector
                                             | PrimApp of Pos.t * Primop.t * 'expr vector
                                             | Triv of Pos.t * CTriv.t

        val pos = fn Fn (pos, _, _) => pos
                   | Block (pos, _) => pos
                   | App (pos, _, _) => pos
                   | PrimApp (pos, _, _) => pos
                   | Triv (pos, _) => pos

        fun toDoc exprToDoc stmtToDoc prologueToDoc =
            fn Fn (_, name, cases) =>
               let fun caseToDoc (prologue, body) =
                       prologueToDoc prologue <+> PP.text "=>" <+> exprToDoc body
               in PP.braces (PP.align (OptionExt.toDoc CVar.toDoc name <$>
                                       (PP.punctuate (PP.semi ^^ PP.line)
                                                     (Vector.map caseToDoc cases))))
               end
             | Block (_, block) => PP.braces (PP.nest 4 (Block.toDoc exprToDoc stmtToDoc block))
             | App (_, f, args) =>
               let fun step (arg, acc) = acc <+> exprToDoc arg
               in PP.parens (PP.align (Vector.foldl step (exprToDoc f) args))
               end
             | PrimApp (_, po, args) =>
               let fun step (arg, acc) = acc <+> exprToDoc arg
               in PP.parens (PP.align (Vector.foldl step (Primop.toDoc po) args))
               end
             | Triv (_, t) => CTriv.toDoc t
    end

    structure Stmt = struct
        datatype ('expr, 'bind) t = Def of 'bind * 'expr
                                  | AugDef of 'bind * 'expr
                                  | Expr of 'expr

        fun pos exprPos bindPos =
            fn Def (bind, _) => bindPos bind
             | AugDef (bind, _) => bindPos bind
             | Expr expr => exprPos expr

        fun toDoc exprToDoc bindToDoc =
            fn Def (pat, expr) => bindToDoc pat <+> PP.text "=" <+> exprToDoc expr
             | AugDef (pat, expr) => bindToDoc pat <+> PP.text "+=" <+> exprToDoc expr
             | Expr expr => exprToDoc expr
    end

    datatype expr = FixE of (expr, stmt, prologue) Expr.t
    and stmt = FixS of (expr, bind) Stmt.t
    and bind = Bind of expr * expr option
    and prologue = Prolog of expr vector * expr option

    val wrapE = FixE
    val wrapS = FixS

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = Expr.pos o unwrapE
    fun bindPos (Bind (pat, _)) = exprPos pat
    val stmtPos = Stmt.pos exprPos bindPos o unwrapS

    fun exprToDoc expr = Expr.toDoc exprToDoc stmtToDoc prologueToDoc (unwrapE expr)
    and stmtToDoc stmt = Stmt.toDoc exprToDoc bindToDoc (unwrapS stmt)
    and bindToDoc (Bind (pat, cond)) =
        exprToDoc pat ^^ (OptionExt.toDoc (fn ce => PP.space ^^ PP.text "|" <+> exprToDoc ce) cond)
    and prologueToDoc (Prolog (pats, cond)) =
        PP.punctuate PP.space (Vector.map exprToDoc pats) ^^
            (OptionExt.toDoc (fn ce => PP.space ^^ PP.text "|" <+> exprToDoc ce) cond)

    val toDoc = Block.toDoc exprToDoc stmtToDoc
end
