structure NameMap = BinaryMapFn(type ord_key = Name.t
                                val compare = Name.compare)

structure Argv0 = struct
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>

    type t = {self: Name.t, params: Name.t}

    fun toDoc {self = self, params = params} =
        PPrint.parens (PPrint.text "self =" <+> Name.toDoc self ^^ PPrint.text "," <+>
                           PPrint.text "params =" <+> Name.toDoc params)
end

structure Argv1 = struct
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>

    type t = {self: Name.t, params: Name.t, denv: Name.t}

    fun toDoc {self = self, params = params, denv = denv} =
        PPrint.parens (PPrint.text "self =" <+> Name.toDoc self ^^ PPrint.text "," <+>
                           PPrint.text "params =" <+> Name.toDoc params ^^ PPrint.text "," <+>
                               PPrint.text "denv =" <+> Name.toDoc denv)
end

functor FlatAstFn(structure T: TRIV
                  structure S: ASTMT
                  structure A: TO_DOC) : sig
    structure Expr : sig
        structure Triv : TRIV

        datatype ('expr, 'stmt) t = Block of Pos.t * ('expr, 'stmt) Block.t
                                  | PrimApp of Pos.t * Primop.t * 'expr vector
                                  | Triv of Pos.t * Triv.t

        val pos : ('expr, 'stmt) t -> Pos.t

        val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('e, 's) t -> PPrint.doc
    end
    structure Stmt : ASTMT
    structure Argv : TO_DOC

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of expr Stmt.t

    type prologue = expr DNF.t * stmt vector

    type proc = { name: Name.t
                , clovers: Name.t vector
                , args: Argv.t
                , cases: (prologue * expr) vector }

    type program = { procs: proc NameMap.map
                   , main: (expr, stmt) Block.t }

    val unwrapE : expr -> (expr, stmt) Expr.t
    val unwrapS : stmt -> expr Stmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t
    val blockPos : (expr, stmt) Block.t -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val procToDoc : proc -> PPrint.doc
    val toDoc : program -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = struct
        structure PP = PPrint
        val op^^ = PP.^^
        val op<+> = PP.<+>
        val op<$> = PP.<$>

        structure Triv = T

        datatype ('expr, 'stmt) t = Block of Pos.t * ('expr, 'stmt) Block.t
                                  | PrimApp of Pos.t * Primop.t * 'expr vector
                                  | Triv of Pos.t * Triv.t

        fun pos (Block (pos, _)) = pos
          | pos (PrimApp (pos, _, _)) = pos
          | pos (Triv (pos, _)) = pos

        fun toDoc exprToDoc stmtToDoc =
            fn Block (_, block) => PP.braces (PP.nest 4 (Block.toDoc exprToDoc stmtToDoc block))
             | PrimApp (_, po, args) =>
               let fun step (arg, acc) = acc <+> exprToDoc arg
               in PP.parens (PP.align (Vector.foldl step (Primop.toDoc po) args))
               end
             | Triv (_, t) => Triv.toDoc t
    end

    structure Stmt = S
    structure Argv = A

    datatype expr = FixE of (expr, stmt) Expr.t
    and stmt = FixS of expr Stmt.t

    type prologue = expr DNF.t * stmt vector

    type proc = { name: Name.t
                , clovers: Name.t vector
                , args: Argv.t
                , cases: (prologue * expr) vector }

    type program = { procs: proc NameMap.map
                   , main: (expr, stmt) Block.t }

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    fun exprPos (FixE expr) = Expr.pos expr
    and stmtPos (FixS stmt) = Stmt.pos exprPos stmt
    val blockPos = Block.pos exprPos stmtPos

    fun exprToDoc (FixE expr) = Expr.toDoc exprToDoc stmtToDoc expr
    and stmtToDoc (FixS stmt) = Stmt.toDoc exprToDoc stmt

    fun procToDoc {name = name, clovers = clovers, args = args, cases = cases} =
        let fun caseToDoc ((cond, bindStmts), body) =
                PP.text "case" <+> DNF.toDoc exprToDoc cond ^^ PP.text ":" <$>
                    Block.toDoc exprToDoc stmtToDoc (bindStmts, body)
            val nameDoc = Name.toDoc name
            val cloversDoc = PP.braces (PP.punctuate (PP.text ", ") (Vector.map Name.toDoc clovers))
            val argsDoc = Argv.toDoc args
            val casesDoc = PP.punctuate (PP.semi ^^ PP.line) (Vector.map caseToDoc cases)
        in
            nameDoc ^^ cloversDoc ^^ argsDoc <+> PP.text "=" <+> PP.lBrace <$>
                PP.nest 4 casesDoc <$> PP.rBrace
        end

    fun toDoc { procs = procs, main = main } =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in NameMap.foldl step PP.empty procs <$> Block.toDoc exprToDoc stmtToDoc main
        end
end

structure FlatAst0 = FlatAstFn(structure T = FlatTriv0
                               structure S = FlatStmt0
                               structure A = Argv0)
structure FlatAst1 = FlatAstFn(structure T = FlatTriv1
                               structure S = FlatStmt1
                               structure A = Argv1)
