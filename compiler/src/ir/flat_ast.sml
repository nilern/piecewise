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

functor FlatAstFn(structure E: FLAT_EXPR
                  structure S: AUGLESS_STMT
                  structure A: TO_DOC) :> sig
    structure Expr : FLAT_EXPR
    structure Stmt : AUGLESS_STMT
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
end where type ('e, 's) Expr.t = ('e, 's) E.t
      and type Expr.Triv.t = E.Triv.t
      and type 'e Stmt.t = 'e S.t
      and type Stmt.Var.t = S.Var.t
      and type Argv.t = A.t = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = E
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

structure FlatAst0 = FlatAstFn(structure E = FlatExpr0
                               structure S = FlatStmt0
                               structure A = Argv0)
structure FlatAst1 = FlatAstFn(structure E = FlatExpr1
                               structure S = FlatStmt1
                               structure A = Argv1)
