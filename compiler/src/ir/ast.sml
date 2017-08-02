signature STMT = sig
    type 'expr t
    val pos : ('e -> Pos.t) -> 'e t -> Pos.t
    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end

signature AST = sig
    structure Stmt : STMT

    datatype expr = FixE of (expr, stmt, prologue) Expr.t
    and stmt = FixS of expr Stmt.t
    and prologue = Prolog of expr DNF.t * stmt vector

    val app : Pos.t * expr * expr vector -> (expr, stmt, prologue) Expr.t

    val unwrapE : expr -> (expr, stmt, prologue) Expr.t
    val unwrapS : stmt -> expr Stmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t
    val blockPos : (expr, stmt) Block.t -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val toDoc : (expr, stmt) Block.t -> PPrint.doc
end

functor AstFn(S : STMT) :> AST where type 'expr Stmt.t = 'expr S.t = struct
    val PrimApp = Expr.PrimApp
    val Triv = Expr.Triv
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Stmt = S

    datatype expr = FixE of (expr, stmt, prologue) Expr.t
    and stmt = FixS of expr Stmt.t
    and prologue = Prolog of expr DNF.t * stmt vector

    fun app (pos, f, args) =
        let val apply = FixE (Triv (pos, ATriv.Var (ATag.Lex, Name.fromString "apply")))
            val argsExpr = FixE (PrimApp (pos, Primop.Tuple, args))
            val metaArgs = FixE (PrimApp (pos, Primop.Tuple, Vector.fromList [f, argsExpr]))
        in PrimApp (pos, Primop.Call, Vector.fromList [apply, metaArgs])
        end

    fun unwrapE (FixE expr) = expr
    fun unwrapS (FixS stmt) = stmt

    val exprPos = Expr.pos o unwrapE
    val stmtPos = Stmt.pos exprPos o unwrapS
    val blockPos = Block.pos exprPos stmtPos

    fun exprToDoc (FixE expr) = Expr.toDoc exprToDoc stmtToDoc prologueToDoc expr
    and stmtToDoc (FixS stmt) = Stmt.toDoc exprToDoc stmt
    and prologueToDoc (Prolog (cond, stmts)) =
        PP.text "@guard" <+> PP.parens (DNF.toDoc exprToDoc cond) ^^ PP.semi <$>
            PP.punctuate (PP.semi ^^ PP.line) (Vector.map stmtToDoc stmts)

    fun toDoc stmts = Block.toDoc exprToDoc stmtToDoc stmts
end

structure Ast = AstFn(AStmt)
structure AuglessAst = AstFn(AuglessStmt)
