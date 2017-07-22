signature STMT = sig
    type 'expr t
    val pos : ('e -> Pos.t) -> 'e t -> Pos.t
    val toDoc : ('e -> PPrint.doc) -> 'e t -> PPrint.doc
end

signature AST = sig
    structure Stmt : STMT

    datatype expr = FixE of (expr, stmt, stmt vector) Expr.t
    and stmt = FixS of expr Stmt.t

    val app : Pos.t * expr * expr vector -> (expr, stmt, stmt vector) Expr.t

    val unwrapE : expr -> (expr, stmt, stmt vector) Expr.t
    val unwrapS : stmt -> expr Stmt.t

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val toDoc : (expr, stmt) Block.t -> PPrint.doc
end

functor AstF(S : STMT) :> AST where type 'expr Stmt.t = 'expr S.t = struct
    val PrimApp = Expr.PrimApp
    val Triv = Expr.Triv
    structure PP = PPrint
    val op^^ = PP.^^

    structure Stmt = S

    datatype expr = FixE of (expr, stmt, stmt vector) Expr.t
    and stmt = FixS of expr Stmt.t

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

    fun exprToDoc (FixE expr) =
        let val stmtVecToDoc = PP.punctuate (PP.semi ^^ PP.line) o Vector.map stmtToDoc
        in Expr.toDoc exprToDoc stmtToDoc stmtVecToDoc expr
        end
    and stmtToDoc (FixS stmt) = Stmt.toDoc exprToDoc stmt
    and toDoc stmts = Block.toDoc exprToDoc stmtToDoc stmts
end

structure Ast = AstF(AStmt)
structure AuglessVarStmt = AuglessStmt(AVar)
structure AuglessAst = AstF(AuglessVarStmt)
