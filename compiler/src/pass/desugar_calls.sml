structure DesugarCalls :> sig
    exception Argc of Pos.t * Primop.t * int * int

    val desugar : (AuglessAst.expr, AuglessAst.stmt) Block.t
                -> (AuglessAst.expr, AuglessAst.stmt) Block.t
end = struct
    structure AAst = AuglessAst
    structure Expr = AAst.Expr
    structure Triv = Expr.Triv
    structure LVar = BaseVar
    val Call = Expr.Call
    val PrimCall = Expr.PrimCall
    val Triv = Expr.Triv
    val Var = Triv.Var
    val FixE = AAst.FixE
    val mapStmtExprs = AAst.mapStmtExprs
    val mapExprExprs = AAst.mapExprExprs

    exception Argc of Pos.t * Primop.t * int * int

    fun desugarExpr expr =
        case mapExprExprs desugarExpr expr
        of AAst.FixE (Expr.Call (pos, f, args)) =>
           let val apply =
                   FixE (Triv (pos, Var (RVar.Current (LVar.Lex (Name.fromString "apply")))))
               val argtup = FixE (PrimCall (pos, Primop.Tuple, args))
               val args = Vector.fromList [f, argtup]
           in FixE (Call (pos, apply, args))
           end
         | AAst.FixE (Expr.PrimCall (pos, Primop.Apply, args)) =>
           if Vector.length args = 2
           then FixE (Call (pos, Vector.sub (args, 0), Vector.fromList [Vector.sub (args, 1)]))
           else raise Argc (pos, Primop.Apply, 2, Vector.length args)
         | expr => expr

    val desugar = Block.mapExprs mapStmtExprs desugarExpr
end
