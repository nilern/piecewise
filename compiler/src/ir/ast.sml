signature AST = sig
    structure Expr : HIGHER_ORDER_EXPR
    structure Stmt : ASTMT

    datatype expr = FixE of (expr, stmt, prologue) Expr.t
    and stmt = FixS of expr Stmt.t
    and prologue = Prolog of expr DNF.t * stmt vector

    val mapExprExprs : (expr -> expr) -> expr -> expr
    val mapStmtExprs : (expr -> expr) -> stmt -> stmt
    val mapPrologueExprs : (expr -> expr) -> prologue -> prologue

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t
    val blockPos : (expr, stmt) Block.t -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val toDoc : (expr, stmt) Block.t -> PPrint.doc
end

functor AstFn(structure RV : TO_DOC structure LV : TO_DOC) : AST = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = HigherOrderExpr(RV)
    structure Stmt = StmtFn(LV)

    datatype expr = FixE of (expr, stmt, prologue) Expr.t
    and stmt = FixS of expr Stmt.t
    and prologue = Prolog of expr DNF.t * stmt vector

    fun mapStmtExprs f (FixS stmt) = FixS (Stmt.mapExprs f stmt)
    fun mapPrologueExprs f (Prolog (cond, bindStmts)) =
        Prolog (DNF.map f cond, Vector.map (mapStmtExprs f) bindStmts)
    fun mapExprExprs f (FixE expr) = FixE (Expr.map mapStmtExprs mapPrologueExprs f expr)

    fun exprPos (FixE expr) = Expr.pos expr
    fun stmtPos (FixS stmt) = Stmt.pos exprPos stmt
    val blockPos = Block.pos exprPos stmtPos

    fun exprToDoc (FixE expr) = Expr.toDoc exprToDoc stmtToDoc prologueToDoc expr
    and stmtToDoc (FixS stmt) = Stmt.toDoc exprToDoc stmt
    and prologueToDoc (Prolog (cond, stmts)) =
        PP.text "@guard" <+> PP.parens (DNF.toDoc exprToDoc cond) ^^ PP.semi <$>
            PP.punctuate (PP.semi ^^ PP.line) (Vector.map stmtToDoc stmts)

    fun toDoc stmts = Block.toDoc exprToDoc stmtToDoc stmts
end

structure Ast = AstFn(struct
    structure RV = BaseVar
    structure LV = LVar
end)

structure AuglessAst = AstFn(struct
    structure RV = RVar
    structure LV = BaseVar
end)
