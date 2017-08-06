signature AST = sig
    structure Expr : HIGHER_ORDER_EXPR
    structure Stmt : ASTMT

    datatype expr = FixE of (expr, stmt, prologue) Expr.t
    and stmt = FixS of expr Stmt.t
    and prologue = Prolog of expr DNF.t * stmt vector

    (*val app : Pos.t * expr * expr vector -> (expr, stmt, prologue) Expr.t*)

    val unwrapE : expr -> (expr, stmt, prologue) Expr.t
    val unwrapS : stmt -> expr Stmt.t

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

structure Ast = AstFn(struct
    structure RV = BaseVar
    structure LV = LVar
end)

structure AuglessAst = AstFn(struct
    structure RV = RVar
    structure LV = BaseVar
end)
