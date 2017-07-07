structure NameSet = BinarySetFn(type ord_key = Name.t
                                val compare = Name.compare)

structure StraightenScope :> sig
    val straighten : AuglessAst.stmt vector -> AuglessAst.stmt vector
end = struct
    val FixE = AuglessAst.FixE
    val FixS = AuglessAst.FixS
    val FixBS = AuglessAst.FixBS
    val Fn = Expr.Fn
    val Block = Expr.Block
    val App = Expr.App
    val PrimApp = Expr.PrimApp
    val Var = Expr.Var
    val Def = AuglessStmt.Def
    val Expr = AuglessStmt.Expr

    fun elabExpr (AuglessAst.FixE expr) =
        FixE (case expr
              of Expr.Fn (pos, name, cases) =>
                 let fun elabBStmt (AuglessAst.FixBS bstmt) =
                         FixBS (case bstmt
                                of BindStmt1.Def (var, expr) => BindStmt1.Def (var, elabExpr expr)
                                 | BindStmt1.Expr expr => BindStmt1.Expr (elabExpr expr))
                     fun elabCase (AuglessAst.Bind (pos, dnf, bstmts), body) =
                         ( AuglessAst.Bind (pos, DNF.map elabExpr dnf, Vector.map elabBStmt bstmts)
                         , elabExpr body )
                 in Fn (pos, name, Vector.map elabCase cases)
                 end
               | Expr.Block (pos, stmts) => Block (pos, elabStmts stmts)
               | Expr.App (pos, f, args) => App (pos, elabExpr f, Vector.map elabExpr args)
               | Expr.PrimApp (pos, po, args) => PrimApp (pos, po, Vector.map elabExpr args)
               | v as Expr.Var _ => v
               | c as Expr.Const _ => c)

    and elabStmts stmts =
        let fun elabStmt (AuglessAst.FixS stmt, (stmts', lvars)) =
                case stmt
                of AuglessStmt.Def (temp, bind, expr) =>
                   let val (bind', lvars') = elabBind (bind, lvars)
                       val stmt' = FixS (Def (temp, bind', elabExpr expr))
                   in (VectorExt.conj stmts' stmt', lvars')
                   end
                 | AuglessStmt.Expr expr =>
                   let val stmt' = FixS (Expr (elabExpr expr))
                   in (VectorExt.conj stmts' stmt', lvars)
                   end
            val pos = AuglessAst.stmtPos (Vector.sub (stmts, 0))
            val init = (VectorExt.empty (), NameSet.empty)
            val (stmts', lvars) = Vector.foldl elabStmt init stmts
            val boxAlloc = FixE (PrimApp (pos, Primop.Box, VectorExt.empty ()))
            fun newBoxDef name =
                let val temp = Name.freshFromString "box"
                    val tExpr = FixE (Var (pos, Var.Lex temp))
                    val bind =
                        AuglessAst.Bind
                            (pos, DNF.always (),
                             VectorExt.singleton
                                 (FixBS (BindStmt1.Def (Var.Lex name, tExpr))))
                in FixS (Def (temp, bind, boxAlloc))
                end
            val boxDefs =
                Vector.fromList (List.map newBoxDef (NameSet.listItems lvars))
        in VectorExt.concat boxDefs stmts'
        end

    and elabBind (AuglessAst.Bind (pos, dnf, bstmts), lvars) =
        let fun elabBStmt (AuglessAst.FixBS bstmt, (bstmts', lvars)) =
                case bstmt
                of BindStmt1.Def (var as Var.Lex name, expr) =>
                   let val varExpr = FixE (Var (pos, var))
                       val assign = FixE (PrimApp (pos, Primop.BSet,
                                                   Vector.fromList [varExpr, elabExpr expr]))
                       val stmt' = FixBS (BindStmt1.Expr assign)
                       val stmts' = VectorExt.conj bstmts' stmt'
                   in
                       case var
                       of Var.Lex name =>
                          (stmts', NameSet.add (lvars, name))
                        | Var.Dyn name =>
                          (stmts', lvars)
                   end
                 | BindStmt1.Def (Var.Dyn name, expr) =>
                   let val stmt' = FixBS (BindStmt1.Def (Var.Dyn name, elabExpr expr))
                   in (VectorExt.conj bstmts' stmt', lvars)
                   end
                 | BindStmt1.Expr expr =>
                   let val stmt' = FixBS (BindStmt1.Expr (elabExpr expr))
                   in (VectorExt.conj bstmts' stmt', lvars)
                   end
            val init = (VectorExt.empty (), lvars)
            val (bstmts', lvars') = Vector.foldl elabBStmt init bstmts
        in
            (AuglessAst.Bind (pos, DNF.map elabExpr dnf, bstmts'), lvars')
        end

    val straighten = elabStmts
end
