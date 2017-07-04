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
    val Const = Expr.Const
    val Def = AuglessStmt.Def
    val Expr = AuglessStmt.Expr

    structure VarSet = BinarySetFn(type ord_key = Var.t
                                   val compare = Var.compare)

    fun elabExpr (AuglessAst.FixE expr) =
        FixE (case expr
              of Expr.Fn (pos, name, cases) =>
                 let fun elabCase (bind, body) =
                         (#1 (elabBind bind), elabExpr body)
                 in Fn (pos, name, Vector.map elabCase cases)
                 end
               | Expr.Block (pos, stmts) => Block (pos, elabStmts stmts)
               | Expr.App (pos, f, args) =>
                 App (pos, elabExpr f, Vector.map elabExpr args)
               | Expr.PrimApp (pos, po, args) =>
                 PrimApp (pos, po, Vector.map elabExpr args)
               | v as Expr.Var _ => v
               | c as Expr.Const _ => c)
    and elabStmt (AuglessAst.FixS stmt, (stmts', bounds)) =
        case stmt
        of AuglessStmt.Def (name, bind, expr) =>
           let val (bind', bounds') = elabBind bind
               val stmt' = FixS (Def (name, bind', elabExpr expr))
           in (VectorExt.conj stmts' stmt', VarSet.union (bounds, bounds'))
           end
         | AuglessStmt.Expr expr =>
           let val stmt' = FixS (Expr (elabExpr expr))
           in (VectorExt.conj stmts' stmt', bounds)
           end
    and elabStmts stmts =
        let val pos = AuglessAst.stmtPos (Vector.sub (stmts, 0))
            val (stmts', bounds) =
                Vector.foldl elabStmt (VectorExt.empty (), VarSet.empty) stmts
            fun newBoxDef var =
                let val name = Name.freshFromString "box"
                    val temp = FixE (Var (pos, Var.Lex name))
                    val bind =
                        AuglessAst.Bind
                            (pos, DNF.always (),
                             VectorExt.singleton
                                 (FixBS (BindStmt1.Def (var, temp))))
                    val alloc =
                        FixE (PrimApp (pos, Primop.Box, VectorExt.empty ()))
                in FixS (Def (name, bind, alloc))
                end
            val boxDefs =
                Vector.fromList (List.map newBoxDef (VarSet.listItems bounds))
        in VectorExt.concat boxDefs stmts'
        end
    and elabBind (AuglessAst.Bind (pos, dnf, bstmts)) =
        let fun elabBStmt (AuglessAst.FixBS bstmt, (bstmts', bounds)) =
                case bstmt
                of BindStmt1.Def (var, expr) =>
                   let val varExpr = FixE (Var (pos, var))
                       val assign =
                           FixE (PrimApp (pos, Primop.BSet,
                                          Vector.fromList [varExpr, expr]))
                       val stmt' = FixBS (BindStmt1.Expr assign)
                       val bounds' = VarSet.add (bounds, var)
                   in (VectorExt.conj bstmts' stmt', bounds')
                   end
                 | BindStmt1.Expr expr =>
                   let val stmt' = FixBS (BindStmt1.Expr (elabExpr expr))
                   in (VectorExt.conj bstmts' stmt', bounds)
                   end
            val bounds = VarSet.empty
            val (bstmts', bounds') =
                Vector.foldl elabBStmt (VectorExt.empty (), VarSet.empty) bstmts
        in (AuglessAst.Bind (pos, DNF.map elabExpr dnf, bstmts'), bounds')
        end

    val straighten = elabStmts
end
