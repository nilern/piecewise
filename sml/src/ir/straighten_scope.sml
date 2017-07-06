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

    structure NameSet = BinarySetFn(type ord_key = Name.t
                                    val compare = Name.compare)

    fun boxAllocAt pos = FixE (PrimApp (pos, Primop.Box, VectorExt.empty ()))

    fun dynFrameDef pos dvars =
        let val fname = Name.fromString "denv"
            val ftemp = Name.freshFromString "denv"
            val ffdef = FixBS (BindStmt1.Def ( Var.Lex fname
                                             , FixE (Var (pos, Var.Lex ftemp))))
            val fbind =
                AuglessAst.Bind ( pos
                                , DNF.always ()
                                , VectorExt.singleton ffdef )
            val boxAlloc = boxAllocAt pos
            fun dePair name =
                    Vector.fromList [ FixE (Const ( pos
                                                  , Const.Symbol
                                                     (Name.toString name)))
                                   , boxAlloc ]
            val dePairs = (* OPTIMIZE: *)
                VectorExt.flatMap dePair
                                  (Vector.fromList
                                      (NameSet.listItems dvars))
            val falloc = FixE (PrimApp (pos, Primop.DEnv, dePairs))
        in FixS (Def (ftemp, fbind, falloc))
        end

    fun dynFrameBindDef pos dvars =
        let val fname = Name.fromString "denv"
            val boxAlloc = boxAllocAt pos
            fun dePair name =
                Vector.fromList [ FixE (Const ( pos
                                      , Const.Symbol
                                         (Name.toString name)))
                       , boxAlloc ]
            val dePairs = (* OPTIMIZE: *)
                VectorExt.flatMap dePair
                      (Vector.fromList
                          (NameSet.listItems dvars))
            val falloc = FixE (PrimApp (pos, Primop.DEnv, dePairs))
        in FixBS (BindStmt1.Def (Var.Lex fname, falloc))
        end

    fun elabExpr (AuglessAst.FixE expr) =
        FixE (case expr
              of Expr.Fn (pos, name, cases) =>
                 let fun elabBStmt (AuglessAst.FixBS bstmt, (stmts', dvars)) =
                         case bstmt
                         of BindStmt1.Def (var as Var.Dyn name, expr) =>
                            let val varExpr = FixE (Var (pos, var))
                                val assign =
                                    FixE (PrimApp (pos, Primop.BSet,
                                                   Vector.fromList [varExpr,
                                                                    expr]))
                                val stmt' = FixBS (BindStmt1.Expr assign)
                            in
                               ( VectorExt.conj stmts' stmt'
                               , NameSet.add (dvars, name))
                           end
                         | BindStmt1.Def (var as Var.Lex _, expr) =>
                           let val stmt' =
                                   FixBS (BindStmt1.Def (var, elabExpr expr))
                           in (VectorExt.conj stmts' stmt', dvars)
                           end
                         | BindStmt1.Expr expr =>
                           let val stmt' =
                                   FixBS (BindStmt1.Expr (elabExpr expr))
                           in (VectorExt.conj stmts' stmt', dvars)
                           end
                     fun elabCase (AuglessAst.Bind (pos, dnf, bstmts), body) =
                         let val init = (VectorExt.empty (), NameSet.empty)
                             val (bstmts', dvars) =
                                 Vector.foldl elabBStmt init bstmts
                             val frameDef = dynFrameBindDef pos dvars
                             val bstmts'' =
                                 if NameSet.numItems dvars > 0
                                 then VectorExt.prepend bstmts'
                                                        (dynFrameBindDef pos
                                                                         dvars)
                                 else bstmts'
                         in ( AuglessAst.Bind ( pos
                                              , DNF.map elabExpr dnf
                                              , bstmts'' )
                            , elabExpr body )
                        end
                 in Fn (pos, name, Vector.map elabCase cases)
                 end
               | Expr.Block (pos, stmts) => Block (pos, elabStmts stmts)
               | Expr.App (pos, f, args) =>
                 App (pos, elabExpr f, Vector.map elabExpr args)
               | Expr.PrimApp (pos, po, args) =>
                 PrimApp (pos, po, Vector.map elabExpr args)
               | v as Expr.Var _ => v
               | c as Expr.Const _ => c)

    and elabStmt (AuglessAst.FixS stmt, (stmts', lvars, dvars)) =
        case stmt
        of AuglessStmt.Def (temp, bind, expr) =>
           let val (bind', lvars', dvars') = elabBind (bind, lvars, dvars)
               val stmt' = FixS (Def (temp, bind', elabExpr expr))
           in (VectorExt.conj stmts' stmt', lvars', dvars')
           end
         | AuglessStmt.Expr expr =>
           let val stmt' = FixS (Expr (elabExpr expr))
           in (VectorExt.conj stmts' stmt', lvars, dvars)
           end

    and elabStmts stmts =
        let val pos = AuglessAst.stmtPos (Vector.sub (stmts, 0))
            val init = (VectorExt.empty (), NameSet.empty, NameSet.empty)
            val (stmts', lvars, dvars) = Vector.foldl elabStmt init stmts
            val boxAlloc = boxAllocAt pos
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
            val stmts'' = VectorExt.concat boxDefs stmts'
        in
            if NameSet.numItems dvars > 0
            then VectorExt.prepend stmts'' (dynFrameDef pos dvars)
            else stmts''
        end

    and elabBind (AuglessAst.Bind (pos, dnf, bstmts), lvars, dvars) =
        let fun elabBStmt (AuglessAst.FixBS bstmt, (bstmts', lvars, dvars)) =
                case bstmt
                of BindStmt1.Def (var, expr) =>
                   let val varExpr = FixE (Var (pos, var))
                       val assign =
                           FixE (PrimApp (pos, Primop.BSet,
                                          Vector.fromList [varExpr, expr]))
                       val stmt' = FixBS (BindStmt1.Expr assign)
                       val stmts' = VectorExt.conj bstmts' stmt'
                   in
                       case var
                       of Var.Lex name =>
                          (stmts', NameSet.add (lvars, name), dvars)
                        | Var.Dyn name =>
                          (stmts', lvars, NameSet.add (dvars, name))
                   end
                 | BindStmt1.Expr expr =>
                   let val stmt' = FixBS (BindStmt1.Expr (elabExpr expr))
                   in (VectorExt.conj bstmts' stmt', lvars, dvars)
                   end
            val init = (VectorExt.empty (), lvars, dvars)
            val (bstmts', lvars', dvars') = Vector.foldl elabBStmt init bstmts
        in
            ( AuglessAst.Bind (pos, DNF.map elabExpr dnf, bstmts')
            , lvars', dvars')
        end

    val straighten = elabStmts
end
