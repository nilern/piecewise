structure AnfConvert :> sig
    val convert : FlatAst1.program -> Anf.program
end = struct
    structure Cfg = Anf.Cfg
    val PrimApp = Anf.Expr.PrimApp
    val Triv = Anf.Expr.Triv
    val Def = Anf.Stmt.Def
    val Guard = Anf.Stmt.Guard
    val Expr = Anf.Stmt.Expr
    val Var = Anf.Expr.Triv.Var
    val Local = FlatTag1.Local

    fun exprToStmts expr =
        let val ((stmts, expr), blocks) = exprToBlock expr
        in (VectorExt.conj stmts (Expr expr), blocks)
        end

    and exprToTriv expr =
        case expr
        of FlatAst1.FixE (FlatAst1.Expr.Triv (pos, triv)) =>
           (triv, Vector.fromList [], LabelMap.empty)
          | _ =>
            let val name = Name.freshFromString "v"
                val triv = Var (Local, name)
                val ((stmts, arg'), blocks) = exprToBlock expr
            in ( triv, VectorExt.conj stmts (Def (Anf.Expr.pos arg', name, arg')), blocks )
            end

    and stmtToStmts (FlatAst1.FixS stmt) =
        case stmt
        of FlatAst1.Stmt.Def (pos, name, expr) =>
           let val ((stmts, expr'), blocks) = exprToBlock expr
           in (VectorExt.conj stmts (Def (pos, name, expr')), blocks)
           end
         | FlatAst1.Stmt.Guard (pos, dnf) =>
           let val (dnf', blocks) = convertDnf dnf
           in (Vector.fromList [Guard (pos, dnf')], blocks)
           end
         | FlatAst1.Stmt.Expr expr => exprToStmts expr

    and convertStmts stmts =
        let fun step (stmt, (stmts, blocks)) =
                let val (stmts', blocks') = stmtToStmts stmt
                in (VectorExt.concat stmts stmts', LabelMap.unionWith #1 (blocks, blocks'))
                end
        in Vector.foldl step (Vector.fromList [], LabelMap.empty) stmts
        end

    and convertBlock (stmts, expr) =
        let val (stmts', blocks) = convertStmts stmts
            val ((exprStmts, expr'), exprBlocks) = exprToBlock expr
        in ((VectorExt.concat stmts' exprStmts, expr'), LabelMap.unionWith #1 (blocks, exprBlocks))
        end

    and exprToBlock (FlatAst1.FixE expr) =
        case expr
        of FlatAst1.Expr.Block (_, block) => convertBlock block
         | FlatAst1.Expr.PrimApp (pos, po, args) =>
           let fun step (arg, (stmts, trivs, blocks)) =
                   let val (triv, stmts', blocks') = exprToTriv arg
                   in ( VectorExt.concat stmts stmts'
                      , VectorExt.conj trivs triv
                      , LabelMap.unionWith #1 (blocks, blocks') )
                   end
               val init = (Vector.fromList [], Vector.fromList [], LabelMap.empty)
               val (stmts, trivs, blocks) = Vector.foldl step init args
               val expr' = PrimApp (pos, po, trivs)
           in ((stmts, expr'), blocks)
           end
         | FlatAst1.Expr.Triv (pos, triv) =>
           ((Vector.fromList [], Triv (pos, triv)), LabelMap.empty)

    and convertDnf dnf =
        let val blocks = ref LabelMap.empty
            fun convertAExpr expr =
                let val entry = Label.fresh ()
                    val (block, blocks') = exprToBlock expr
                    val blocks'' = LabelMap.unionWith #1 (!blocks, blocks')
                in blocks := LabelMap.insert (blocks'', entry, block)
                 ; entry
                end
            val dnf' = DNF.map convertAExpr dnf
        in (dnf', !blocks)
        end

    fun convertCase ((cond, bindStmts), body) =
        let val entry = Label.fresh ()
            val (cond', condBlocks) = convertDnf cond
            val (bindStmts', bindBlocks) = convertStmts bindStmts
            val ((bodyStmts, bodyExpr), bodyBlocks) = exprToBlock body
            val entryBlock = (VectorExt.concat bindStmts' bodyStmts, bodyExpr)
            val blocks = LabelMap.unionWith #1 ( LabelMap.unionWith #1 (condBlocks, bindBlocks)
                                               , bodyBlocks)
        in { cond = cond'
           , cfg = { entry = entry, blocks = LabelMap.insert (blocks, entry, entryBlock) } }
        end

    fun convertProc { name = name, clovers = clovers
                    , args = { self = self, params = params, denv = denv }
                    , cases = cases } =
        { name = name, clovers = clovers
        , args = { self = self, params = params, denv = denv }
        , cases = Vector.map convertCase cases }

    fun convert { procs = procs, main = main } =
        let val entry = Label.fresh ()
            val (entryBlock, blocks) = convertBlock main
        in { procs = NameMap.map convertProc procs
           , main = { entry = entry
                    , blocks = LabelMap.insert (blocks, entry, entryBlock) } }
        end
end
