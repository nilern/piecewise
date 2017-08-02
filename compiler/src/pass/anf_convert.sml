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

    fun exprToBlock blocks (FlatAst1.FixE expr) =
        case expr
        of FlatAst1.Expr.Block (_, block) => convertBlock blocks block
         | FlatAst1.Expr.PrimApp (pos, po, args) =>
           let fun step (arg, (stmts, trivs)) =
                   let val (triv, stmts') = exprToTriv blocks arg
                   in ( VectorExt.concat stmts stmts', VectorExt.conj trivs triv )
                   end
               val init = (Vector.fromList [], Vector.fromList [])
               val (stmts, trivs) = Vector.foldl step init args
           in (stmts, PrimApp (pos, po, trivs))
           end
         | FlatAst1.Expr.Triv (pos, triv) => (Vector.fromList [], Triv (pos, triv))

    and exprToStmts blocks expr =
        let val (stmts, expr) = exprToBlock blocks expr
        in VectorExt.conj stmts (Expr expr)
        end

    and exprToTriv blocks expr =
        case expr
        of FlatAst1.FixE (FlatAst1.Expr.Triv (pos, triv)) => (triv, Vector.fromList [])
          | _ =>
            let val name = Name.freshFromString "v"
                val triv = Var (Local, name)
                val (stmts, arg') = exprToBlock blocks expr
            in (triv, VectorExt.conj stmts (Def (Anf.Expr.pos arg', name, arg')))
            end

    and stmtToStmts blocks (FlatAst1.FixS stmt) =
        case stmt
        of FlatAst1.Stmt.Def (pos, name, expr) =>
           let val (stmts, expr') = exprToBlock blocks expr
           in VectorExt.conj stmts (Def (pos, name, expr'))
           end
         | FlatAst1.Stmt.Guard (pos, dnf) =>
           let val dnf' = convertDnf blocks dnf
           in Vector.fromList [Guard (pos, dnf')]
           end
         | FlatAst1.Stmt.Expr expr => exprToStmts blocks expr

    and convertStmts blocks stmts = VectorExt.flatMap (stmtToStmts blocks) stmts

    and convertBlock blocks (stmts, expr) =
        let val stmts' = convertStmts blocks stmts
            val (exprStmts, expr') = exprToBlock blocks expr
        in (VectorExt.concat stmts' exprStmts, expr')
        end

    and convertDnf blocks dnf =
        let fun convertAExpr expr =
                let val entry = Label.fresh ()
                    val block = exprToBlock blocks expr
                in Cfg.Builder.insert (blocks, entry, block)
                 ; entry
                end
        in DNF.map convertAExpr dnf
        end

    fun convertCase ((cond, bindStmts), body) =
        let val entry = Label.fresh ()
            val blocks = Cfg.Builder.empty ()
            val cond' = convertDnf blocks cond
            val entryBlock = convertBlock blocks (bindStmts, body)
        in { cond = cond'
           , cfg = Cfg.Builder.build (blocks, entry, entryBlock) }
        end

    fun convertProc { name = name, clovers = clovers
                    , args = { self = self, params = params, denv = denv }
                    , cases = cases } =
        { name = name, clovers = clovers
        , args = { self = self, params = params, denv = denv }
        , cases = Vector.map convertCase cases }

    fun convert { procs = procs, main = main } =
        let val entry = Label.fresh ()
            val blocks = Cfg.Builder.empty ()
            val entryBlock = convertBlock blocks main
        in { procs = NameMap.map convertProc procs
           , main = Cfg.Builder.build (blocks, entry, entryBlock) }
        end
end
