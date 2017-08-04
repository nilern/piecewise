structure AnfConvert :> sig
    val convert : FlatAst1.program -> Anf.program
end = struct
    structure Cfg = Anf.Cfg
    structure Expr = Anf.Expr
    structure ValExpr = Anf.ValExpr
    val PrimApp = Anf.Expr.PrimApp
    val Triv = Anf.Expr.Triv
    val Def = Anf.Stmt.Def
    val Expr = Anf.Stmt.Expr
    val Guard = ValExpr.Guard
    val Var = Anf.Expr.Triv.Var
    val Local = FlatTag1.Local

    fun trivialize stmtBuilder =
        fn Anf.Expr.Triv (_, triv) => triv
         | expr => let val name = Name.freshFromString "v"
                   in VectorExt.Builder.append stmtBuilder (Def (Expr.pos expr, name, expr))
                    ; Var (Local, name)
                   end

    fun convertExpr stmtBuilder blocks label (FlatAst1.FixE expr) =
        case expr
        of FlatAst1.Expr.Block (_, (stmts, expr)) =>
           ( convertStmts stmtBuilder blocks label stmts
           ; convertExpr stmtBuilder blocks label expr )
         | FlatAst1.Expr.PrimApp (pos, po, args) =>
           let val args' = Vector.map (exprToTriv stmtBuilder blocks label) args
           in PrimApp (pos, po, args')
           end
         | FlatAst1.Expr.Triv posTriv => Triv posTriv

    and exprToTriv stmtBuilder blocks label expr =
        trivialize stmtBuilder (convertExpr stmtBuilder blocks label expr)

    and convertStmts stmtBuilder blocks label stmts =
        let fun convert stmts =
                if VectorSlice.length stmts > 0
                then case VectorSlice.sub (stmts, 0)
                     of FlatAst1.FixS (FlatAst1.Stmt.Def (pos, name, expr)) =>
                        let val expr' = convertExpr stmtBuilder blocks label expr
                        in VectorExt.Builder.append stmtBuilder (Def (pos, name, expr'))
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                      | FlatAst1.FixS (FlatAst1.Stmt.Guard (pos, dnf)) =>
                        let val dnf' = convertDnf blocks dnf
                            val stmts' = VectorExt.Builder.build stmtBuilder
                            val _ = VectorExt.Builder.clear stmtBuilder
                            val bLabel = !label
                            val label' = Label.fresh ()
                        in Cfg.Builder.insert (blocks, bLabel, (stmts', Guard (pos, dnf', !label)))
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                      | FlatAst1.FixS (FlatAst1.Stmt.Expr expr) =>
                        let val expr' = convertExpr stmtBuilder blocks label expr
                        in VectorExt.Builder.append stmtBuilder (Expr expr')
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                else ()
        in convert (VectorSlice.full stmts)
        end

    and convertDnf blocks dnf =
        let fun convertAExpr expr =
                let val entry = Label.fresh ()
                    val label = ref entry
                    val stmtBuilder = VectorExt.Builder.empty ()
                    val triv = exprToTriv stmtBuilder blocks label expr
                    val block = ( VectorExt.Builder.build stmtBuilder
                                , ValExpr.Triv (FlatAst1.exprPos expr, triv) )
                in Cfg.Builder.insert (blocks, entry, block)
                 ; entry
                end
        in DNF.map convertAExpr dnf
        end

    fun convertBlock blocks label (stmts, expr) =
        let val stmtBuilder = VectorExt.Builder.empty ()
            val _ = convertStmts stmtBuilder blocks label stmts
            val triv = exprToTriv stmtBuilder blocks label expr
        in (VectorExt.Builder.build stmtBuilder, ValExpr.Triv (FlatAst1.exprPos expr, triv))
        end

    fun convertCase ((cond, bindStmts), body) =
        let val entry = Label.fresh ()
            val label = ref entry
            val blocks = Cfg.Builder.empty ()
            val cond' = convertDnf blocks cond
            val entryBlock = convertBlock blocks label (bindStmts, body)
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
            val label = ref entry
            val blocks = Cfg.Builder.empty ()
            val entryBlock = convertBlock blocks label main
        in { procs = NameMap.map convertProc procs
           , main = Cfg.Builder.build (blocks, entry, entryBlock) }
        end
end
