structure AnfConvert :> sig
    val convert : FlatAst1.program -> Anf.program
end = struct
    structure Cfg = Anf.Cfg
    structure Expr = Anf.Expr
    structure Stmt = Anf.Stmt
    structure ValExpr = Anf.ValExpr

    val PrimApp = Expr.PrimApp
    val Triv = Expr.Triv
    val Def = Stmt.Def
    val Expr = Stmt.Expr
    val Guard = ValExpr.Guard
    val Var = Expr.Triv.Var
    val Local = FlatTag1.Local

    structure BlockBuilder :> sig
        type t

        val empty : Label.t -> t
        val append : t -> Stmt.t -> unit
        val build : t -> ValExpr.t -> Label.t * Anf.block
        val take : t -> (Label.t -> ValExpr.t) -> Label.t * Anf.block
    end = struct
        type t = Label.t ref * Stmt.t VectorExt.Builder.t

        fun empty label = (ref label, VectorExt.Builder.empty ())

        fun append (_, stmts) stmt = VectorExt.Builder.append stmts stmt

        fun build (label, stmts) vexpr = (!label, (VectorExt.Builder.build stmts, vexpr))

        fun take (label, stmts) makeVExpr =
            let val label' = Label.fresh ();
                val res = (!label, (VectorExt.Builder.build stmts, makeVExpr label'))
            in (label := label'; VectorExt.Builder.clear stmts)
             ; res
            end
    end

    fun trivialize blockBuilder =
        fn Anf.Expr.Triv (_, triv) => triv
         | expr => let val name = Name.freshFromString "v"
                   in BlockBuilder.append blockBuilder (Def (Expr.pos expr, name, expr))
                    ; Var (Local, name)
                   end

    fun convertExpr cfgBuilder blockBuilder (FlatAst1.FixE expr) =
        case expr
        of FlatAst1.Expr.Block (_, (stmts, expr)) =>
           ( convertStmts cfgBuilder blockBuilder stmts
           ; convertExpr cfgBuilder blockBuilder expr )
         | FlatAst1.Expr.PrimApp (pos, po, args) =>
           let val args' = Vector.map (exprToTriv cfgBuilder blockBuilder) args
           in PrimApp (pos, po, args')
           end
         | FlatAst1.Expr.Triv posTriv => Triv posTriv

    and exprToTriv cfgBuilder blockBuilder expr =
        trivialize blockBuilder (convertExpr cfgBuilder blockBuilder expr)

    and convertStmts cfgBuilder blockBuilder stmts =
        let fun convert stmts =
                if VectorSlice.length stmts > 0
                then case VectorSlice.sub (stmts, 0)
                     of FlatAst1.FixS (FlatAst1.Stmt.Def (pos, name, expr)) =>
                        let val expr' = convertExpr cfgBuilder blockBuilder expr
                        in BlockBuilder.append blockBuilder (Def (pos, name, expr'))
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                      | FlatAst1.FixS (FlatAst1.Stmt.Guard (pos, dnf)) =>
                        let val dnf' = convertDnf cfgBuilder dnf
                            val (label, block) =
                                BlockBuilder.take blockBuilder
                                                  (fn label' => Guard (pos, dnf', label'))
                        in Cfg.Builder.insert (cfgBuilder, label, block)
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                      | FlatAst1.FixS (FlatAst1.Stmt.Expr expr) =>
                        let val expr' = convertExpr cfgBuilder blockBuilder expr
                        in BlockBuilder.append blockBuilder (Expr expr')
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                else ()
        in convert (VectorSlice.full stmts)
        end

    and convertDnf cfgBuilder dnf =
        let fun convertAExpr expr =
                let val entry = Label.fresh ()
                in convertBlock cfgBuilder entry (Vector.fromList [], expr)
                 ; entry
                end
        in DNF.map convertAExpr dnf
        end

    and convertBlock cfgBuilder entry (stmts, expr) =
        let val blockBuilder = BlockBuilder.empty entry
            val _ = convertStmts cfgBuilder blockBuilder stmts
            val triv = exprToTriv cfgBuilder blockBuilder expr
            val vexpr = ValExpr.Triv (FlatAst1.exprPos expr, triv)
            val (label, block) = BlockBuilder.build blockBuilder vexpr
        in Cfg.Builder.insert (cfgBuilder, label, block)
        end

    fun convertCase ((cond, bindStmts), body) =
        let val entry = Label.fresh ()
            val cfgBuilder = Cfg.Builder.empty entry
            val cond' = convertDnf cfgBuilder cond
        in convertBlock cfgBuilder entry (bindStmts, body)
         ; { cond = cond'
           , cfg = Cfg.Builder.build cfgBuilder }
        end

    fun convertProc { name = name, clovers = clovers
                    , args = { self = self, params = params, denv = denv }
                    , cases = cases } =
        { name = name, clovers = clovers
        , args = { self = self, params = params, denv = denv }
        , cases = Vector.map convertCase cases }

    fun convert { procs = procs, main = main } =
        let val entry = Label.fresh ()
            val cfgBuilder = Cfg.Builder.empty entry
        in convertBlock cfgBuilder entry main
         ; { procs = NameMap.map convertProc procs
           , main = Cfg.Builder.build cfgBuilder }
        end
end
