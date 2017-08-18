structure AnfConvert :> sig
    val convert : FlatAst1.program -> Anf.program
end = struct
    structure Cfg = Anf.Cfg
    structure Expr = Anf.Expr
    structure Stmt = Anf.Stmt
    structure ValExpr = Anf.ValExpr
    structure Triv = Expr.Triv

    structure FExpr = FlatAst1.Expr
    structure FStmt = FlatAst1.Stmt

    val Call = Expr.Call
    val PrimCall = Expr.PrimCall
    val Triv = Expr.Triv
    val Def = Stmt.Def
    val Expr = Stmt.Expr
    val Guard = ValExpr.Guard
    val Var = Triv.Var

    structure BlockBuilder :> sig
        type t

        val empty : Name.t -> t
        val append : t -> Stmt.t -> unit
        val build : t -> ValExpr.t -> Name.t * Anf.block
        val take : t -> Pos.t -> Name.t DNF.t -> Name.t * Anf.block
    end = struct
        type t = Name.t ref * Stmt.t VectorExt.Builder.t

        fun empty label = (ref label, VectorExt.Builder.empty ())

        fun append (_, stmts) stmt = VectorExt.Builder.append stmts stmt

        fun build (label, stmts) vexpr = (!label, (VectorExt.Builder.build stmts, vexpr))

        fun take (label, stmts) pos dnf =
            let val label' = Name.freshFromString "l";
                val res = (!label, (VectorExt.Builder.build stmts, Guard (pos, dnf, label')))
            in (label := label'; VectorExt.Builder.clear stmts)
             ; res
            end
    end

    fun trivialize blockBuilder =
        fn Expr.Triv (_, triv) => triv
         | expr => let val name = Name.freshFromString "v"
                   in BlockBuilder.append blockBuilder (Def (Expr.pos expr, name, expr))
                    ; Var (FlatVar1.Data name)
                   end

    fun convertExpr cfgBuilder blockBuilder (FlatAst1.FixE expr) =
        case expr
        of FExpr.Block (_, (stmts, expr)) =>
           ( convertStmts cfgBuilder blockBuilder stmts
           ; convertExpr cfgBuilder blockBuilder expr )
         | FExpr.Call (pos, f, args) =>
           let val f' = exprToTriv cfgBuilder blockBuilder f
               val args' = Vector.map (exprToTriv cfgBuilder blockBuilder) args
           in Call (pos, f', args')
           end
         | FExpr.PrimCall (pos, po, args) =>
           let val args' = Vector.map (exprToTriv cfgBuilder blockBuilder) args
           in PrimCall (pos, po, args')
           end
         | FExpr.Triv posTriv => Triv posTriv

    and exprToTriv cfgBuilder blockBuilder expr =
        trivialize blockBuilder (convertExpr cfgBuilder blockBuilder expr)

    and convertTailExpr cfgBuilder blockBuilder (FlatAst1.FixE expr) =
        case expr
        of FExpr.Block (_, (stmts, expr)) =>
           ( convertStmts cfgBuilder blockBuilder stmts
           ; convertTailExpr cfgBuilder blockBuilder expr )
         | FExpr.Call (pos, f, args) =>
           let val f' = exprToTriv cfgBuilder blockBuilder f
               val args' = Vector.map (exprToTriv cfgBuilder blockBuilder) args
           in ValExpr.Call (pos, f', args')
           end
         | FExpr.PrimCall (pos, _, _) =>
           ValExpr.Triv (pos, exprToTriv cfgBuilder blockBuilder (FlatAst1.FixE expr))
         | FExpr.Triv posTriv => ValExpr.Triv posTriv

    and convertStmts cfgBuilder blockBuilder stmts =
        let fun convert stmts =
                if VectorSlice.length stmts > 0
                then case VectorSlice.sub (stmts, 0)
                     of FlatAst1.FixS (FStmt.Def (pos, name, expr)) =>
                        let val expr' = convertExpr cfgBuilder blockBuilder expr
                        in BlockBuilder.append blockBuilder (Def (pos, name, expr'))
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                      | FlatAst1.FixS (FStmt.Guard (pos, dnf)) =>
                        let val dnf' = convertDnf cfgBuilder dnf
                            val (label, block) = BlockBuilder.take blockBuilder pos dnf'
                        in Cfg.Builder.insert (cfgBuilder, label, block)
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                      | FlatAst1.FixS (FStmt.Expr expr) =>
                        let val expr' = convertExpr cfgBuilder blockBuilder expr
                        in BlockBuilder.append blockBuilder (Expr expr')
                         ; convert (VectorSlice.subslice (stmts, 1, NONE))
                        end
                else ()
        in convert (VectorSlice.full stmts)
        end

    and convertDnf cfgBuilder dnf =
        let fun convertAExpr expr =
                let val entry = Name.freshFromString "l"
                in convertBlock cfgBuilder entry (Vector.fromList [], expr)
                 ; entry
                end
        in DNF.map convertAExpr dnf
        end

    and convertBlock cfgBuilder entry (stmts, expr) =
        let val blockBuilder = BlockBuilder.empty entry
            val _ = convertStmts cfgBuilder blockBuilder stmts
            val vexpr = convertTailExpr cfgBuilder blockBuilder expr
            val (label, block) = BlockBuilder.build blockBuilder vexpr
        in Cfg.Builder.insert (cfgBuilder, label, block)
        end

    fun convertCase ((cond, bindStmts), body) =
        let val entry = Name.freshFromString "l"
            val cfgBuilder = Cfg.Builder.empty entry
            val cond' = convertDnf cfgBuilder cond
        in convertBlock cfgBuilder entry (bindStmts, body)
         ; { cond = cond'
           , cfg = Cfg.Builder.build cfgBuilder }
        end

    fun convertProc { pos = pos, name = name, clovers = clovers, args = args, cases = cases } =
        { pos = pos, name = name, clovers = clovers, args = args
        , cases = Vector.map convertCase cases }

    fun convert { procs = procs, main = main } =
        let val entry = Name.freshFromString "entry"
            val cfgBuilder = Cfg.Builder.empty entry
        in convertBlock cfgBuilder entry main
         ; { procs = NameMap.map convertProc procs
           , main = Cfg.Builder.build cfgBuilder }
        end
end
