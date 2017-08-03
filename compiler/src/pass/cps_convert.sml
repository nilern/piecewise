structure CpsConvert :> sig
    val convert : Anf.program -> Cps.program
end = struct
    structure Cfg = Cps.Cfg
    val Def = Cps.Stmt.Def
    val Expr = Cps.Stmt.Expr
    val Continue = Cps.Transfer.Continue
    val Var = Cps.Expr.Triv.Var
    val Local = FlatTag1.Local

    fun convertDispatch blockBuilder canCases blocks = raise Fail "unimplemented"

    fun convertStmts blockBuilder blocks stmts =
        let fun loop stmts stmts' =
                if VectorSlice.length stmts > 0
                then case VectorSlice.sub (stmts, 0)
                     of Anf.Stmt.Def (pos, name, expr) =>
                        loop (VectorSlice.subslice (stmts, 1, NONE))
                             (Def (pos, name, expr) :: stmts')
                      | Anf.Stmt.Guard (pos, dnf) => raise Fail "unimplemented"
                      | Anf.Stmt.Expr expr => loop (VectorSlice.subslice (stmts, 1, NONE))
                                                   (Expr expr :: stmts')
                else Vector.fromList (List.rev stmts')
        in loop (VectorSlice.full stmts) []
        end

    fun convertBlock convertExpr blockBuilder blocks label =
        let val (stmts, expr) = valOf (LabelMap.find (blocks, label))
            val stmts' = convertStmts blockBuilder blocks stmts
            val (exprStmts, expr') = convertExpr expr
            val cont = { args = Vector.fromList []
                       , block = (VectorExt.concat stmts' exprStmts, expr') }
        in Cfg.Builder.insert (blockBuilder, label, cont)
        end

    fun convertCfg blockBuilder ret { entry = entry, blocks = blocks } =
        let val convertExpr =
                fn Anf.Expr.Triv (_, triv) =>
                  (Vector.fromList [], Continue (ret, Vector.fromList [triv]))
                 | expr =>
                   let val name = Name.freshFromString "v"
                       val triv = Var (Local, name)
                   in ( Vector.fromList [Def (Anf.Expr.pos expr, name, expr)]
                      , Continue (ret, Vector.fromList [triv]))
                   end
        in convertBlock convertExpr blockBuilder blocks entry
        end

    fun convertProc { name = name, clovers = clovers
                    , args = { self = self, params = params, denv = denv }
                    , cases = cases } =
        let fun canonicalCases cases =
                let fun canonicalizeCase { cond = cond, cfg = { entry = entry, blocks = _ } } =
                        Vector.map (fn clause => (clause, entry)) (DNF.toClauses cond)
                in VectorExt.flatMap canonicalizeCase cases
                end
            fun casesBlocks (cases : Anf.procCase vector) =
                let fun step (cs, blocks) = LabelMap.unionWith #1 (#blocks (#cfg cs), blocks)
                in Vector.foldl step LabelMap.empty cases
                end
            val ret = Label.fresh ()
            val blockBuilder = Cfg.Builder.empty ()
            val canCases = canonicalCases cases
            val blocks = casesBlocks cases
            val cfgs = Vector.map #cfg cases
        in convertDispatch blockBuilder canCases blocks
         ; Vector.app (convertCfg blockBuilder ret) cfgs
         ; { name = name
           , clovers = clovers
           , args = { self = self, params = params, denv = denv, ret = ret }
           , cfg = Cfg.Builder.build blockBuilder NONE }
        end

    fun convert ({ procs = procs, main = main as { entry = entry, ... } } : Anf.program) =
        let val blockBuilder = Cfg.Builder.empty ()
        in convertCfg blockBuilder (Label.fresh () (* HACK *)) main
         ; { procs = NameMap.map convertProc procs
           , main = Cfg.Builder.build blockBuilder (SOME entry) }
        end
end
