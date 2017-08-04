structure CpsConvert :> sig
    val convert : Anf.program -> Cps.program
end = struct
    structure Cfg = Cps.Cfg
    val Continue = Cps.Transfer.Continue
    val Branch = Cps.Transfer.Branch

    structure Dispatcher = struct
        type t = (Label.t DNF.Clause.t * Label.t) vector
        datatype target_res = Success of Label.t | Fail
        datatype fail_dest = Cases of Pos.t | Guard

        fun fromCases cases =
            let fun canonicalizeCase { cond = cond, cfg = { entry = entry, blocks = _ } } =
                    Vector.map (fn clause => (clause, entry)) (DNF.toClauses cond)
            in VectorExt.flatMap canonicalizeCase cases
            end

        fun fromGuard cond dest = Vector.map (fn clause => (clause, dest)) (DNF.toClauses cond)

        (* FIXME: honor atom deps! *)
        fun pickCond (clauses : t) = DNF.Clause.first (#1 (Vector.sub (clauses, 0)))

        fun prune clauses cond v =
            let fun info aexpr = if aexpr = cond then SOME v else NONE
                fun pruneClause (clause, dest) =
                    (DNF.Clause.remove (fn cond' => cond' = cond) clause, dest)
            in Vector.map pruneClause (VectorExt.remove (DNF.Clause.isNeverWhen info o #1) clauses)
            end

        fun target clauses =
            if Vector.length clauses = 0
            then SOME Fail
            else let val (clause, dest) = Vector.sub (clauses, 0)
                 in if DNF.Clause.length clause = 0
                    then SOME (Success dest)
                    else NONE
                 end
    end

    (* TODO: generate DAG instead of tree by memoizing convert *)
    fun convertDispatch cfgBuilder dispatcher k blocks failDest =
        let fun convertAssuming dispatcher condCont v pos =
                convert (Dispatcher.prune dispatcher condCont v) pos
            and convert dispatcher pos =
                case Dispatcher.target dispatcher
                of SOME (Dispatcher.Success dest) =>
                   ( convertBlock cfgBuilder dest k blocks
                   ; dest )
                 | SOME Dispatcher.Fail =>
                   (case failDest
                    of Dispatcher.Cases pos => Cfg.Builder.genCaseFail cfgBuilder pos
                     | Dispatcher.Guard => Cfg.Builder.genGuardFail cfgBuilder (valOf pos))
                 | NONE =>
                   let val condCont = Dispatcher.pickCond dispatcher
                       val pos = SOME (Anf.blockPos (LabelMap.lookup (blocks, condCont)))
                       val k1 = convertAssuming dispatcher condCont true pos
                       val k2 = convertAssuming dispatcher condCont false pos
                   in convertBlock cfgBuilder condCont (fn triv => Branch (triv, k1, k2)) blocks
                    ; condCont
                   end
        in convert dispatcher NONE
        end

    and convertBlock cfgBuilder label k blocks =
        if not (Cfg.Builder.contains cfgBuilder label)
        then let val (stmts, vexpr) = LabelMap.lookup (blocks, label)
             in case vexpr
                of Anf.ValExpr.Triv (_, triv) =>
                   let val cont = { args = Vector.fromList [], block = (stmts, k triv) }
                   in Cfg.Builder.insert (cfgBuilder, label, cont)
                   end
                 | Anf.ValExpr.Guard (pos, dnf, dest) =>
                   let val dispatcher = Dispatcher.fromGuard dnf dest
                       val dispatchLabel =
                           convertDispatch cfgBuilder dispatcher k blocks Dispatcher.Guard
                   in Cfg.Builder.prependStmts (cfgBuilder, dispatchLabel, stmts)
                   end
             end
        else ()

    fun convertProc { name = name, clovers = clovers
                    , args = { self = self, params = params, denv = denv }
                    , cases = cases } =
        let fun casesBlocks (cases : Anf.procCase vector) =
                let fun step (cs, blocks) = LabelMap.unionWith #1 (#blocks (#cfg cs), blocks)
                in Vector.foldl step LabelMap.empty cases
                end
            val cfgBuilder = Cfg.Builder.empty ()
            val dispatcher = Dispatcher.fromCases cases
            val blocks = casesBlocks cases
            val ret = Label.fresh ()
            fun k triv = Continue (ret, Vector.fromList [triv])
            val entry = (* FIXME: proc should carry a Pos.t for this *)
                convertDispatch cfgBuilder dispatcher k blocks (Dispatcher.Cases Pos.def)
        in { name = name
           , clovers = clovers
           , args = { self = self, params = params, denv = denv, ret = ret }
           , cfg = Cfg.Builder.build cfgBuilder entry }
        end

    fun convert { procs = procs, main = main as { entry = entry, blocks = blocks } } =
        let val halt = Label.fresh () (* HACK *)
            val cfgBuilder = Cfg.Builder.empty ()
            fun k triv = Continue (halt, Vector.fromList [triv])
        in convertBlock cfgBuilder entry k blocks
         ; { procs = NameMap.map convertProc procs
           , main = Cfg.Builder.build cfgBuilder entry }
        end
end
