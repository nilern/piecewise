structure CpsConvert :> sig
    val convert : Anf.program -> Cps.program
end = struct
    structure Expr = Cps.Expr
    structure Stmt = Cps.Stmt
    structure Transfer = Cps.Transfer
    structure Cfg = Cps.Cfg
    structure Triv = Expr.Triv
    val Data = FlatVar1.Data
    val Continue = Cps.Transfer.Continue
    val Branch = Cps.Transfer.Branch
    val Halt = Cps.Transfer.Halt

    structure Dispatcher = struct
        type t = (Name.t DNF.Clause.t * Name.t) vector
        datatype target_res = Success of Name.t | Fail
        datatype fail_dest = Cases of Pos.t | Guard

        fun fromCases cases =
            let fun canonicalizeCase { cond = cond, cfg = { entry = entry, blocks = _ } } =
                    Vector.map (fn clause => (clause, entry)) (DNF.toClauses cond)
            in VectorExt.flatMap canonicalizeCase cases
            end

        fun fromGuard cond dest = Vector.map (fn clause => (clause, dest)) (DNF.toClauses cond)

        fun pickCond (clauses : t) = DNF.Clause.someDepLeaf (#1 (Vector.sub (clauses, 0)))

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
    fun convertDispatch cfgBuilder dispatcher ks blocks failDest =
        let fun convertAssuming dispatcher condCont v pos =
                convert (Dispatcher.prune dispatcher condCont v) pos
            and convert dispatcher pos =
                case Dispatcher.target dispatcher
                of SOME (Dispatcher.Success dest) =>
                   ( convertBlock cfgBuilder dest ks blocks
                   ; dest )
                 | SOME Dispatcher.Fail =>
                   (case failDest
                    of Dispatcher.Cases pos => Cfg.Builder.genCaseFail cfgBuilder pos
                     | Dispatcher.Guard => Cfg.Builder.genGuardFail cfgBuilder (valOf pos))
                 | NONE =>
                   let val condCont = Dispatcher.pickCond dispatcher
                       val pos = SOME (Anf.blockPos (NameMap.lookup (blocks, condCont)))
                       val ks = Vector.fromList [ convertAssuming dispatcher condCont true pos
                                                , convertAssuming dispatcher condCont false pos ]
                   in convertBlock cfgBuilder condCont ks blocks
                    ; condCont
                   end
        in convert dispatcher NONE
        end

    and convertBlock cfgBuilder label ks blocks =
        if not (Cfg.Builder.contains cfgBuilder label)
        then let val (stmts, vexpr) = NameMap.lookup (blocks, label)
             in case vexpr
                of Anf.ValExpr.Triv (pos, triv) =>
                   let val transfer = case Vector.length ks
                                      of 0 => Halt (pos, triv)
                                       | 1 =>
                                         Continue (pos, Vector.sub (ks, 0), Vector.fromList [triv])
                                       | 2 =>
                                         Branch (pos, triv, Vector.sub (ks, 0), Vector.sub (ks, 1))
                                       | _ => raise Fail "unreachable"
                       val cont = { args = Argv.empty, block = (stmts, transfer) }
                   in Cfg.Builder.insert (cfgBuilder, label, cont)
                   end
                 | Anf.ValExpr.Call (pos, f, args) =>
                   let val block = case Vector.length ks
                                   of 0 => let val name = Name.freshFromString "v"
                                               val callExpr = Expr.Call (pos, f, args)
                                               val callStmt = Stmt.Def (pos, name, callExpr)
                                           in ( VectorExt.conj stmts callStmt
                                              , Halt (pos, Triv.Var (Data name)) )
                                           end
                                    | 1 => (stmts, Transfer.Call (pos, Vector.sub (ks, 0), f, args))
                                    | 2 => let val name = Name.freshFromString "v"
                                               val callExpr = Expr.Call (pos, f, args)
                                               val callStmt = Stmt.Def (pos, name, callExpr)
                                           in ( VectorExt.conj stmts callStmt
                                              , Branch ( pos, Triv.Var (Data name)
                                                       , Vector.sub (ks, 0), Vector.sub (ks, 1) ) )
                                           end
                                    | _ => raise Fail "unreachable"
                       val cont = { args = Argv.empty, block = block }
                   in Cfg.Builder.insert (cfgBuilder, label, cont)
                   end
                 | Anf.ValExpr.Guard (pos, dnf, dest) =>
                   let val dispatcher = Dispatcher.fromGuard dnf dest
                       val dispatchName =
                           convertDispatch cfgBuilder dispatcher ks blocks Dispatcher.Guard
                   in Cfg.Builder.prependStmts (cfgBuilder, dispatchName, stmts)
                   end
             end
        else ()

    fun convertProc { pos = pos, name = name, clovers = clovers, args = args, cases = cases } =
        let fun casesBlocks (cases : Anf.procCase vector) =
                let fun step (cs, blocks) = NameMap.unionWith #1 (#blocks (#cfg cs), blocks)
                in Vector.foldl step NameMap.empty cases
                end
            val cfgBuilder = Cfg.Builder.empty ()
            val dispatcher = Dispatcher.fromCases cases
            val blocks = casesBlocks cases
            val ret = Name.freshFromString "ret"
            val ks = Vector.fromList [ret]
            val entry = convertDispatch cfgBuilder dispatcher ks blocks (Dispatcher.Cases pos)
            val _ = Cfg.Builder.assocArgs (cfgBuilder, entry, Argv.prepend args ret Type.Closure)
        in { pos = pos, name = name
           , clovers = clovers
           , cfg = Cfg.Builder.build cfgBuilder entry }
        end

    fun convert { procs = procs, main = main as { entry = entry, blocks = blocks } } =
        let val cfgBuilder = Cfg.Builder.empty ()
            val ks = Vector.fromList []
        in convertBlock cfgBuilder entry ks blocks
         ; { procs = NameMap.map convertProc procs
           , main = Cfg.Builder.build cfgBuilder entry }
        end
end
