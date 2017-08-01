structure CpsConvert :> sig
    val convert : FlatAst1.program -> Cps0.program
end = struct
    val PrimApp = CpsExpr0.PrimApp
    val Guard = CpsExpr0.Guard
    val Triv = CpsExpr0.Triv
    val Var = CpsExpr0.Triv.Var
    val Const = CpsExpr0.Triv.Const
    val Local = FlatTag1.Local
    val Symbol = Const.Symbol

    fun analyzeSubExpr temp (FlatAst1.FixE expr) =
        case expr
        of FlatAst1.Expr.Triv (_, triv) => (temp, triv)
         | _ => let val tempName = Name.freshFromString "v"
                in (SOME tempName, Var (Local, tempName))
                end

    fun analyzeStmt temp (FlatAst1.FixS stmt) =
        case stmt
        of FlatAst1.Stmt.Def (_, var, _) => SOME var
         | _ => temp

    fun convertExpr asComplex temp (FlatAst1.FixE expr) res =
        case expr
        of FlatAst1.Expr.Block (_, block) => convertBlock temp res block
         | FlatAst1.Expr.PrimApp (pos, po, args) =>
           let fun convert temp trivArgs args res =
                   let val (conts, succ) = res
                   in if VectorSlice.length args > 0
                      then let val arg = VectorSlice.sub (args, 0)
                               val args' = VectorSlice.subslice (args, 1, NONE)
                               val (temp', trivArg) = analyzeSubExpr temp arg
                               val res' = convert temp' (VectorExt.conj trivArgs trivArg) args' res
                           in convertExpr false temp arg res'
                           end
                      else let val k = ContRef0.fresh ()
                               val cont = { args = OptionExt.toVector temp
                                          , expr = PrimApp (pos, po, trivArgs)
                                          , succs = Vector.fromList [succ] }
                           in (ContMap0.insert (conts, k, cont), ContRef0.Label k)
                           end
                   end
           in convert temp (VectorExt.empty ()) (VectorSlice.full args) res
           end
         | FlatAst1.Expr.Triv (pos, triv) =>
           if asComplex
           then let val (conts, succ) = res
                    val k = ContRef0.fresh ()
                    val cont = { args = OptionExt.toVector temp
                               , expr = Triv (pos, triv)
                               , succs = Vector.fromList [succ] }
                in (ContMap0.insert (conts, k, cont), ContRef0.Label k)
                end
           else res

    and convertStmt temp (FlatAst1.FixS stmt) res =
        case stmt
        of FlatAst1.Stmt.Def (_, _, expr) => convertExpr true temp expr res
         | FlatAst1.Stmt.Guard (pos, dnf) =>
           let val (conts', succ) = res
               val k = ContRef0.fresh ()
               val pk = ContRef0.fresh ()
               val panic = { args = VectorExt.empty ()
                           , expr = PrimApp (pos, Primop.Panic,
                                             Vector.fromList [Const (Symbol "match")])
                           , succs = Vector.fromList [] }
               val conts = ref (ContMap0.insert (conts', pk, panic))
               fun convertAExpr expr =
                   let val (conts', ContRef0.Label k) =
                           convertExpr true NONE expr (!conts, ContRef0.NextAtom k)
                   in conts := conts'; k
                   end
               val dnf' = DNF.map convertAExpr dnf
               val cont = { args = OptionExt.toVector temp
                          , expr = Guard (pos, dnf')
                          , succs = Vector.fromList [succ, ContRef0.Label pk] }
           in (ContMap0.insert (!conts, k, cont), ContRef0.Label k)
           end
         | FlatAst1.Stmt.Expr expr => convertExpr false temp expr res

    and convertBlock temp res (stmts, expr) =
        let fun convert temp (stmts, expr) res =
                if VectorSlice.length stmts > 0
                then let val stmt = VectorSlice.sub (stmts, 0)
                         val block' = (VectorSlice.subslice (stmts, 1, NONE), expr)
                         val temp' = analyzeStmt temp stmt
                         val res' = convert temp' block' res
                     in convertStmt temp stmt res'
                     end
                else convertExpr true temp expr res
        in convert temp (VectorSlice.full stmts, expr) res
        end

    fun convertTopBlock init block =
        let val (block', ContRef0.Label entry) = convertBlock NONE init block
        in (entry, block')
        end

    fun convertProc { name = name, clovers = clovers
                    , args = { self = self, params = params, denv = denv }
                    , cases = cases } =
        let val retCont = ContRef0.fresh ()
            val init = (ContMap0.empty, ContRef0.Label retCont)
        in
            { name = name
            , clovers = clovers
            , args = { self = self, params = params, denv = denv, cont = retCont }
            , cases = Vector.map (convertTopBlock init) cases }
        end

    fun convert { procs = procs, main = main } =
        { procs = NameMap.map convertProc procs
        , main = convertTopBlock (ContMap0.empty, ContRef0.Halt) main }
end
