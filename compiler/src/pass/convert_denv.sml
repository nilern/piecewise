structure ConvertDEnv :> sig
    val convert : FlatAst0.program -> FlatAst1.program
end = struct
    structure Env :> sig
        type t

        val root : Name.t -> t
        val push : t -> NameSet.set -> t
        val name : t -> Name.t
        val parent : t -> t option
    end = struct
        datatype t = Root of Name.t
                   | Cons of Name.t * t

        val root = Root
        fun push env names =
            if NameSet.numItems names > 0
            then Cons (Name.freshFromString "denv", env)
            else env
        val name = fn Root name => name
                    | Cons (name, _) => name
        val parent = fn Root _ => NONE
                      | Cons (_, parent) => SOME parent
    end

    structure Expr = FlatAst1.Expr
    structure Stmt = FlatAst1.Stmt
    structure Triv = Expr.Triv

    structure PExpr = FlatAst0.Expr
    structure PStmt = FlatAst0.Stmt
    structure PTriv = PExpr.Triv

    val FixE = FlatAst1.FixE
    val FixS = FlatAst1.FixS
    val Block = Expr.Block
    val Call = Expr.Call
    val PrimCall = Expr.PrimCall
    val Triv = Expr.Triv
    val Def = Stmt.Def
    val Guard = Stmt.Guard
    val Expr = Stmt.Expr
    val Var = Triv.Var
    val Const = Triv.Const

    fun envPair pos name =
        Vector.fromList [ FixE (Triv (pos, Const (Const.Symbol (Name.toString name))))
                        , FixE (PrimCall (pos, Primop.Box, VectorExt.empty ())) ]

    fun envDef pos env names =
        let val oldEnvName =
                FixE (Triv (pos, Var (FlatVar1.Data (Env.name (Option.valOf (Env.parent env))))))
            val newEnvName = Env.name env
            val dePair = envPair pos
            val dePairs = VectorExt.flatMap dePair (Vector.fromList (NameSet.listItems names))
            val alloc =
                FixE (PrimCall (pos, Primop.DEnv, VectorExt.prepend dePairs oldEnvName))
        in FixS (Def (pos, newEnvName, alloc))
        end

    fun stmtVecBindings stmts =
        let fun stmtBindings (FlatAst0.FixS stmt, names) =
                case stmt
                of FlatAst0.Stmt.Def (_, BaseVar.Dyn name, _) => NameSet.add (names, name)
                 | FlatAst0.Stmt.Def (_, BaseVar.Lex _, _) => names
                 | FlatAst0.Stmt.Guard _ => names
                 | FlatAst0.Stmt.Expr _ => names
        in Vector.foldl stmtBindings NameSet.empty stmts
        end

    and elabExpr env (FlatAst0.FixE expr) =
        FixE (case expr
              of PExpr.Block (pos, block as (stmts, expr)) =>
                 let val names = stmtVecBindings stmts
                     val env' = Env.push env names
                 in Block (pos, (elabStmts pos env' names stmts, elabExpr env' expr))
                 end
               | PExpr.Call (pos, f, args) =>
                 let val args' = Vector.map (elabExpr env) args
                     val deExpr = FixE (Triv (pos, Var (FlatVar1.Data (Env.name env))))
                 in Call (pos, elabExpr env f, VectorExt.prepend args' deExpr)
                 end
               | PExpr.PrimCall (pos, po, args) =>
                 PrimCall (pos, po, Vector.map (elabExpr env) args)
               | PExpr.Triv (pos, PTriv.Var (FlatVar0.Label name)) =>
                 Triv (pos, Var (FlatVar1.Label name))
               | PExpr.Triv (pos, PTriv.Var (FlatVar0.Data (RVar.Current (BaseVar.Lex name)))) =>
                 Triv (pos, Var (FlatVar1.Data name))
               | PExpr.Triv (pos, PTriv.Var (FlatVar0.Data var)) =>
                 let val (name, skip) = case var
                                        of RVar.Current (BaseVar.Dyn name) =>
                                           (name, Const.Int (Int.toLarge 0))
                                         | RVar.Upper (BaseVar.Dyn name) =>
                                           (name, Const.Int (Int.toLarge 1))
                                         | _ => raise Fail "unreachable"
                     val nameSym = Const.Symbol (Name.toString name)
                 in PrimCall (pos, Primop.DGet,
                             Vector.fromList [ FixE (Triv (pos, Var (FlatVar1.Data (Env.name env))))
                                             , FixE (Triv (pos, Const skip))
                                             , FixE (Triv (pos, Const nameSym)) ])
                 end
               | PExpr.Triv (pos, PTriv.Const c) => Triv (pos, Const c))

    and elabStmts pos env names stmts =
        let fun elabStmt env (FlatAst0.FixS stmt) =
                FixS (case stmt
                      of PStmt.Def (pos, BaseVar.Lex name, expr) =>
                         Def (pos, name, elabExpr env expr)
                       | PStmt.Def (pos, BaseVar.Dyn name, expr) =>
                         let val nameSym = Const.Symbol (Name.toString name)
                             val load =
                                 (FixE (PrimCall ( pos
                                                , Primop.DGet
                                                , Vector.fromList
                                                  [ FixE (Triv ( pos
                                                               , Var (FlatVar1.Data
                                                                      (Env.name env))))
                                                  , FixE (Triv (pos,
                                                                Const (Const.Int (Int.toLarge 0))))
                                                  , FixE (Triv (pos, Const nameSym)) ])))
                         in
                             Expr (FixE (PrimCall (pos, Primop.BSet,
                                                  Vector.fromList [load, elabExpr env expr])))
                         end
                       | PStmt.Guard (pos, dnf) => Guard (pos, DNF.map (elabExpr env) dnf)
                       | PStmt.Expr expr => Expr (elabExpr env expr))
            val stmts' = Vector.map (elabStmt env) stmts
        in
            if NameSet.numItems names > 0
            then VectorExt.prepend stmts' (envDef pos env names)
            else stmts'
        end

    fun elabProc { pos = pos, name = name, clovers = clovers, args = args, cases = cases } =
        let val envName = Name.freshFromString "denv"
            fun elabCase ((cond, bindStmts), body) =
                (* MAYBE: We might not need the boxes here if we leave the order of argument binding
                          undefined. Any use of side effects (which the dynamic env is for) during
                          binding is bad style anyway. Do we punish for that with subtle bugs or
                          penalize every lambda-bound dynamic variable (which might be rare)? *)
                let val pos = FlatAst0.blockPos (bindStmts, body)
                    val names = stmtVecBindings bindStmts
                    val env = Env.push (Env.root envName) names
                in ( ( DNF.map (elabExpr env) cond, elabStmts pos env names bindStmts )
                   , elabExpr env body )
                end
        in
            { pos = pos, name = name
            , clovers = clovers
            , args = Argv.prepend args envName Type.DynEnv
            , cases = Vector.map elabCase cases }
        end

    fun convert { procs = procs, main = main as (stmts, expr) } =
        let val pos = FlatAst0.blockPos main
            val envName = Name.freshFromString "denv"
            val alloc = FixE (PrimCall (pos, Primop.EmptyDEnv, Vector.fromList []))
            val def = FixS (Def (pos, envName, alloc))
            val names = stmtVecBindings stmts
            val env = Env.push (Env.root envName) names
        in
            { procs = NameMap.map elabProc procs
            , main = ( VectorExt.prepend (elabStmts pos env names stmts) def
                     , elabExpr env expr ) }
        end
end
