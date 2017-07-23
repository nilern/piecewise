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

    val FixE = FlatAst1.FixE
    val FixS = FlatAst1.FixS
    val Block = FlatAst1.Expr.Block
    val PrimApp = FlatAst1.Expr.PrimApp
    val Triv = FlatAst1.Expr.Triv
    val Def = FlatAst1.Stmt.Def
    val Guard = FlatAst1.Stmt.Guard
    val Expr = FlatAst1.Stmt.Expr
    val Var = FlatTriv1.Var
    val Const = FlatTriv1.Const

    fun envPair pos name =
        Vector.fromList [ FixE (Triv (pos, Const (Const.Symbol (Name.toString name))))
                        , FixE (PrimApp (pos, Primop.Box, VectorExt.empty ())) ]

    fun envDef pos env names =
        let val oldEnvName =
                FixE (Triv (pos, Var (FlatTag1.Local, Env.name (Option.valOf (Env.parent env)))))
            val newEnvName = Env.name env
            val dePair = envPair pos
            val dePairs = VectorExt.flatMap dePair (Vector.fromList (NameSet.listItems names))
            val alloc =
                FixE (PrimApp (pos, Primop.DEnv, VectorExt.prepend dePairs oldEnvName))
        in FixS (Def (pos, newEnvName, alloc))
        end

    fun stmtVecBindings stmts =
        let fun stmtBindings (FlatAst0.FixS stmt, names) =
                case stmt
                of FlatAst0.Stmt.Def (_, (CTag.Dyn, name), _) => NameSet.add (names, name)
                 | FlatAst0.Stmt.Def (_, (CTag.Lex, _), _) => names
                 | FlatAst0.Stmt.Guard _ => names
                 | FlatAst0.Stmt.Expr _ => names
        in Vector.foldl stmtBindings NameSet.empty stmts
        end

    and elabExpr env (FlatAst0.FixE expr) =
        FixE (case expr
              of FlatAst0.Expr.Block (pos, block as (stmts, expr)) =>
                 let val names = stmtVecBindings stmts
                     val env' = Env.push env names
                 in Block (pos, (elabStmts pos env' names stmts, elabExpr env' expr))
                 end
               | FlatAst0.Expr.PrimApp (pos, Primop.Call, args) =>
                 let val args' = Vector.map (elabExpr env) args
                     val deExpr = FixE (Triv (pos, Var (FlatTag1.Local, (Env.name env))))
                 in PrimApp (pos, Primop.Call, VectorExt.prepend args' deExpr)
                 end
               | FlatAst0.Expr.PrimApp (pos, po, args) =>
                 PrimApp (pos, po, Vector.map (elabExpr env) args)
               | FlatAst0.Expr.Triv (pos, FlatTriv0.Var (FlatTag0.Lex, name)) =>
                 Triv (pos, Var (FlatTag1.Local, name))
               | FlatAst0.Expr.Triv (pos, FlatTriv0.Var (FlatTag0.Label, name)) =>
                 Triv (pos, FlatTriv1.Var (FlatTag1.Label, name))
               | FlatAst0.Expr.Triv (pos, FlatTriv0.Var (varTag, name)) =>
                 let val skip = Const.Int (case varTag
                                           of FlatTag0.Dyn => "0"
                                            | FlatTag0.UpperDyn => "1"
                                            | _ => raise Fail "unreachable")
                     val nameSym = Const.Symbol (Name.toString name)
                 in PrimApp (pos, Primop.DGet,
                             Vector.fromList [ FixE (Triv (pos, Var (FlatTag1.Local, Env.name env)))
                                             , FixE (Triv (pos, Const skip))
                                             , FixE (Triv (pos, Const nameSym)) ])
                 end
               | FlatAst0.Expr.Triv (pos, FlatTriv0.Const c) => Triv (pos, Const c))

    and elabStmts pos env names stmts =
        let fun elabStmt env (FlatAst0.FixS stmt) =
                FixS (case stmt
                      of FlatAst0.Stmt.Def (pos, (CTag.Lex, name), expr) =>
                         Def (pos, name, elabExpr env expr)
                       | FlatAst0.Stmt.Def (pos, (CTag.Dyn, name), expr) =>
                         let val nameSym = Const.Symbol (Name.toString name)
                             val load =
                                 (FixE (PrimApp ( pos
                                                , Primop.DGet
                                                , Vector.fromList
                                                  [ FixE (Triv ( pos
                                                               , Var ( FlatTag1.Local
                                                                     , Env.name env)))
                                                  , FixE (Triv (pos, Const (Const.Int "0")))
                                                  , FixE (Triv (pos, Const nameSym)) ])))
                         in
                             Expr (FixE (PrimApp (pos, Primop.BSet,
                                                  Vector.fromList [load, elabExpr env expr])))
                         end
                       | FlatAst0.Stmt.Guard (pos, dnf) => Guard (pos, DNF.map (elabExpr env) dnf)
                       | FlatAst0.Stmt.Expr expr => Expr (elabExpr env expr))
            val stmts' = Vector.map (elabStmt env) stmts
        in
            if NameSet.numItems names > 0
            then VectorExt.prepend stmts' (envDef pos env names)
            else stmts'
        end

    fun elabProc { name = name, clovers = clovers, cases = cases } =
        let fun elabCase (self, formals, envName, prologue, body) =
                (* MAYBE: We might not need the boxes here if we leave the order of argument binding
                          undefined. Any use of side effects (which the dynamic env is for) during
                          binding is bad style anyway. Do we punish for that with subtle bugs or
                          penalize every lambda-bound dynamic variable (which might be rare)? *)
                let val pos = FlatAst0.stmtPos (Vector.sub (prologue, 0))
                    val names = stmtVecBindings prologue
                    val env = Env.push (Env.root envName) names
                in ( self, formals, envName
                   , elabStmts pos env names prologue
                   , elabExpr env body )
                end
        in
            { name = name, clovers = clovers, cases = Vector.map elabCase cases }
        end

    fun convert { procs = procs, main = (stmts, expr) } =
        let val pos = FlatAst0.stmtPos (Vector.sub (stmts, 0))
            val envName = Name.freshFromString "denv"
            val alloc = FixE (PrimApp (pos, Primop.EmptyDEnv, Vector.fromList []))
            val def = FixS (Def (pos, envName, alloc))
            val names = stmtVecBindings stmts
            val env = Env.push (Env.root envName) names
        in
            { procs = Vector.map elabProc procs
            , main = ( VectorExt.prepend (elabStmts pos env names stmts) def
                     , elabExpr env expr ) }
        end
end
