structure FlatAst1 = FlatAst(Name)

(* FIXME: if no dynamic vars are defined in a scope, App:s still use the unemitted denv *)
structure ConvertDEnv :> sig
    val convert : FlatAst0.program -> FlatAst1.program
end = struct
    structure Env :> sig
        type t

        val root : Name.t -> t
        val push : t -> Name.t -> t
        val name : t -> Name.t
        val parent : t -> t option
    end = struct
        datatype t = Root of Name.t
                   | Cons of Name.t * t

        val root = Root
        fun push env newName = Cons (newName, env)
        val name = fn Root name => name
                    | Cons (name, _) => name
        val parent = fn Root _ => NONE
                      | Cons (_, parent) => SOME parent
    end

    val FixE = FlatAst1.FixE
    val FixS = FlatAst1.FixS
    val FixBS = FlatAst1.FixBS
    val Block = FlatAst1.Expr.Block
    val App = FlatAst1.Expr.App
    val PrimApp = FlatAst1.Expr.PrimApp
    val Var = FlatAst1.Expr.Var
    val Const = FlatAst1.Expr.Const
    val Def = AuglessStmt.Def
    val Expr = AuglessStmt.Expr

    fun envPair pos name =
        Vector.fromList [ FixE (Const (pos, Const.Symbol (Name.toString name)))
                        , FixE (PrimApp (pos, Primop.Box, VectorExt.empty ())) ]

    fun envDef pos env names =
        let val oldEnvName = Env.name (Option.valOf (Env.parent env))
            val temp = Name.freshFromString "denv"
            val newEnvName = Env.name env
            val bdef = FixBS (BindStmt1.Def (newEnvName, FixE (Var (pos, temp))))
            val bind = FlatAst1.Bind (pos, DNF.always (), VectorExt.singleton bdef)
            val dePair = envPair pos
            val dePairs = VectorExt.flatMap dePair (Vector.fromList (NameSet.listItems names))
            val alloc =
                FixE (PrimApp (pos, Primop.DEnv,
                               VectorExt.prepend dePairs (FixE (Var (pos, oldEnvName)))))
        in FixS (Def (temp, bind, alloc))
        end

    fun envBindDef pos env names =
        let val oldEnvName = Env.name (Option.valOf (Env.parent env))
            val newEnvName = Env.name env
            val dePair = envPair pos
            val dePairs = VectorExt.flatMap dePair (Vector.fromList (NameSet.listItems names))
            val alloc =
                FixE (PrimApp (pos, Primop.DEnv,
                               VectorExt.prepend dePairs (FixE (Var (pos, oldEnvName)))))
        in FixBS (BindStmt1.Def (newEnvName, alloc))
        end

    fun elabBindStmt env (FlatAst0.FixBS bstmt, (bstmts', names)) =
        case bstmt
        of BindStmt1.Def (Var.Lex name, expr) =>
           let val bstmt' = FixBS (BindStmt1.Def (name, elabExpr env expr))
           in (VectorExt.conj bstmts' bstmt', names)
           end
         | BindStmt1.Def (Var.Dyn name, expr) =>
           let val pos = FlatAst0.Expr.pos (FlatAst0.unwrapE expr)
               val nameSym = Const.Symbol (Name.toString name)
               val load =
                   (FixE (PrimApp ( pos
                                  , Primop.DGet
                                  , Vector.fromList [ FixE (Var (pos, Env.name env))
                                                    , FixE (Const (pos, nameSym)) ])))
               val bstmt' =
                   FixBS (BindStmt1.Expr
                       (FixE (PrimApp (pos, Primop.BSet,
                                       Vector.fromList [load, elabExpr env expr]))))
           in (VectorExt.conj bstmts' bstmt', NameSet.add (names, name))
           end
         | BindStmt1.Expr expr =>
           let val bstmt' = FixBS (BindStmt1.Expr (elabExpr env expr))
           in (VectorExt.conj bstmts' bstmt', names)
           end

    and elabBind env (FlatAst0.Bind (pos, dnf, bstmts)) names =
        let val (bstmts', names') =
                Vector.foldl (elabBindStmt env) (VectorExt.empty (), names) bstmts
        in (FlatAst1.Bind (pos, DNF.map (elabExpr env) dnf, bstmts'), names')
        end

    and elabExpr env (FlatAst0.FixE expr) =
            FixE (case expr
                  of FlatAst0.Expr.Block (pos, stmts) => Block (pos, elabStmts pos env stmts)
                   | FlatAst0.Expr.App (pos, f, args) =>
                     let val args' = Vector.map (elabExpr env) args
                         val deExpr = FixE (Var (pos, Env.name env))
                     in App (pos, elabExpr env f, VectorExt.prepend args' deExpr)
                     end
                   | FlatAst0.Expr.PrimApp (pos, po, args) =>
                     PrimApp (pos, po, Vector.map (elabExpr env) args)
                   | FlatAst0.Expr.Var (pos, Var.Lex name) => Var (pos, name)
                   | FlatAst0.Expr.Var (pos, Var.Dyn name) =>
                     let val nameSym = Const.Symbol (Name.toString name)
                     in PrimApp (pos, Primop.DGet, Vector.fromList [ FixE (Var (pos, Env.name env))
                                                                   , FixE (Const (pos, nameSym)) ])
                     end
                   | FlatAst0.Expr.Const (pos, c) => Const (pos, c))

    and elabStmts pos env stmts =
        let fun elabStmt env (FlatAst0.FixS stmt, (stmts', names)) =
                case stmt
                of AuglessStmt.Def (temp, bind, expr) =>
                   let val (bind', names') = elabBind env bind names
                       val stmt' = FixS (AuglessStmt.Def (temp, bind', elabExpr env expr))
                   in (VectorExt.conj stmts' stmt', names')
                   end
                 | AuglessStmt.Expr expr =>
                   let val stmt' = FixS (AuglessStmt.Expr (elabExpr env expr))
                   in (VectorExt.conj stmts' stmt', names)
                   end
            val env' = Env.push env (Name.freshFromString "denv")
            val (stmts', names) =
                Vector.foldl (elabStmt env') (VectorExt.empty (), NameSet.empty) stmts
        in
            if NameSet.numItems names > 0
            then VectorExt.prepend stmts' (envDef pos env' names)
            else stmts'
        end

    fun elabProc { name = name, clovers = clovers, cases = cases } =
        let fun elabCase (self, formals, envName, (FlatAst0.Bind (pos, dnf, bstmts)), body) =
                (* TODO: here we don't actually need the boxes since there can be no recursive defs *)
                let val env = Env.push (Env.root envName) (Name.freshFromString "denv")
                    val (bstmts', names) =
                        Vector.foldl (elabBindStmt env) (VectorExt.empty (), NameSet.empty) bstmts
                    val bstmts'' = if NameSet.numItems names > 0
                                   then VectorExt.prepend bstmts' (envBindDef pos env names)
                                   else bstmts'
                    val bind' = FlatAst1.Bind (pos, DNF.map (elabExpr env) dnf, bstmts'')
                in (self, formals, envName, bind', elabExpr env body)
                end
        in
            { name = name, clovers = clovers, cases = Vector.map elabCase cases }
        end

    fun convert { procs = procs, main = main } =
        let val pos = Pos.def
            val envName = Name.freshFromString "denv"
            val env = Env.root envName
            val temp = Name.freshFromString "denv"
            val bdef = FixBS (BindStmt1.Def (envName, FixE (Var (pos, temp))))
            val bind = FlatAst1.Bind (pos, DNF.always (), VectorExt.singleton bdef)
            val alloc = FixE (PrimApp (pos, Primop.EmptyDEnv, Vector.fromList []))
            val def = FixS (AuglessStmt.Def (temp, bind, alloc))
        in
            { procs = Vector.map elabProc procs
            , main = VectorExt.prepend (elabStmts pos env main) def }
        end
end
