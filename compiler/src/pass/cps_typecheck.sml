structure CpsTypecheck :> sig
    exception Argc of Pos.t * int * int
    exception Unbound of Pos.t * Name.t
    exception Type of Pos.t * Type.t * Type.t
    exception NonLabel of Pos.t * Type.t

    val typecheck : Cps.program -> unit
end = struct
    structure Cont = Cps.Cont
    structure DominatorTree = Cps.DominatorTree
    structure Stmt = Cps.Stmt
    structure Expr = Cps.Expr
    structure Triv = Expr.Triv
    structure Transfer = Cps.Transfer
    type proc = Cps.proc

    exception Argc of Pos.t * int * int
    exception Unbound of Pos.t * Name.t
    exception Type of Pos.t * Type.t * Type.t
    exception NonLabel of Pos.t * Type.t

    fun contType { args = { names = _, types = types }, block = _ } =
        Type.Label types

    fun procType { pos = _, name = _, clovers = _
                 , cfg = { entry = entry, conts = conts } } =
        contType (NameMap.lookup (conts, entry))

    structure Env :> sig
        type t

        val empty : t
        val get : t -> Name.t -> Type.t option
        val assoc : t -> Name.t -> Type.t -> t
        val pushProcLabels : t -> proc NameMap.map -> t
        val pushContLabels : t -> Cont.t NameMap.map -> t
        val pushArgs : t -> Argv.t -> t
    end = struct
        type t = Type.t NameMap.map

        val empty = NameMap.empty

        fun get env name = NameMap.find (env, name)

        fun assoc env name ty = NameMap.insert (env, name, ty)

        fun pushProcLabels env procs =
            let fun pushProcLabel (label, proc, env) =
                    assoc env label (procType proc)
            in NameMap.foldli pushProcLabel env procs
            end

        fun pushContLabels env conts =
            let fun pushContLabel (label, cont, env) =
                    assoc env label (contType cont)
            in NameMap.foldli pushContLabel env conts
            end

        fun pushArgs env { names = names, types = types } =
            Vector.foldli (fn (i, name, env) => NameMap.insert (env, name, Vector.sub (types, i)))
                          env names
    end

    val constType =
        fn Const.Int _ => Type.Int
         | Const.Char _ => Type.Char

    fun trivType env =
        fn Triv.Var (FlatVar1.Data name) => valOf (Env.get env name)
         | Triv.Var (FlatVar1.Label name) => valOf (Env.get env name)
         | Triv.Const c => constType c

    fun exprType env =
        fn Expr.Call (_, f, args) => raise Fail "unimplemented"
         | Expr.PrimCall (pos, Primop.EmptyDEnv, args) =>
           let val argc = Vector.length args
           in if argc = 0 then Type.DynEnv else raise Argc (pos, 0, argc)
           end
         | Expr.Triv (_, triv) => trivType env triv

    fun checkExpr env expr = ignore (exprType env expr)

    fun checkStmt (stmt, env) =
        case stmt
        of Stmt.Def (_, name, expr) =>
           let val ty = exprType env expr
           in Env.assoc env name ty
           end
         | Stmt.Expr expr => (checkExpr env expr; env)

    fun checkTransfer env =
        fn Transfer.Continue (pos, label, args) =>
           (case Env.get env label
            of SOME (Type.Label pTypes) => raise Fail "unimplemented"
             | SOME ty => raise NonLabel (pos, ty)
             | NONE => raise Unbound (pos, label))
         | Transfer.Call (_, label, f, args) => raise Fail "unimplemented"
         | Transfer.Branch (_, cond, conseq, alt) => raise Fail "unimplemented"
         | Transfer.Halt (pos, v) =>
           case trivType env v
           of Type.Int => env
            | ty => raise Type (pos, Type.Int, ty)

    fun checkBlock env (stmts, transfer) =
        let val env = Vector.foldl checkStmt env stmts
        in checkTransfer env transfer
        end

    fun checkCont env { args = args, block = block } =
        let val env = Env.pushArgs env args
        in checkBlock env block
        end

    fun checkLabel env conts label =
        case NameMap.find (conts, label)
        of SOME cont => checkCont env cont
         | NONE => env

    fun checkConts env conts dtree =
        case dtree
        of DominatorTree.Branch (label, children) =>
           let val env = checkLabel env conts label
           in ignore (Vector.map (checkConts env conts) children)
           end
         | DominatorTree.Leaf label =>
           ignore (checkLabel env conts label)

    fun checkCfg env (cfg as { entry = _, conts = conts }) =
        let val env = Env.pushContLabels env conts
            val dtree = DominatorTree.ofCfg cfg (* OPTIMIZE: share this work between passes? *)
        in checkConts env conts dtree
        end

    fun checkProc env { pos = _, name = _, clovers = _, cfg = cfg } =
        checkCfg env cfg

    fun typecheck { procs = procs, main = main } =
        let val env = Env.empty
            val env = Env.pushProcLabels env procs
        in NameMap.app (checkProc env) procs
         ; checkCfg env main
        end
end
