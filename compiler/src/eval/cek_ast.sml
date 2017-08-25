structure CekAst :> sig
    val interpret : (AuglessAst.expr, AuglessAst.stmt) Block.t -> Value.t
end = struct
    structure AAst = AuglessAst
    structure Stmt = AuglessAst.Stmt
    structure Expr = AuglessAst.Expr
    structure Triv = Expr.Triv
    structure LVar = BaseVar

    (* TODO: type ctx = { cont = Frame.t, lenv = Env.t, denv = Env.t } *)

    structure Env :> sig (* HACK: raises Option in various places *)
        type t

        val empty : t
        val find : t -> Name.t -> Value.t
        val pushBlock : t -> NameSet.set -> t
        val def : t -> Name.t -> Value.t -> unit
    end = struct
        datatype t = Block of t * Value.t option ref NameMap.map
                   | Top

        val empty = Top

        fun findVar (Block (p, bs)) name =
            OptionExt.orElse (NameMap.find (bs, name)) (fn () => findVar p name)
          | findVar Top _ = NONE

        fun find env name = valOf (! (valOf (findVar env name)))

        fun pushBlock env names =
            let val bindings = NameSet.foldl (fn (name, bs) => NameMap.insert (bs, name, ref NONE))
                                             NameMap.empty names
            in Block (env, bindings)
            end

        fun def env name value = valOf (findVar env name) := SOME value
    end

    type envs = {lex: Env.t, dyn: Env.t}

    structure Frame :> sig
        datatype t = Block of t * envs * int * (AuglessAst.expr, AuglessAst.stmt) Block.t
                   | Def of t * envs * BaseVar.t
                   | Halt
    end = struct
        datatype t = Block of t * envs * int * (AuglessAst.expr, AuglessAst.stmt) Block.t
                   | Def of t * envs * BaseVar.t
                   | Halt
    end

    fun stmtVecBindings stmts =
        let fun step (AAst.FixS stmt, bs as (lbs, dbs)) =
                case stmt
                of Stmt.Def (_, LVar.Lex name, _) => (NameSet.add (lbs, name), dbs)
                 | Stmt.Def (_, LVar.Dyn name, _) => (lbs, NameSet.add (lbs, name))
                 | Stmt.Guard _ => bs
                 | Stmt.Expr _ => bs
        in Vector.foldl step (NameSet.empty, NameSet.empty) stmts
        end

    fun continue cont envs value =
        case cont
        of Frame.Block (cont, envs, i, block as (stmts, expr)) =>
           let val i = i + 1
           in if i < Vector.length stmts
              then evalStmt (Frame.Block (cont, envs, i, block)) envs (Vector.sub (stmts, i))
              else evalExpr cont envs expr
           end
         | Frame.Def (cont, envs, LVar.Lex name) =>
           ( Env.def (#lex envs) name value
           ; continue cont envs value) (* HACK: value is always ignored so just returns something *)
         | Frame.Halt => value

    and evalConst cont envs const =
        case const
        of Const.Int i => continue cont envs (Value.Int i)

    and evalTriv cont envs triv =
        case triv
        of Triv.Var (RVar.Current (LVar.Lex name)) =>
           continue cont envs (Env.find (#lex envs) name)
         | Triv.Const const => evalConst cont envs const

    and evalExpr cont envs (AAst.FixE expr) =
        case expr
        of Expr.Block (_, block) => evalBlock cont envs block
         | Expr.Triv (_, triv) => evalTriv cont envs triv

    and evalStmt cont envs (AAst.FixS stmt) =
        case stmt
        of Stmt.Def (_, var, expr) => evalExpr (Frame.Def (cont, envs, var)) envs expr
         | Stmt.Expr expr => evalExpr cont envs expr

    and evalBlock cont envs (block as (stmts, expr)) =
        let val (lbs, _ (* TODO: dbs *)) = stmtVecBindings stmts
            val envs = { lex = Env.pushBlock (#lex envs) lbs
                       , dyn = #dyn envs }
        in case Vector.length stmts
           of 0 => evalExpr cont envs expr
            | n => evalStmt (Frame.Block (cont, envs, 0, block)) envs (Vector.sub (stmts, 0))
        end

    val interpret = evalBlock Frame.Halt {lex = Env.empty, dyn = Env.empty}
end
