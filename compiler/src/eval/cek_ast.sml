structure CekAst :> sig
    structure Value : TO_DOC

    val interpret : (AuglessAst.expr, AuglessAst.stmt) Block.t -> Value.t
end = struct
    structure AAst = AuglessAst
    structure Stmt = AuglessAst.Stmt
    structure Expr = AuglessAst.Expr
    structure Triv = Expr.Triv
    structure LVar = BaseVar

    (* TODO: use indirection values for recursive defs instead of hiding ref:s in Env.t *)

    structure Env :> sig (* HACK: raises Option in various places *)
        type 'v t

        val empty : 'v t
        val find : 'v t -> Name.t -> 'v
        val pushBlock : 'v t -> NameSet.set -> 'v t
        val def : 'v t -> Name.t -> 'v -> unit
    end = struct
        datatype 'v t = Block of 'v t * 'v option ref NameMap.map
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

    type 'v envs = {lex: 'v Env.t, dyn: 'v Env.t}

    structure Frame = struct
        type ('f, 'v) ctx = {cont: 'f, env: 'v envs}

        datatype 'v t = Block of ('v t, 'v) ctx * int * (AuglessAst.expr, AuglessAst.stmt) Block.t
                      | Def of ('v t, 'v) ctx * BaseVar.t
                      | Halt
    end

    structure Value = struct
        datatype t = Int of LargeInt.int
                   | Float of LargeReal.real
                   | Char of WideChar.char
                   | Bool of bool

        val toString =
            fn Int i => LargeInt.toString i
             | Float f => LargeReal.toString f
             | Char c => WideChar.toString c
             | Bool true => "True"
             | Bool false => "False"

        val toDoc = PPrint.text o toString
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

    fun continue {cont = cont, env = _} value =
        case cont
        of Frame.Block (ctx as {cont = _, env = env}, i, block as (stmts, expr)) =>
           let val i = i + 1
           in if i < Vector.length stmts
              then evalStmt {cont = Frame.Block (ctx, i, block), env = env} (Vector.sub (stmts, i))
              else evalExpr ctx expr
           end
         | Frame.Def (ctx as {cont = _, env = {lex = lenv, dyn = denv}}, var) =>
           ( case var
             of LVar.Lex name => Env.def lenv name value
              | LVar.Dyn name => Env.def denv name value
           ; continue ctx value) (* HACK: just returns something *)
         | Frame.Halt => value

    and evalConst ctx const =
        case const
        of Const.Int i => continue ctx (Value.Int i)

    and evalTriv ctx triv =
        case triv
        of Triv.Var (RVar.Current var) =>
           let val value = case var
                           of LVar.Lex name => Env.find (#lex (#env ctx)) name
                            | LVar.Dyn name => Env.find (#dyn (#env ctx)) name
           in continue ctx value
           end
         | Triv.Const const => evalConst ctx const

    and evalExpr ctx (AAst.FixE expr) =
        case expr
        of Expr.Block (_, block) => evalBlock ctx block
         | Expr.Triv (_, triv) => evalTriv ctx triv

    and evalStmt (ctx as {cont = _, env = env}) (AAst.FixS stmt) =
        case stmt
        of Stmt.Def (_, var, expr) => evalExpr {cont = Frame.Def (ctx, var), env = env} expr
         | Stmt.Expr expr => evalExpr ctx expr

    and evalBlock {cont = cont, env = {lex = lenv, dyn = denv}} (block as (stmts, expr)) =
        let val (lbs, dbs) = stmtVecBindings stmts
            val env = { lex = Env.pushBlock lenv lbs
                      , dyn = Env.pushBlock denv dbs }
            val ctx = {cont = cont, env = env}
        in case Vector.length stmts
           of 0 => evalExpr ctx expr
            | n => evalStmt {cont = Frame.Block (ctx, 0, block), env = env} (Vector.sub (stmts, 0))
        end

    val interpret = evalBlock {cont = Frame.Halt, env = {lex = Env.empty, dyn = Env.empty}}
end
