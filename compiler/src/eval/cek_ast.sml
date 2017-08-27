structure CekAst :> sig
    structure Value : TO_DOC

    exception Argc of Pos.t * int * int
    exception Type of Pos.t * Type.t * Type.t

    val interpret : (AuglessAst.expr, AuglessAst.stmt) Block.t -> Value.t
end = struct
    structure AAst = AuglessAst
    structure Stmt = AAst.Stmt
    structure Expr = AAst.Expr
    structure Triv = Expr.Triv
    structure LVar = BaseVar

    (* TODO: use indirection values for recursive defs instead of hiding ref:s in Env.t *)

    structure Env :> sig (* HACK: raises Option in various places *)
        type 'v t

        val empty : 'v t
        val find : 'v t -> Name.t -> 'v
        val pushBlock : 'v t -> NameSet.set -> 'v t
        val pushFn : 'v t -> Name.t -> 'v -> 'v t
        val def : 'v t -> Name.t -> 'v -> unit
    end = struct
        datatype 'v t = Block of 'v t * 'v option ref NameMap.map
                      | Fn of 'v t * Name.t * 'v
                      | Top

        val empty = Top

        fun findVar (Block (p, bs)) name =
            OptionExt.orElse (NameMap.find (bs, name)) (fn () => findVar p name)
          | findVar (Fn (p, params, arg)) name =
            if name = params then SOME (ref (SOME arg)) (* HACK *) else findVar p name
          | findVar Top _ = NONE

        fun find env name = valOf (! (valOf (findVar env name)))

        fun pushBlock env names =
            let val bindings = NameSet.foldl (fn (name, bs) => NameMap.insert (bs, name, ref NONE))
                                             NameMap.empty names
            in Block (env, bindings)
            end

        fun pushFn env params args = Fn (env, params, args)

        fun def env name value = valOf (findVar env name) := SOME value
    end

    type 'v envs = {lex: 'v Env.t, dyn: 'v Env.t}

    structure Frame = struct
        type ('f, 'v) ctx = {cont: 'f, env: 'v envs}

        datatype 'v t = Block of ('v t, 'v) ctx * int * (AAst.expr, AAst.stmt) Block.t
                      | Def of ('v t, 'v) ctx * BaseVar.t
                      | Callee of ('v t, 'v) ctx * AAst.expr
                      | Arg of ('v t, 'v) ctx * 'v
                      | PrimArg of ('v t, 'v) ctx * Primop.t * int * AAst.expr vector * 'v list
                      | Case of ('v t, 'v) ctx * int * (AAst.prologue * AAst.expr) vector
                      | Halt
    end

    structure Value = struct
        datatype t = Int of LargeInt.int
                   | Float of LargeReal.real
                   | Char of WideChar.char
                   | Bool of bool
                   | Tuple of t vector
                   | Fn of ((Pos.t * RVar.t option * Name.t * (AAst.prologue * AAst.expr) vector)
                            * t Env.t) vector

        val typeOf =
            fn Int _ => Type.Int
             | Float _ => Type.Float
             | Char _ => Type.Char
             | Bool _ => Type.Bool
             | Tuple _ => Type.Tuple
             | Fn _ => Type.Fn

        val rec toString =
            fn Int i => LargeInt.toString i
             | Float f => LargeReal.toString f
             | Char c => WideChar.toString c
             | Bool true => "True"
             | Bool false => "False"
             | Tuple vs => "(" ^ Vector.foldl (fn (v, acc) => acc ^ ", " ^ toString v) "" vs ^ ")"
             | Fn _ => "#<fn>"

        val toDoc = PPrint.text o toString
    end

    exception Argc of Pos.t * int * int
    exception Type of Pos.t * Type.t * Type.t

    val primopArity =
        fn Primop.Tuple => NONE

    fun stmtVecBindings stmts =
        let fun step (AAst.FixS stmt, bs as (lbs, dbs)) =
                case stmt
                of Stmt.Def (_, LVar.Lex name, _) => (NameSet.add (lbs, name), dbs)
                 | Stmt.Def (_, LVar.Dyn name, _) => (lbs, NameSet.add (lbs, name))
                 | Stmt.Guard _ => bs
                 | Stmt.Expr _ => bs
        in Vector.foldl step (NameSet.empty, NameSet.empty) stmts
        end

    fun evalExpr ctx (AAst.FixE expr) =
        let fun evalTriv ctx pos triv =
                let fun evalConst ctx const =
                        case const
                        of Const.Int i => continue ctx pos (Value.Int i)
                         | Const.Char c => raise Fail "unimplemented"
                         | Const.Symbol s => raise Fail "unimplemented"
                         | Const.String s => raise Fail "unimplemented"
                in case triv
                   of Triv.Var (RVar.Current var) =>
                      let val value = case var
                                      of LVar.Lex name => Env.find (#lex (#env ctx)) name
                                       | LVar.Dyn name => Env.find (#dyn (#env ctx)) name
                      in continue ctx pos value
                      end
                    | Triv.Var (RVar.Upper var) => raise Fail "unimplemented"
                    | Triv.Const const => evalConst ctx const
                end
        in case expr
           of Expr.Fn (cob as (pos, _, _, _)) =>
              continue ctx pos (Value.Fn (Vector.fromList [(cob, (#lex (#env ctx)))]))
            | Expr.Block (_, block) => evalBlock ctx block
            | Expr.Call (pos, f, args) =>
              if Vector.length args = 1
              then let val {cont = cont, env = env} = ctx
                       val cont = Frame.Callee (ctx, Vector.sub (args, 0))
                   in evalExpr {cont = cont, env = env} f
                   end
              else raise Argc (pos, 1, Vector.length args)
            | Expr.PrimCall (pos, po, args) =>
              let val arity = primopArity po
                  val argc = Vector.length args
              in if arity = NONE orelse argc = valOf arity
                 then case argc
                      of 0 => applyPrim ctx pos po (Vector.fromList [])
                       | _ => let val {cont = cont, env = env} = ctx
                                  val cont = Frame.PrimArg (ctx, po, 0, args, [])
                              in evalExpr {cont = cont, env = env} (Vector.sub (args, 0))
                              end
                 else raise Argc (pos, valOf arity, Vector.length args)
              end
            | Expr.Triv (pos, triv) => evalTriv ctx pos triv
        end

    and evalStmt (ctx as {cont = _, env = env}) (AAst.FixS stmt) =
        case stmt
        of Stmt.Def (_, var, expr) => evalExpr {cont = Frame.Def (ctx, var), env = env} expr
         | Stmt.Guard (_, dnf) => raise Fail "unimplemented"
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

    and continue {cont = cont, env = _} pos value =
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
           ; continue ctx pos value) (* HACK: just returns something *)
         | Frame.Callee (ctx as {cont = _, env = env}, arg) =>
           let val cont = Frame.Arg (ctx, value)
           in evalExpr {cont = cont, env = env} arg
           end
         | Frame.Arg (ctx as {cont = cont, env = {lex = _, dyn = denv}}, f) =>
           (case f
            of Value.Fn elems =>
               let val ((_, _, params, cases), lenv) = Vector.sub (elems, 0)
                   val env = {lex = Env.pushFn lenv params value, dyn = denv}
                   val (AAst.Prolog (_, bindStmts), body) = Vector.sub (cases, 0)
               in evalBlock {cont = cont, env = env} (bindStmts, body) (* HACK *)
               end
             | _ => raise Type (pos, Type.Fn, Value.typeOf value))
         | Frame.PrimArg (ctx as {cont = _, env = env}, po, i, argExprs, args) =>
           let val i = i + 1
           in if i < Vector.length argExprs
              then let val cont = Frame.PrimArg (ctx, po, i, argExprs, value :: args)
                   in evalExpr {cont = cont, env = env} (Vector.sub (argExprs, i))
                   end
              else applyPrim ctx pos po (Vector.fromList (List.rev args))
           end
         | Frame.Halt => value

    and applyPrim ctx pos po args =
        case po
        of Primop.Tuple => continue ctx pos (Value.Tuple args)

    val interpret = evalBlock {cont = Frame.Halt, env = {lex = Env.empty, dyn = Env.empty}}
end
