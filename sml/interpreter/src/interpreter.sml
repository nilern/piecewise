structure Interpreter :> sig
    val interpret : Value.expr -> Value.value
end = struct
    structure Env :> sig
        type t

        val empty : t
        val pushBlock : t -> string vector -> t
        val lookup : t -> string -> Value.value
    end = struct
        structure StringMap = BinaryMapFn(type ord_key = string
                                          val compare = String.compare)

        type t = Value.value StringMap.map

        val empty = StringMap.empty

        fun pushBlock env names =
            Vector.foldl (fn (name, env) => StringMap.insert (env, name, Value.uninitialized ()))
                         env names

        fun lookup env name = StringMap.lookup (env, name)
    end

    datatype cont = Stmt of cont * Env.t * Env.t * Value.stmt vector * int * Value.expr
                  | Def of cont * Env.t * Env.t * Value.triv
                  | Halt

    fun eval cont lenv denv =
        fn Value.Block (_, stmts, expr) =>
           if Vector.length stmts = 0
           then eval cont lenv denv expr
           else let val lenv = Env.pushBlock lenv (Value.blockBinders Value.lexName stmts)
                    val denv = Env.pushBlock denv (Value.blockBinders Value.dynName stmts)
                in exec (Stmt (cont, lenv, denv, stmts, 0, expr)) lenv denv (Vector.sub (stmts, 0))
                end
         | Value.Triv (_, Value.Lex name) => continue (Env.lookup lenv name) cont
         | Value.Triv (_, Value.Dyn name) => continue (Env.lookup denv name) cont
         | Value.Triv (_, Value.Const v) => continue v cont

    and exec cont lenv denv =
        fn Value.Def (Value.Triv (_, var as Value.Lex _), NONE, expr) =>
           eval (Def (cont, lenv, denv, var)) lenv denv expr
         | Value.Def (Value.Triv (_, var as Value.Dyn _), NONE, expr) =>
           eval (Def (cont, lenv, denv, var)) lenv denv expr
         | Value.Expr expr => eval cont lenv denv expr

    and continue value =
        fn Stmt (cont, lenv, denv, stmts, i, expr) =>
            let val i = i + 1
            in if i < (Vector.length stmts)
               then exec (Stmt (cont, lenv, denv, stmts, i, expr)) lenv denv (Vector.sub (stmts, i))
               else eval cont lenv denv expr
            end
         | Def (cont, lenv, denv, Value.Lex name) =>
            ( Value.initialize (Env.lookup lenv name) value
            ; continue value cont )
         | Def (cont, lenv, denv, Value.Dyn name) =>
            ( Value.initialize (Env.lookup denv name) value
            ; continue value cont )
         | Halt => value

    fun interpret expr = eval Halt Env.empty Env.empty expr
end
