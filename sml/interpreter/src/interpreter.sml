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
                  | Def of cont * Env.t * Env.t * Value.var
                  | Halt

    fun lookup lenv _ (Value.Lex name) = Env.lookup lenv name
      | lookup _ denv (Value.Dyn name) = Env.lookup denv name

    fun eval cont lenv denv =
        fn Value.Block (_, stmts, expr) =>
           if Vector.length stmts = 0
           then eval cont lenv denv expr
           else let val lenv = Env.pushBlock lenv (Value.blockBinders Value.lexName stmts)
                    val denv = Env.pushBlock denv (Value.blockBinders Value.dynName stmts)
                in exec (Stmt (cont, lenv, denv, stmts, 0, expr)) lenv denv (Vector.sub (stmts, 0))
                end
         | Value.Var (_, var) => continue (lookup lenv denv var) cont
         | Value.Const (_, v) => continue v cont

    and exec cont lenv denv =
        fn Value.Def (Value.Var (_, var), NONE, expr) =>
           eval (Def (cont, lenv, denv, var)) lenv denv expr
         | Value.Expr expr => eval cont lenv denv expr

    and continue value =
        fn Stmt (cont, lenv, denv, stmts, i, expr) =>
            let val i = i + 1
            in if i < (Vector.length stmts)
               then exec (Stmt (cont, lenv, denv, stmts, i, expr)) lenv denv (Vector.sub (stmts, i))
               else eval cont lenv denv expr
            end
         | Def (cont, lenv, denv, var) =>
            ( Value.initialize (lookup lenv denv var) value
            ; continue value cont)
         | Halt => value

    fun interpret expr = eval Halt Env.empty Env.empty expr
end
