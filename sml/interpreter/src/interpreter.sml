structure Interpreter :> sig
    val interpret : Value.expr -> Value.value
end = struct
    datatype cont = Stmt of cont * Value.value Env.t * Value.value Env.t
                          * Value.stmt vector * int * Value.expr
                  | Def of cont * Value.value Env.t * Value.value Env.t * Value.var
                  | Halt

    fun lookup dump lenv _ (Value.Lex name) = Env.lookup lenv name
      | lookup dump _ denv (Value.Dyn name) =
        valOf (OptionExt.orElse (Env.find denv name) (fn () => Dump.find dump name))

    fun eval dump cont lenv denv =
        fn Value.Block (_, stmts, expr) =>
           if Vector.length stmts = 0
           then eval dump cont lenv denv expr
           else let val lenv = Env.pushBlock lenv (Value.blockBinders Value.lexName stmts)
                                             Value.uninitialized
                    val denv = Env.pushBlock denv (Value.blockBinders Value.dynName stmts)
                                             Value.uninitialized
                in exec dump (Stmt (cont, lenv, denv, stmts, 0, expr))
                        lenv denv (Vector.sub (stmts, 0))
                end
         | Value.Var (_, var) => continue (lookup dump lenv denv var) dump cont
         | Value.Const (_, v) => continue v dump cont

    and exec dump cont lenv denv =
        fn Value.Def (Value.Var (_, var), NONE, expr) =>
           eval dump (Def (cont, lenv, denv, var)) lenv denv expr
         | Value.Expr expr => eval dump cont lenv denv expr

    and continue value dump =
        fn Stmt (cont, lenv, denv, stmts, i, expr) =>
            let val i = i + 1
            in if i < (Vector.length stmts)
               then exec dump (Stmt (cont, lenv, denv, stmts, i, expr))
                         lenv denv (Vector.sub (stmts, i))
               else eval dump cont lenv denv expr
            end
         | Def (cont, lenv, denv, var) =>
            ( Value.initialize (lookup dump lenv denv var) value
            ; continue value dump cont)
         | Halt => value

    fun interpret expr = eval Dump.empty Halt Env.empty Env.empty expr
end
