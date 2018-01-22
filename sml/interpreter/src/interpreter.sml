structure Interpreter :> sig
    val interpret : Value.expr -> Value.value
end = struct
    datatype cont = Callee of cont * Value.value Env.t * Value.value Env.t * Value.expr vector
                  | Arg of cont * Value.value Env.t * Value.value Env.t
                         * Value.expr vector * int * Value.value * Value.value list
                  | Stmt of cont * Value.value Env.t * Value.value Env.t
                          * Value.stmt vector * int * Value.expr
                  | Def of cont * Value.value Env.t * Value.value Env.t * Value.var
                  | Halt

    fun lookup dump lenv _ (Value.Lex name) = Env.lookup lenv name
      | lookup dump _ denv (Value.Dyn name) =
        valOf (OptionExt.orElse (Env.find denv name) (fn () => Dump.find dump name))

    fun define lenv denv var value =
        case var
        of Value.Lex name => (Env.insert lenv name value, denv)
         | Value.Dyn name => (lenv, Env.insert denv name value)

    fun eval dump cont lenv denv =
        fn Value.Fn (_, methods) =>
            continue (Value.wrap (Value.Closure (methods, lenv))) dump cont
         | Value.Call (_, callee, args) =>
            eval dump (Callee (cont, lenv, denv, args)) lenv denv callee
         | Value.Block (_, stmts, expr) =>
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
        fn Callee (cont, lenv, denv, argExprs) =>
           if Vector.length argExprs = 0
           then apply dump cont denv value (Value.wrap (Value.Tuple (Vector.fromList [])))
           else let val cont = Arg (cont, lenv, denv, argExprs, 0, value, [])
                in eval dump cont lenv denv (Vector.sub (argExprs, 0))
                end
         | Arg (cont, lenv, denv, argExprs, i, callee, argValues) =>
           let val i = i + 1
           in if i < (Vector.length argExprs)
              then let val cont = Arg (cont, lenv, denv, argExprs, i, callee, value :: argValues)
                   in eval dump cont lenv denv (Vector.sub (argExprs, i))
                   end
              else let val argv = VectorExt.fromListRev (value :: argValues)
                   in apply dump cont denv callee (Value.wrap (Value.Tuple argv))
                   end
           end
         | Stmt (cont, lenv, denv, stmts, i, expr) =>
            let val i = i + 1
            in if i < (Vector.length stmts)
               then let val cont = Stmt (cont, lenv, denv, stmts, i, expr)
                    in exec dump cont lenv denv (Vector.sub (stmts, i))
                    end
               else eval dump cont lenv denv expr
            end
         | Def (cont, lenv, denv, var) =>
            ( Value.initialize (lookup dump lenv denv var) value
            ; continue value dump cont)
         | Halt =>
           (case Dump.pop dump
            of SOME (cont', dump') => continue value dump' cont'
             | NONE => value)

    and apply dump cont denv callee args =
        case Value.force callee
        of SOME (Value.Closure (methods, lenv)) =>
            (case Vector.sub (methods, 0)
             of Value.Method (Value.Var (_, var), _, body) =>
                 let val (lenv, denv) = define lenv denv var args
                 in eval dump cont lenv denv body
                 end)

    fun interpret expr = eval Dump.empty Halt Env.empty Env.empty expr
end
