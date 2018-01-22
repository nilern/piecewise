structure Interpreter :> sig
    val interpret : Value.expr -> Value.value
end = struct
    type value = Value.value
    type expr = Value.expr
    type stmt = Value.stmt

    val wrap = Value.wrap
    val force = Value.force

    datatype cont = Callee of cont * value Env.t * value Env.t * expr vector
                  | Arg of cont * value Env.t * value Env.t * expr vector * int * value * value list
                  | PrimArg of cont * value Env.t * value Env.t * expr vector
                             * int * string * value list
                  | Stmt of cont * value Env.t * value Env.t * stmt vector * int * expr
                  | Init of cont * value Env.t * value Env.t * Value.var
                  | Halt

    fun lookup dump lenv denv =
        fn Value.Lex name => Env.lookup lenv name
         | Value.Dyn name => valOf (OptionExt.orElse (Env.find denv name)
                                                     (fn () => Dump.find dump name))

    fun define lenv denv var value =
        case var
        of Value.Lex name => (Env.insert lenv name value, denv)
         | Value.Dyn name => (lenv, Env.insert denv name value)

    fun declare lenv denv stmts =
        ( Env.pushBlock lenv (Value.blockBinders Value.lexName stmts) Value.uninitialized
        , Env.pushBlock denv (Value.blockBinders Value.dynName stmts) Value.uninitialized )

    fun eval dump cont lenv denv =
        fn Value.Fn (_, methods) =>
            continue (wrap (Value.Closure (methods, lenv))) dump cont
         | Value.Call (_, callee, args) =>
            eval dump (Callee (cont, lenv, denv, args)) lenv denv callee
         | Value.PrimCall (_, opcode, args) =>
            let val i = 0
            in if i < Vector.length args
               then let val cont = PrimArg (cont, lenv, denv, args, i, opcode, [])
                    in eval dump cont lenv denv (Vector.sub (args, i))
                    end
               else primApply dump cont denv opcode (Vector.fromList [])
            end
         | Value.Block (_, stmts, expr) =>
            let val i = 0
            in if i < Vector.length stmts
               then let val (lenv, denv) = declare lenv denv stmts
                        val cont = Stmt (cont, lenv, denv, stmts, i, expr)
                    in exec dump cont lenv denv (Vector.sub (stmts, i))
                    end
               else eval dump cont lenv denv expr
            end
         | Value.Var (_, var) => continue (lookup dump lenv denv var) dump cont
         | Value.Const (_, v) => continue v dump cont

    and exec dump cont lenv denv =
        fn Value.Def (Value.Var (_, var), NONE, expr) =>
           eval dump (Init (cont, lenv, denv, var)) lenv denv expr
         | Value.Expr expr => eval dump cont lenv denv expr

    and continue value dump =
        fn Callee (cont, lenv, denv, argExprs) =>
            let val i = 0
            in if i < Vector.length argExprs
               then let val cont = Arg (cont, lenv, denv, argExprs, i, value, [])
                    in eval dump cont lenv denv (Vector.sub (argExprs, i))
                    end
               else apply dump cont denv value (wrap (Value.Tuple (Vector.fromList [])))
            end
         | Arg (cont, lenv, denv, argExprs, i, callee, argValues) =>
            let val i = i + 1
            in if i < Vector.length argExprs
               then let val cont = Arg (cont, lenv, denv, argExprs, i, callee, value :: argValues)
                    in eval dump cont lenv denv (Vector.sub (argExprs, i))
                    end
               else let val argv = VectorExt.fromListRev (value :: argValues)
                    in apply dump cont denv callee (wrap (Value.Tuple argv))
                    end
            end
         | PrimArg (cont, lenv, denv, argExprs, i, opcode, argValues) =>
            let val i = i + 1
            in if i < Vector.length argExprs
               then let val cont =
                            PrimArg (cont, lenv, denv, argExprs, i, opcode, value :: argValues)
                    in eval dump cont lenv denv (Vector.sub (argExprs, i))
                    end
               else let val argv = VectorExt.fromListRev (value :: argValues)
                    in primApply dump cont denv opcode argv
                    end
            end
         | Stmt (cont, lenv, denv, stmts, i, expr) =>
            let val i = i + 1
            in if i < Vector.length stmts
               then let val cont = Stmt (cont, lenv, denv, stmts, i, expr)
                    in exec dump cont lenv denv (Vector.sub (stmts, i))
                    end
               else eval dump cont lenv denv expr
            end
         | Init (cont, lenv, denv, var) =>
            ( Value.initialize (lookup dump lenv denv var) value
            ; continue value dump cont)
         | Halt =>
           (case Dump.pop dump
            of SOME (cont', dump') => continue value dump' cont'
             | NONE => value)

    and apply dump cont denv callee args =
        case force callee
        of SOME (Value.Closure (methods, lenv)) =>
            let val Value.Method (pats, _, body) = Vector.sub (methods, 0)
            in case Vector.sub (pats, 0)
               of Value.Var (_, var) =>
                   let val (lenv, denv) = define lenv denv var args
                   in eval dump cont lenv denv body
                   end
            end

    and primApply dump cont denv opcode argv =
        case opcode
        of "iAdd" =>
            if Vector.length argv = 2
            then case (force (Vector.sub (argv, 0)), force (Vector.sub (argv, 1)))
                 of (SOME (Value.Int a), SOME (Value.Int b)) =>
                     continue (Value.wrap (Value.Int (a + b))) dump cont
                  | _ => raise Fail "primApply: arg types"
            else raise Fail "primApply: argc"

    fun interpret expr = eval Dump.empty Halt Env.empty Env.empty expr
end
