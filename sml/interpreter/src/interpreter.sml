structure Interpreter :> sig
    val interpret : Value.expr -> Value.value
end = struct
    type value = Value.value
    type expr = Value.expr
    type stmt = Value.stmt

    val wrap = Value.wrap
    val force = Value.force

    type envs = { lex: value Env.t, dyn: value Env.t }

    datatype frame = Callee of envs * expr vector
                   | Arg of envs * expr vector * int * value * value list
                   | PrimArg of envs * expr vector * int * string * value list
                   | Stmt of envs * stmt vector * expr * int
                   | Match of envs * (* HACK: *) envs ref * expr vector * expr * int * value
                   | Init of envs * Value.var
                   | Def of Value.var

    fun lookup dump { lex = lenv, dyn = denv } =
        fn Value.Lex name => Env.lookup lenv name
         | Value.Dyn name => valOf (OptionExt.orElse (Env.find denv name)
                                                     (fn () => Dump.find dump name))

    fun insert { lex = lenv, dyn = denv } var value =
        case var
        of Value.Lex name => { lex = Env.insert lenv name value, dyn = denv }
         | Value.Dyn name => { lex = lenv, dyn = Env.insert denv name value }

    fun declare { lex = lenv, dyn = denv } stmts =
        { lex = Env.declare lenv (Value.blockBinders Value.lexName stmts) Value.uninitialized
        , dyn = Env.declare denv (Value.blockBinders Value.dynName stmts) Value.uninitialized }

    fun declareParams { lex = lenv, dyn = denv } pats =
        { lex = Env.declare lenv (VectorExt.flatMap (Value.patBinders Value.lexName) pats)
                            Value.uninitialized
        , dyn = Env.declare denv (VectorExt.flatMap (Value.patBinders Value.dynName) pats)
                            Value.uninitialized }

    fun define { lex = lenv, dyn = denv } { lex = lDelta, dyn = dDelta } =
        ( Env.define lenv lDelta Value.initialize
        ; Env.define denv dDelta Value.initialize )

    fun eval dump cont envs =
        fn Value.Fn (_, methods) =>
            continue (wrap (Value.Closure (methods, #lex envs))) dump cont
         | Value.Call (_, callee, args) =>
            eval dump (Callee (envs, args) :: cont) envs callee
         | Value.PrimCall (_, opcode, args) =>
            let val i = 0
            in if i < Vector.length args
               then let val cont = PrimArg (envs, args, i, opcode, []) :: cont
                    in eval dump cont envs (Vector.sub (args, i))
                    end
               else primApply dump cont (#dyn envs) opcode (Vector.fromList [])
            end
         | Value.Block (_, stmts, expr) =>
            let val i = 0
            in if i < Vector.length stmts
               then let val envs = declare envs stmts
                        val cont = Stmt (envs, stmts, expr, i) :: cont
                    in exec dump cont envs (Vector.sub (stmts, i))
                    end
               else eval dump cont envs expr
            end
         | Value.Var (_, var) => continue (lookup dump envs var) dump cont
         | Value.Const (_, v) => continue v dump cont

    and exec dump cont envs =
        fn Value.Def (Value.Var (_, var), NONE, expr) =>
           (* TODO: Use the `match`, `Match` etc. machinery here as well. *)
           eval dump (Init (envs, var) :: cont) envs expr
         | Value.Expr expr => eval dump cont envs expr

    and continue value dump =
        fn Callee (envs, argExprs) :: cont =>
            let val i = 0
            in if i < Vector.length argExprs
               then let val cont = Arg (envs, argExprs, i, value, []) :: cont
                    in eval dump cont envs (Vector.sub (argExprs, i))
                    end
               else apply dump cont (#dyn envs) value (wrap (Value.Tuple (Vector.fromList [])))
            end
         | Arg (envs, argExprs, i, callee, argValues) :: cont =>
            let val i = i + 1
            in if i < Vector.length argExprs
               then let val cont = Arg (envs, argExprs, i, callee, value :: argValues) :: cont
                    in eval dump cont envs (Vector.sub (argExprs, i))
                    end
               else let val argv = VectorExt.fromListRev (value :: argValues)
                    in apply dump cont (#dyn envs) callee (wrap (Value.Tuple argv))
                    end
            end
         | PrimArg (envs, argExprs, i, opcode, argValues) :: cont =>
            let val i = i + 1
            in if i < Vector.length argExprs
               then let val cont = PrimArg (envs, argExprs, i, opcode, value :: argValues) :: cont
                    in eval dump cont envs (Vector.sub (argExprs, i))
                    end
               else let val argv = VectorExt.fromListRev (value :: argValues)
                    in primApply dump cont (#dyn envs) opcode argv
                    end
            end
         | Stmt (envs, stmts, expr, i) :: cont =>
            let val i = i + 1
            in if i < Vector.length stmts
               then let val cont = Stmt (envs, stmts, expr, i) :: cont
                    in exec dump cont envs (Vector.sub (stmts, i))
                    end
               else eval dump cont envs expr
            end
         | Match (envs, envDeltas, pats, body, i, seq) :: cont =>
            let val i = i + 1
            in if i < Vector.length pats
               then let val SOME (Value.Slice (vs, _)) = force seq
                        val seq = wrap (Value.Slice (vs, i))
                        val cont = Match (envs, envDeltas, pats, body, i, seq) :: cont
                    in match dump cont envs envDeltas (Vector.sub (pats, i)) seq
                    end
               else ( define envs (!envDeltas)
                    ; eval dump cont envs body )
            end
         | Init (envs, var) :: cont =>
            ( Value.initialize (lookup dump envs var) value
            ; continue value dump cont)
         | [] =>
           (case Dump.pop dump
            of SOME (cont', dump') => continue value dump' cont'
             | NONE => value)

    and apply dump cont denv callee args =
        case force callee
        of SOME (Value.Closure (methods, lenv)) =>
            let val Value.Method (pats, _, body) = Vector.sub (methods, 0)
                val i = 0
            in if i < Vector.length pats
               then let val envs = declareParams { lex = lenv, dyn = denv } pats
                        val envDeltas = ref { lex = Env.empty, dyn = Env.empty }
                        val argSlice = wrap (Value.Slice (args, 0))
                        val cont = Match (envs, envDeltas, pats, body, i, argSlice) :: cont
                    in match dump cont envs envDeltas (Vector.sub (pats, i)) argSlice
                    end
               else raise Fail "unimplemented"
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

    and match dump cont envs envDeltas pattern argSeq =
        case pattern
        of Value.Var (_, var) =>
            let val SOME (Value.Slice (vs, i)) = force argSeq
                val SOME (Value.Tuple vs) = force vs
            in envDeltas := insert (!envDeltas) var (Vector.sub (vs, i))
             ; continue argSeq dump cont
            end

    fun interpret expr = eval Dump.empty [] { lex = Env.empty, dyn = Env.empty } expr
end
