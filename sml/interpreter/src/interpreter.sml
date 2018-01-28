structure Interpreter :> sig
    val interpret : Value.expr -> Value.value
end = struct
    structure Prim = Primops

    type value = Value.value
    type expr = Value.expr
    type stmt = Value.stmt

    val wrap = Value.wrap
    val force = Value.force

    type envs = { lex: value Env.t, dyn: value Env.t }

    datatype callable = Value of value
                      | Opcode of string

    datatype frame = Callee of envs * expr vector
                   | Arg of envs * expr VectorSlice.slice * callable * value VectorExt.builder
                   | Stmt of envs * stmt vector * expr * int
                   | Def of envs * expr * expr option
                   | Match of envs * (* HACK: *) envs ref * expr vector * int
                   | OuterMatch of value
                   | Commit of envs * (* HACK: *) envs ref * expr

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
            (case VectorExt.uncons args
             of SOME (argExpr, remArgs) =>
                 let val argsBuilder = VectorExt.builder (Vector.length args)
                     val cont = Arg (envs, remArgs, Opcode opcode, argsBuilder) :: cont
                 in eval dump cont envs argExpr
                 end
              | NONE => primApply dump cont (#dyn envs) opcode #[])
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
        fn Value.Def (pat, guard, expr) =>
            let val cont = Def (envs, pat, guard) :: cont
            in eval dump cont envs expr
            end
         | Value.Expr expr => eval dump cont envs expr

    and continue value dump =
        fn Callee (envs, argExprs) :: cont =>
            (case VectorExt.uncons argExprs
             of SOME (argExpr, remArgExprs) =>
                 let val argsBuilder = VectorExt.builder (Vector.length argExprs)
                     val cont = Arg (envs, remArgExprs, Value value, argsBuilder) :: cont
                 in eval dump cont envs argExpr
                 end
              | NONE => apply dump cont (#dyn envs) value (wrap (Value.Tuple #[])))
         | Arg (envs, argExprs, callee, argsBuilder) :: cont =>
            ( #update argsBuilder (#2 (VectorSlice.base argExprs) - 1, value)
            ; case VectorSliceExt.uncons argExprs
              of SOME (argExpr, remArgExprs) =>
                  let val cont = Arg (envs, remArgExprs, callee, argsBuilder) :: cont
                  in eval dump cont envs argExpr
                  end
               | NONE =>
                  let val argv = #done argsBuilder ()
                  in case callee
                     of Value f => apply dump cont (#dyn envs) f (wrap (Value.Tuple argv))
                      | Opcode opcode => primApply dump cont (#dyn envs) opcode argv
                  end)
         | Stmt (envs, stmts, expr, i) :: cont =>
            let val i = i + 1
            in if i < Vector.length stmts
               then let val cont = Stmt (envs, stmts, expr, i) :: cont
                    in exec dump cont envs (Vector.sub (stmts, i))
                    end
               else eval dump cont envs expr
            end
         | Def (envs, pat, (* TODO: *) guard) :: cont =>
            let val i = 0
                val envDeltas = ref { lex = Env.empty, dyn = Env.empty }
                (* HACK: Look at `cont` and create a 'sub-block' as the `body`: *)
                val Stmt (_, stmts, expr, stmtIndex) :: _ = cont
                val stmts = VectorSlice.vector (VectorSlice.slice (stmts, stmtIndex + 1, NONE))
                val body = Value.Block (Pos.def, stmts, expr)
                val tup = wrap (Value.Tuple (Vector.fromList [value]))
                val seq = wrap (Value.Slice (tup, i))
                val cont = Commit (envs, envDeltas, body) :: cont
            in match dump cont envs envDeltas pat seq
            end
         | Match (envs, envDeltas, pats, i) :: cont =>
            let val i = i + 1
            in if i < Vector.length pats
               then let val cont = Match (envs, envDeltas, pats, i) :: cont
                    in match dump cont envs envDeltas (Vector.sub (pats, i)) value
                    end
               else continue value dump cont
            end
         | OuterMatch argSeq :: cont => continue argSeq dump cont
         | Commit (envs, envDeltas, body) :: cont =>
           (* TODO: Check that value is an empty seq *)
           ( define envs (!envDeltas)
           ; eval dump cont envs body )
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
                        val argSlice = wrap (Value.Slice (args, i))
                        val cont = Match (envs, envDeltas, pats, i)
                                   :: Commit (envs, envDeltas, body)
                                   :: cont
                    in match dump cont envs envDeltas (Vector.sub (pats, i)) argSlice
                    end
               else raise Fail "unimplemented"
            end

    and primApply dump cont denv opcode argv = continue (Prim.applyPure opcode argv) dump cont

    and match dump cont envs envDeltas pattern argSeq =
        case pattern
        of Value.Var (_, var) =>
            let val SOME (Value.Slice (args, i)) = force argSeq
                val SOME (Value.Tuple argv) = force args
                val arg = Vector.sub (argv, i) (* TODO: Check that i < vvs.length *)
                val argSeq = wrap (Value.Slice (args, i + 1))
            in envDeltas := insert (!envDeltas) var arg
             ; continue argSeq dump cont
            end
         | Value.PrimCall (_, opcode, params) =>
            let val paramCount = Vector.length params
                val SOME (innerArgSeq, outerArgSeq) = Prim.unApply opcode argSeq paramCount
                val cont = OuterMatch outerArgSeq :: cont
            in match dump cont envs envDeltas (Vector.sub (params, 0)) innerArgSeq
            end

    fun interpret expr = eval Dump.empty [] { lex = Env.empty, dyn = Env.empty } expr
end
