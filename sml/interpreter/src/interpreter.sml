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
        fn Value.Def (pat, guard, expr) =>
            let val cont = Def (envs, pat, guard) :: cont
            in eval dump cont envs expr
            end
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

    and primApply dump cont denv opcode argv =
        case opcode
        of "iAdd" =>
            if Vector.length argv = 2
            then case (force (Vector.sub (argv, 0)), force (Vector.sub (argv, 1)))
                 of (SOME (Value.Int a), SOME (Value.Int b)) =>
                     continue (Value.wrap (Value.Int (a + b))) dump cont
                  | _ => raise Fail "__iAdd: arg types"
            else raise Fail "__iAdd: argc"

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
            let val SOME (innerArgSeq, outerArgSeq) = primUnApply opcode params argSeq
                val cont = OuterMatch outerArgSeq :: cont
            in match dump cont envs envDeltas (Vector.sub (params, 0)) innerArgSeq
            end

    and primUnApply opcode params argSeq =
        case opcode
        of "rest" =>
           if Vector.length params = 1
           then case force argSeq
                of SOME (Value.Slice (args, i)) =>
                    (case force args
                     of SOME (Value.Tuple argv) =>
                         let val innerArgs = wrap (Value.Tuple (Vector.fromList [argSeq]))
                         in SOME (wrap (Value.Slice (innerArgs, 0)),
                                  wrap (Value.Slice (args, Vector.length argv)))
                         end
                      | SOME _ => raise Fail "__rest: arg types"
                      | NONE => raise Fail "__rest: uninitialized")
                 | SOME _ => raise Fail "__rest: arg types"
                 | NONE => raise Fail "__rest: uninitialized"
           else raise Fail "__rest: argc"

    fun interpret expr = eval Dump.empty [] { lex = Env.empty, dyn = Env.empty } expr
end
