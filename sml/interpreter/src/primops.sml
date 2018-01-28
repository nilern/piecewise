structure Primops :> sig
    val applyPure : string -> Value.value vector -> Value.value

    val unApply : string -> Value.value -> int -> (Value.value * Value.value) option
end = struct
    val force = Value.force
    val wrap = Value.wrap

    fun applyPure opcode args =
        case opcode
        of "iAdd" =>
            case Vector.map force args
            of #[SOME (Value.Int a), SOME (Value.Int b)] => Value.wrap (Value.Int (a + b))
             | #[SOME _, SOME _] => raise Fail "__iAdd: arg types"
             | #[_, _] => raise Fail "__iAdd: uninitialized"
             | _ => raise Fail "__iAdd: argc"

    fun unApply opcode argSeq patternCount =
        case opcode
        of "rest" =>
           if patternCount = 1
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
end
