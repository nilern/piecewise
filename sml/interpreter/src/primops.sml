structure Primops :> sig
    (* HACK: Using the generic `Fail` exception. *)

    (* Apply primitive operation to arguments.
       Raise `Uninitialized` if an the value of an uninitialized argument would be required. *)
    val applyPure : string -> Value.value vector -> Value.value

    (* Match primitive pattern to a sequence and produce a new sequence of the given length.
       Return the new sequence and the remainder of the matched sequence if successful.
       Return none if the matched sequence was too short.
       Raise `Uninitialized` if an the value of an uninitialized part of the sequence
       would be required. *)
    val unApply : string -> Value.value -> int -> (Value.value * Value.value) option

    val slicePopFront : Value.value -> (Value.value * Value.value) option
end = struct
    val forceExn = Value.forceExn
    val wrap = Value.wrap

    fun applyPure opcode args =
        case opcode
        of "iAdd" =>
            case Vector.map forceExn args
            of #[Value.Int a, Value.Int b] => Value.wrap (Value.Int (a + b))
             | #[_, _] => raise Fail "__iAdd: arg types"
             | _ => raise Fail "__iAdd: argc"

    fun unApply opcode argSeq patternCount =
        case opcode
        of "rest" =>
           if patternCount = 1
           then case forceExn argSeq
                of Value.Slice (args, i) =>
                    (case forceExn args
                     of Value.Tuple argv =>
                         let val innerArgs = wrap (Value.Tuple (Vector.fromList [argSeq]))
                         in SOME (wrap (Value.Slice (innerArgs, 0)),
                                  wrap (Value.Slice (args, Vector.length argv)))
                         end
                      | _ => raise Fail "__rest: arg types")
                 | _ => raise Fail "__rest: arg types"
           else raise Fail "__rest: argc"

    fun slicePopFront value =
        case forceExn value
        of Value.Slice (base, i) =>
            (case forceExn base
             of Value.Tuple vs =>
                 if i < Vector.length vs
                 then SOME (Vector.sub (vs, i), wrap (Value.Slice (base, i + 1)))
                 else NONE
              | _ => raise Fail "slicePopFront: arg types")
         | _ => raise Fail "slicePopFront: arg types"
end
