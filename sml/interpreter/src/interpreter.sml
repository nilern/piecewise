structure Interpreter : sig
    val interpret : Value.expr -> Value.value
end = struct
    structure Env : sig
        type t

        val empty : t
    end = struct
        type t = (string * Value.value) list

        val empty = []
    end

    datatype continuation = Halt

    fun eval cont env =
        fn Value.Triv (_, Value.Const v) => continue v cont

    and continue value =
        fn Halt => value

    fun interpret expr = eval Halt Env.empty expr
end
