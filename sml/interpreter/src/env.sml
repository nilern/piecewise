structure Env :> sig
    type 'a t

    val empty : 'a t
    val pushBlock : 'a t -> string vector -> (unit -> 'a) -> 'a t
    val lookup : 'a t -> string -> 'a
end = struct
    structure StringMap = BinaryMapFn(type ord_key = string
                                      val compare = String.compare)

    type 'a t = 'a StringMap.map

    val empty = StringMap.empty

    fun pushBlock env names create =
        Vector.foldl (fn (name, env) => StringMap.insert (env, name, create ()))
                     env names

    fun lookup env name = StringMap.lookup (env, name)
end
