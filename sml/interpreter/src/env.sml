structure Env :> sig
    type 'a t

    val empty : 'a t

    val insert : 'a t -> string -> 'a -> 'a t
    val declare : 'a t -> string vector -> (unit -> 'a) -> 'a t
    val define : 'a t -> 'a t -> ('a -> 'a -> unit) -> unit

    val find : 'a t -> string -> 'a option
    val lookup : 'a t -> string -> 'a
end = struct
    structure StringMap = BinaryMapFn(type ord_key = string
                                      val compare = String.compare)

    type 'a t = 'a StringMap.map

    val empty = StringMap.empty

    fun insert env name value = StringMap.insert (env, name, value)

    fun declare env names create =
        Vector.foldl (fn (name, env) => StringMap.insert (env, name, create ())) env names

    fun define env delta init =
        StringMap.appi (fn (k, v) => init (StringMap.lookup (env, k)) v) delta

    fun find env name = StringMap.find (env, name)

    fun lookup env name = StringMap.lookup (env, name)
end
