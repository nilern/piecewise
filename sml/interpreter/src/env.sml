structure Env :> sig
    type 'a t

    val empty : 'a t
    val declare : 'a t -> string vector -> (unit -> 'a) -> 'a t

    val find : 'a t -> string -> 'a option
    val lookup : 'a t -> string -> 'a

    type 'a delta

    val emptyDelta : unit -> 'a delta
    val deltaInsert : 'a delta -> string -> 'a -> unit

    val applyDelta : 'a t -> 'a delta -> ('a -> 'a -> unit) -> unit
end = struct
    structure StringMap = BinaryMapFn(type ord_key = string
                                      val compare = String.compare)

    type 'a t = 'a StringMap.map

    val empty = StringMap.empty

    fun declare env names create =
        Vector.foldl (fn (name, env) => StringMap.insert (env, name, create ())) env names

    fun find env name = StringMap.find (env, name)

    fun lookup env name = StringMap.lookup (env, name)

    structure StringTable = HashTableFn(type hash_key = string
                                        val hashVal = HashString.hashString
                                        val sameKey = op=)

    type 'a delta = 'a StringTable.hash_table

    fun emptyDelta () = StringTable.mkTable (0, Fail "unreachable")

    fun deltaInsert delta name value = StringTable.insert delta (name, value)

    fun applyDelta env delta init =
      StringTable.appi (fn (k, v) => init (StringMap.lookup (env, k)) v) delta
end
