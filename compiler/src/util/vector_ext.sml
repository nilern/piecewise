signature VECTOR_EXT = sig
    structure Builder : sig
        type 'a t

        val empty : unit -> 'a t
        val append : 'a t -> 'a -> unit
        val clear : 'a t -> unit
        val build : 'a t -> 'a vector
    end

    val toList : 'a vector -> 'a list

    val empty : unit -> 'a vector
    val singleton : 'a -> 'a vector

    val conj : 'a vector -> 'a -> 'a vector
    val prepend : 'a vector -> 'a -> 'a vector

    val concat : 'a vector -> 'a vector -> 'a vector
    val flatten : 'a vector vector -> 'a vector

    val flatMap : ('a -> 'b vector) -> 'a vector -> 'b vector
    val filter : ('a -> bool) -> 'a vector -> 'a vector
    val remove : ('a -> bool) -> 'a vector -> 'a vector
end

structure VectorExt :> VECTOR_EXT = struct
    structure Builder = struct
        type 'a t = 'a list ref

        fun empty () = ref []

        fun append builder v = builder := v :: !builder

        fun clear builder = builder := []

        fun build builder = Vector.fromList (List.rev (!builder))
    end

    fun toList vec = Vector.foldr op:: [] vec

    fun empty () = Vector.fromList []
    fun singleton v = Vector.fromList [v]

    fun conj vs v =
        let val len = Vector.length vs
        in
            Vector.tabulate (len + 1,
                             fn i => if i < len then Vector.sub(vs, i) else v)
        end
    fun prepend vs v =
        let val len = Vector.length vs
        in Vector.tabulate (len + 1, fn 0 => v | i => Vector.sub(vs, i - 1))
        end

    fun concat u v = Vector.concat [u, v]
    fun flatten vv = Vector.foldl (fn (v, acc) => concat v acc) (empty ()) vv

    fun flatMap f = Vector.foldl (fn (v, acc) => concat acc (f v)) (empty ())
    fun filter pred = Vector.fromList o List.filter pred o toList
    fun remove pred = filter (not o pred)
end
