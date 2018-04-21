signature VECTOR_EXT = sig
    structure Builder : sig
        type 'a t

        val empty : unit -> 'a t
        val append : 'a t -> 'a -> unit
        val clear : 'a t -> unit
        val build : 'a t -> 'a vector
    end

    type 'a builder = { done : unit -> 'a vector
                      , sub : int -> 'a
                      , update : int * 'a -> unit }
    val builder : int -> 'a builder

    val toList : 'a vector -> 'a list

    val empty : unit -> 'a vector
    val singleton : 'a -> 'a vector
    val fromListRev : 'a list -> 'a vector

    val conj : 'a vector -> 'a -> 'a vector
    val prepend : 'a vector -> 'a -> 'a vector

    val uncons : 'a vector -> ('a * 'a VectorSlice.slice) option

    val concat : 'a vector -> 'a vector -> 'a vector
    val flatten : 'a vector vector -> 'a vector

    val app2 : ('a * 'b -> unit) -> 'a vector -> 'b vector -> unit
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

    type 'a builder = { done : unit -> 'a vector
                      , sub : int -> 'a
                      , update : int * 'a -> unit }
    val builder = MLton.Vector.create

    fun toList vec = Vector.foldr op:: [] vec

    fun empty () = Vector.fromList []
    fun singleton v = Vector.fromList [v]

    fun fromListRev vs =
        let fun loop (v :: vs) index =
                let val builder = loop vs (index + 1)
                in #update builder (#lastIndex builder - index, v)
                 ; builder
                end
              | loop [] index =
                let val {done = done, update = update, sub = _} = MLton.Vector.create index
                in { build = done, update = update, lastIndex = index - 1 }
                end
        in #build (loop vs 0) ()
        end

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

    fun uncons vec =
        if Vector.length vec > 0
        then SOME (Vector.sub (vec, 0), VectorSlice.slice (vec, 1, NONE))
        else NONE

    fun concat u v = Vector.concat [u, v]
    fun flatten vv = Vector.foldl (fn (v, acc) => concat v acc) (empty ()) vv

    fun app2 f vec1 vec2 = Vector.appi (fn (i, v) => f (v, Vector.sub (vec2, i))) vec1
    fun flatMap f = Vector.foldl (fn (v, acc) => concat acc (f v)) (empty ())
    fun filter pred = Vector.fromList o List.filter pred o toList
    fun remove pred = filter (not o pred)
end
