signature VECTOR_EXT = sig
    type 'a builder = { done : unit -> 'a vector
                      , sub : int -> 'a
                      , update : int * 'a -> unit }
    val builder : int -> 'a builder

    val toList : 'a vector -> 'a list

    val fromListRev : 'a list -> 'a vector

    val uncons : 'a vector -> ('a * 'a VectorSlice.slice) option

    val concat : 'a vector -> 'a vector -> 'a vector

    val flatMap : ('a -> 'b vector) -> 'a vector -> 'b vector
    val filter : ('a -> bool) -> 'a vector -> 'a vector
end

structure VectorExt :> VECTOR_EXT = struct
    type 'a builder = { done : unit -> 'a vector
                      , sub : int -> 'a
                      , update : int * 'a -> unit }
    val builder = MLton.Vector.create

    fun toList vec = Vector.foldr op:: [] vec

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

    fun uncons vec =
        if Vector.length vec > 0
        then SOME (Vector.sub (vec, 0), VectorSlice.slice (vec, 1, NONE))
        else NONE

    fun concat u v = Vector.concat [u, v]

    fun flatMap f = Vector.foldl (fn (v, acc) => concat acc (f v)) (Vector.fromList [])
    fun filter pred = Vector.fromList o List.filter pred o toList
end
