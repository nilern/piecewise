signature VECTOR_EXT = sig
    val empty : unit -> 'a vector
    val singleton : 'a -> 'a vector
    val conj : 'a vector -> 'a -> 'a vector
    val prepend : 'a vector -> 'a -> 'a vector

    val flatMap : ('a -> 'b vector) -> 'a vector -> 'b vector
end

structure VectorExt :> VECTOR_EXT = struct
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
        in
            Vector.tabulate (len + 1, fn 0 => v | i => Vector.sub(vs, i - 1))
        end

    fun flatMap f vs =
        let fun step (v, acc) = Vector.concat [acc, f v]
        in Vector.foldl step (Vector.fromList []) vs end
end
