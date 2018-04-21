structure VectorSliceExt :> sig
    val uncons : 'a VectorSlice.slice -> ('a * 'a VectorSlice.slice) option
end = struct
    fun uncons vs =
        if VectorSlice.length vs > 0
        then SOME (VectorSlice.sub (vs, 0), VectorSlice.subslice (vs, 1, NONE))
        else NONE
end
