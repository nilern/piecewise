structure OptionExt :> sig
    val or : 'a option -> 'a option -> 'a option
    val orElse : 'a option -> (unit -> 'a option) -> 'a option

    val mapOr : 'b -> ('a -> 'b) -> 'a option -> 'b
    val mapOrElse : (unit -> 'b) -> ('a -> 'b) -> 'a option -> 'b

    val toList : 'a option -> 'a list
    val toVector : 'a option -> 'a vector
    val toDoc : ('a -> PPrint.doc) -> 'a option -> PPrint.doc
end = struct
    fun or (v as SOME _) _ = v
      | or NONE v = v

    fun orElse (v as SOME _) _ = v
      | orElse NONE thunk = thunk ()

    fun mapOr _ f (SOME v) = f v
      | mapOr default _ NONE = default

    fun mapOrElse _ f (SOME v) = f v
      | mapOrElse thunk _ NONE = thunk ()

    fun toList (SOME v) = [v]
      | toList NONE = []

    fun toVector ov = Vector.fromList (toList ov)

    fun toDoc f = mapOr PPrint.empty f
end
