structure OptionExt :> sig
    val or : 'a option -> 'a option -> 'a option
    val orElse : 'a option -> (unit -> 'a option) -> 'a option

    val toList : 'a option -> 'a list
end = struct
    fun toList (SOME v) = [v]
      | toList NONE = []

    fun or (v as SOME _) _ = v
      | or NONE v = v

    fun orElse (v as SOME _) _ = v
      | orElse NONE thunk = thunk ()
end
