structure OptionExt :> sig
    val toList : 'a option -> 'a list
end = struct
    fun toList (SOME v) = [v]
      | toList NONE = []
end
