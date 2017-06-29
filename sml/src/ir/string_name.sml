(* TODO: get rid of this, doesn't support gensym *)

structure StringName :> NAME = struct
    type t = string
    val compare = String.compare
    fun fromString s = s
    fun toString s = s
    val toDoc = PPrint.text
end
