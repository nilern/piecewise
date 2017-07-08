structure ListExt :> sig
    val some : ('a -> 'b option) -> 'a list -> 'b option
end = struct
    fun some f (x :: xs) = OptionExt.orElse (f x) (fn () => some f xs)
      | some f [] = NONE
end
