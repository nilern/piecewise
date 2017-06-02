structure Name = struct
    datatype t = Plain of string
               | Unique of string * int

    fun toString (Plain cs) = cs
      | toString (Unique (cs, i)) = cs ^ Int.toString i
end
