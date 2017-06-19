structure Const = struct
    datatype t = Int of string
               | String of string
               | Char of string

    fun toString (Int s) = s
      | toString (String s) = s
      | toString (Char s) = s
end
