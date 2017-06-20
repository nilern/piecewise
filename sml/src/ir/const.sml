structure Const = struct
    structure PP = PPrint

    datatype t = Int of string
               | String of string
               | Char of string

    fun toString (Int s) = s
      | toString (String s) = s
      | toString (Char s) = s

    fun toDoc (Int s) = PP.text s
      | toDoc (String s) = PP.text s
      | toDoc (Char s) = PP.text s
end
