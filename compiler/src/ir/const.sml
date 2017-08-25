structure Const = struct
    structure PP = PPrint
    val op^^ = PP.^^

    datatype t = Int of LargeInt.int
               | String of string
               | Char of string
               | Symbol of string

    fun toDoc (Int i) = PP.text (LargeInt.toString i)
      | toDoc (String s) = PP.text s
      | toDoc (Char s) = PP.text s
      | toDoc (Symbol s) = PP.text ":" ^^ PP.text s
end
