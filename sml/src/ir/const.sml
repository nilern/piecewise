structure Const = struct
    structure PP = PPrint
    val op^^ = PP.^^

    datatype t = Int of string (* TODO: IntInf (?) *)
               | String of string
               | Char of string
               | Symbol of string

    fun toDoc (Int s) = PP.text s
      | toDoc (String s) = PP.text s
      | toDoc (Char s) = PP.text s
      | toDoc (Symbol s) = PP.text ":" ^^ PP.text s
end
