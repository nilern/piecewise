structure Var = struct
    structure PP = PPrint
    val op^^ = PPrint.^^

    datatype t = Lex of Name.t
               | Dyn of Name.t

    fun toDoc (Lex name) = Name.toDoc name
      | toDoc (Dyn name) = PP.text "$" ^^ Name.toDoc name
end
