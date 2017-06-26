signature VAR = sig
    structure Name : NAME
    datatype t = Lex of Name.t
               | Dyn of Name.t

    val toDoc : t -> PPrint.doc
end

functor Var(Name : NAME) : VAR = struct
    structure PP = PPrint
    val op^^ = PPrint.^^
    structure Name = Name

    datatype t = Lex of Name.t
               | Dyn of Name.t

    fun toDoc (Lex name) = Name.toDoc name
      | toDoc (Dyn name) = PP.text "$" ^^ Name.toDoc name
end
