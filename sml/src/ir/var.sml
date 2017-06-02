structure Var = struct
    datatype t = Lex of Name.t
               | Dyn of Name.t

    fun toString (Lex name) = Name.toString name
      | toString (Dyn name) = "$" ^ Name.toString name
end
