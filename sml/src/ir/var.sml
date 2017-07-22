(* FIXME: should have Lex|Dyn, then [Upper]Lex|[Upper]Dyn
                    , then Label|Local|[Upper]Dyn
                    , then Label|Local *)

signature VAR = sig
    datatype t = Lex of Name.t
               | UpperLex of Name.t
               | Dyn of Name.t
               | UpperDyn of Name.t

    val upper : t -> t

    val fresh : t -> t

    val compare : t * t -> order

    val toDoc : t -> PPrint.doc
end

structure Var : VAR = struct
    structure PP = PPrint
    val op^^ = PPrint.^^

    datatype t = Lex of Name.t
               | UpperLex of Name.t
               | Dyn of Name.t
               | UpperDyn of Name.t

    fun upper (Lex name) = UpperLex name
      | upper (Dyn name) = UpperDyn name

    fun fresh (Lex name) = Lex (Name.fresh name)
      | fresh (UpperLex name) = UpperLex (Name.fresh name)
      | fresh (Dyn name) = Dyn (Name.fresh name)
      | fresh (UpperDyn name) = UpperDyn (Name.fresh name)

    fun compare (Lex name, Lex name') = Name.compare (name, name')
      | compare (Lex _, _) = LESS
      | compare (UpperLex _, Lex _) = GREATER
      | compare (UpperLex name, UpperLex name') = Name.compare (name, name')
      | compare (UpperLex _, _) = LESS
      | compare (Dyn name, Dyn name') = Name.compare (name, name')
      | compare (Dyn _, UpperDyn _) = LESS
      | compare (Dyn _, _) = GREATER
      | compare (UpperDyn name, UpperDyn name') = Name.compare (name, name')
      | compare (UpperDyn _, _) = GREATER

    fun toDoc (Lex name) = Name.toDoc name
      | toDoc (UpperLex name) = PP.text "^" ^^ Name.toDoc name
      | toDoc (Dyn name) = PP.text "$" ^^ Name.toDoc name
      | toDoc (UpperDyn name) = PP.text "^$" ^^ Name.toDoc name
end

structure LLVar :> sig
    datatype t = Local of Name.t
               | Label of Name.t

    val toDoc : t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PPrint.^^

    datatype t = Local of Name.t
               | Label of Name.t

    fun toDoc (Local name) = Name.toDoc name
      | toDoc (Label name) = PP.text "$" ^^ Name.toDoc name
end
