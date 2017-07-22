signature VAR_TAG = sig
    type t

    val upper : t -> t option
    val compare : t * t -> order
    val toDoc : t -> PPrint.doc
end

structure CTag = struct
    datatype t = Lex | Dyn

    fun upper _ = NONE
    val compare = fn (Lex, Lex) => EQUAL
                   | (Lex, Dyn) => LESS
                   | (Dyn, Lex) => GREATER
                   | (Dyn, Dyn) => EQUAL
    val toDoc = fn Lex => PPrint.empty
                 | Dyn => PPrint.text "$"
end

structure ATag = struct
    datatype t = Lex | UpperLex | Dyn | UpperDyn

    val fromCTag = fn CTag.Lex => Lex
                    | CTag.Dyn => Dyn
    val upper = fn Lex => SOME UpperLex
                 | Dyn => SOME UpperDyn
                 | _ => NONE
    val compare = fn (Lex, Lex) => EQUAL
                   | (Lex, _) => LESS
                   | (UpperLex, Lex) => GREATER
                   | (UpperLex, UpperLex) => EQUAL
                   | (UpperLex, _) => LESS
                   | (Dyn, Dyn) => EQUAL
                   | (Dyn, UpperDyn) => LESS
                   | (Dyn, _) => GREATER
                   | (UpperDyn, UpperDyn) => EQUAL
                   | (UpperDyn, _) => GREATER
    val toDoc = fn Lex => PPrint.empty
                 | UpperLex => PPrint.text "^"
                 | Dyn => PPrint.text "$"
                 | UpperDyn => PPrint.text "^$"
end

structure FlatTag0 = struct
    datatype t = Local | Dyn | UpperDyn | Label

    val fromATag = fn ATag.Lex => SOME Local
                    | ATag.UpperLex => NONE
                    | ATag.Dyn => SOME Dyn
                    | ATag.UpperDyn => SOME UpperDyn
    val upper = fn Dyn => SOME UpperDyn
                 | _ => NONE
    val compare = fn (Local, Local) => EQUAL
                   | (Local, _) => LESS
                   | (Dyn, Local) => GREATER
                   | (Dyn, Dyn) => EQUAL
                   | (Dyn, _) => LESS
                   | (UpperDyn, UpperDyn) => EQUAL
                   | (UpperDyn, Label) => LESS
                   | (UpperDyn, _) => GREATER
                   | (Label, Label) => EQUAL
                   | (Label, _) => GREATER
    val toDoc = fn Local => PPrint.empty
                 | Dyn => PPrint.text "$"
                 | UpperDyn => PPrint.text "^$"
                 | Label => PPrint.text "$$"
end

structure FlatTag1 = struct
    datatype t = Local | Label

    fun upper _ = NONE
    val compare = fn (Local, Local) => EQUAL
                   | (Local, Label) => LESS
                   | (Label, Label) => EQUAL
                   | (Label, Local) => GREATER
    val toDoc = fn Local => PPrint.empty
                 | Label => PPrint.text "$"
end

signature VAR = sig
    structure Tag : VAR_TAG

    type t = Tag.t * Name.t

    val upper : t -> t option
    val fresh : t -> t
    val compare : t * t -> order
    val toDoc : t -> PPrint.doc
end

functor VarFn(T: VAR_TAG) : VAR = struct
    val op^^ = PPrint.^^

    structure Tag = T

    type t = Tag.t * Name.t

    fun upper (tag, name) = Option.map (fn tag' => (tag', name)) (Tag.upper tag)

    fun fresh (tag, name) = (tag, Name.fresh name)

    fun compare ((tag1, name1), (tag2, name2)) =
        case Tag.compare (tag1, tag2)
        of EQUAL => Name.compare (name1, name2)
         | order => order

    fun toDoc (tag, name) = Tag.toDoc tag ^^ Name.toDoc name
end

structure CVar = VarFn(CTag)
structure AVar = struct
    structure AV = VarFn(ATag)
    open AV
    fun fromCVar (tag, name) = (ATag.fromCTag tag, name)
end
structure FlatVar0 = struct
    structure FV0 = VarFn(FlatTag0)
    open FV0
    fun fromAVar (tag, name) = Option.map (fn tag' => (tag', name)) (FlatTag0.fromATag tag)
end
structure FlatVar1 = VarFn(FlatTag1)

(*signature VAR = sig
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
end*)
