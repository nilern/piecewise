(*structure FlatTag0 = struct
    datatype t = Lex | Dyn | UpperDyn | Label

    val fromAATag = fn AATag.Lex => SOME Lex
                    | AATag.UpperLex => NONE
                    | AATag.Dyn => SOME Dyn
                    | AATag.UpperDyn => SOME UpperDyn
    val upper = fn Dyn => SOME UpperDyn
                 | _ => NONE
    val compare = fn (Lex, Lex) => EQUAL
                   | (Lex, _) => LESS
                   | (Dyn, Lex) => GREATER
                   | (Dyn, Dyn) => EQUAL
                   | (Dyn, _) => LESS
                   | (UpperDyn, UpperDyn) => EQUAL
                   | (UpperDyn, Label) => LESS
                   | (UpperDyn, _) => GREATER
                   | (Label, Label) => EQUAL
                   | (Label, _) => GREATER
    val toDoc = fn Lex => PPrint.empty
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
end*)

structure BaseVar = struct
    datatype t = Lex of Name.t
               | Dyn of Name.t

    val fresh = fn Lex name => Lex (Name.fresh name)
                 | Dyn name => Dyn (Name.fresh name)

    val compare = fn (Lex n1, Lex n2) => Name.compare (n1, n2)
                   | (Lex _, Dyn _) => LESS
                   | (Dyn _, Lex _) => GREATER
                   | (Dyn n1, Dyn n2) => Name.compare (n1, n2)

    val toDoc = fn Lex name => Name.toDoc name
                 | Dyn name => PPrint.^^ (PPrint.text "$", Name.toDoc name)
end

structure LVar = struct
    datatype t = Def of BaseVar.t
               | Aug of BaseVar.t

    val toDoc = fn Def v => BaseVar.toDoc v
                 | Aug v => PPrint.^^ (BaseVar.toDoc v, PPrint.text "+")
end

structure RVar = struct
    datatype t = Current of BaseVar.t
               | Upper of BaseVar.t

    val fromBaseVar = Current
    val upper = Upper

    val compare = fn (Current v1, Current v2) => BaseVar.compare (v1, v2)
                   | (Current _, Upper _) => LESS
                   | (Upper _, Current _) => GREATER
                   | (Upper v1, Upper v2) => BaseVar.compare (v1, v2)

    val toDoc = fn Current v => BaseVar.toDoc v
                 | Upper v => PPrint.^^ (PPrint.text "^", (BaseVar.toDoc v))
end

(*structure FlatVar0 = struct
    structure FV0 = VarFn(FlatTag0)
    open FV0
    fun fromAVar (tag, name) = Option.map (fn tag' => (tag', name)) (FlatTag0.fromAATag tag)
end
structure FlatVar1 = VarFn(FlatTag1)

structure Label :> sig
    eqtype t
    type ord_key = t

    val fresh : unit -> t
    val compare : t * t -> order
    val toDoc : t -> PPrint.doc
end = struct
    type t = int
    type ord_key = t

    local val counter = ref 0
    in
        fun fresh () = let val res = !counter
                           val _ = counter := res + 1
                       in res
                       end
    end

    val compare = Int.compare
    val toDoc = PPrint.int
end

structure LabelMap = BinaryMapFn(Label)

structure ContRef0 = struct
    structure PP = PPrint
    val op^^ = PP.^^

    datatype t = Label of int
               | NextAtom of int
               | Halt

    local val counter = ref 0
    in
        fun fresh () = let val res = !counter
                           val _ = counter := res + 1
                       in res
                       end
    end

    val rec toDoc =
        fn Label i => PP.text "k" ^^ PP.brackets (PP.text (Int.toString i))
         | NextAtom i => toDoc (Label i) ^^ PP.text ">"
         | Halt => PP.text "__halt"
end*)
