structure BaseVar = struct
    datatype t = Lex of Name.t
               | Dyn of Name.t

    val fresh = fn Lex name => Lex (Name.fresh name)
                 | Dyn name => Dyn (Name.fresh name)

    val compare = fn (Lex n1, Lex n2) => Name.compare (n1, n2)
                   | (Lex _, Dyn _) => LESS
                   | (Dyn _, Lex _) => GREATER
                   | (Dyn n1, Dyn n2) => Name.compare (n1, n2)

    val toString = fn Lex name => Name.toString name
                    | Dyn name => "$" ^ Name.toString name

    val toDoc = PPrint.text o toString
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

    val toString = fn Current v => BaseVar.toString v
                    | Upper v => "^" ^ BaseVar.toString v

    val toDoc = PPrint.text o toString
end

functor FlatVarFn(V : TO_DOC) = struct
    structure Var = V

    datatype t = Data of Var.t
               | Label of Name.t

    val toDoc = fn Data v => Var.toDoc v
                 | Label l => PPrint.^^ (PPrint.text "$$", Name.toDoc l)
end

structure FlatVar0 = FlatVarFn(RVar)
structure FlatVar1 = FlatVarFn(Name)
