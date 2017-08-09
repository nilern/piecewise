structure Argv = struct
    type t = { names: Name.t vector, types: Type.t vector }

    val empty: t = { names = Vector.fromList [], types = Vector.fromList [] }
    fun append { names = names, types = types } name t =
        { names = VectorExt.conj names name, types = VectorExt.conj types t }

    local structure PP = PPrint
          val op^^ = PP.^^
          val op<+> = PP.<+>
    in fun toDoc { names = names, types = types } =
           let fun argToDoc (i, arg) =
                   let val t = Vector.sub (types, i)
                   in Name.toDoc arg ^^ PP.text ":" <+> Type.toDoc t
                   end
           in PP.parens (PP.punctuate (PP.text "," ^^ PP.space) (Vector.mapi argToDoc names))
           end
    end
end
