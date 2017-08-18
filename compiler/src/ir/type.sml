structure Type = struct
    datatype t = Any
               | Int | Float | Bool | Char
               | Fn
               | Closure | DynEnv
               | Label of t vector

    local structure PP = PPrint
          val op^^ = PP.^^
    in val rec toDoc = fn Any => PP.text "Any"
                        | Int => PP.text "Int"
                        | Float => PP.text "Float"
                        | Bool => PP.text "Bool"
                        | Char => PP.text "Char"
                        | Fn => PP.text "Fn"
                        | Closure => PP.text "Closure"
                        | DynEnv => PP.text "DynEnv"
                        | Label argTypes =>
                          PP.text "Label" ^^
                              PP.parens (PP.punctuate (PP.text "," ^^ PP.space)
                                                      (Vector.map toDoc argTypes))
    end
end
