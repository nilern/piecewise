structure Type = struct
    datatype t = Any
               | Int | Float | Bool | Char
               | Fn
               | Cont | DynEnv

    val toDoc = fn Any => PPrint.text "Any"
                 | Int => PPrint.text "Int"
                 | Float => PPrint.text "Float"
                 | Bool => PPrint.text "Bool"
                 | Char => PPrint.text "Char"
                 | Fn => PPrint.text "Fn"
                 | Cont => PPrint.text "Cont"
                 | DynEnv => PPrint.text "DynEnv"
end
