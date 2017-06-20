structure Primop = struct
    structure PP = PPrint
    datatype t = IAdd

    fun fromString "iadd" = IAdd
    fun toString IAdd = "__iadd"
    fun toDoc IAdd = PP.text "__iadd"
end
