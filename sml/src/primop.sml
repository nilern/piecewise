structure Primop = struct
    datatype t = IAdd

    fun fromString "iadd" = IAdd
    fun toString IAdd = "__iadd"
end
