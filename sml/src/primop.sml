structure Primop = struct
    structure PP = PPrint
    datatype t = IAdd | Close

    fun fromString "iadd" = IAdd
      | fromString "close" = Close

    fun toString IAdd = "__iadd"
      | toString Close = "__close"

    fun toDoc IAdd = PP.text "__iadd"
      | toDoc Close = PP.text "__close"
end
