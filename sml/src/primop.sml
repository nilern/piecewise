structure Primop = struct
    structure PP = PPrint
    datatype t = IAdd | Close | FnPtr | FnGet

    fun fromString "iadd" = IAdd
      | fromString "close" = Close
      | fromString "fnPtr" = FnPtr
      | fromString "fnGet" = FnGet

    fun toDoc IAdd = PP.text "__iadd"
      | toDoc Close = PP.text "__close"
      | toDoc FnPtr = PP.text "__fnPtr"
      | toDoc FnGet = PP.text "__fnGet"
end
