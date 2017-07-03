structure Primop = struct
    structure PP = PPrint
    datatype t = IAdd | Close | FnPtr | FnGet | Tag | Repr | AGet

    fun fromString "iadd" = IAdd
      | fromString "close" = Close
      | fromString "fnPtr" = FnPtr
      | fromString "fnGet" = FnGet
      | fromString "tag" = Tag
      | fromString "repr" = Repr
      | fromString "aget" = AGet

    fun toDoc IAdd = PP.text "__iadd"
      | toDoc Close = PP.text "__close"
      | toDoc FnPtr = PP.text "__fnPtr"
      | toDoc FnGet = PP.text "__fnGet"
      | toDoc Tag = PP.text "__tag"
      | toDoc Repr = PP.text "__repr"
      | toDoc AGet = PP.text "__aget"
end
