structure Primop = struct
    structure PP = PPrint
    datatype t = IAdd
               | Close | FnPtr | FnGet
               | Tag | Repr
               | AGet
               | Box | BSet
               | DEnv | EmptyDEnv | DGet

    fun fromString "iadd" = IAdd
      | fromString "close" = Close
      | fromString "fnPtr" = FnPtr
      | fromString "fnGet" = FnGet
      | fromString "tag" = Tag
      | fromString "repr" = Repr
      | fromString "aget" = AGet
      | fromString "box" = Box
      | fromString "bset" = BSet
      | fromString "denv" = DEnv
      | fromString "emptyDenv" = EmptyDEnv
      | fromString "dget" = DGet

    fun toDoc IAdd = PP.text "__iadd"
      | toDoc Close = PP.text "__close"
      | toDoc FnPtr = PP.text "__fnPtr"
      | toDoc FnGet = PP.text "__fnGet"
      | toDoc Tag = PP.text "__tag"
      | toDoc Repr = PP.text "__repr"
      | toDoc AGet = PP.text "__aget"
      | toDoc Box = PP.text "__box"
      | toDoc BSet = PP.text "__bset"
      | toDoc DEnv = PP.text "__denv"
      | toDoc EmptyDEnv = PP.text "__emptyDenv"
      | toDoc DGet = PP.text "__dget"
end
