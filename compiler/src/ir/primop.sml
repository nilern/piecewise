structure Primop = struct
    structure PP = PPrint
    datatype t = IAdd
               | Tuple
               | Close | FnPtr | FnGet | Call
               | Tag | Repr
               | AGet
               | Box | BSet
               | DEnv | EmptyDEnv | DGet
               | Panic

    fun fromString "iadd" = IAdd
      | fromString "tuple" = Tuple
      | fromString "close" = Close
      | fromString "fnPtr" = FnPtr
      | fromString "fnGet" = FnGet
      | fromString "call" = Call
      | fromString "tag" = Tag
      | fromString "repr" = Repr
      | fromString "aget" = AGet
      | fromString "box" = Box
      | fromString "bset" = BSet
      | fromString "denv" = DEnv
      | fromString "emptyDenv" = EmptyDEnv
      | fromString "dget" = DGet
      | fromString "panic" = Panic

    fun toDoc IAdd = PP.text "__iadd"
      | toDoc Tuple = PP.text "__tuple"
      | toDoc Close = PP.text "__close"
      | toDoc FnPtr = PP.text "__fnPtr"
      | toDoc FnGet = PP.text "__fnGet"
      | toDoc Call = PP.text "__call"
      | toDoc Tag = PP.text "__tag"
      | toDoc Repr = PP.text "__repr"
      | toDoc AGet = PP.text "__aget"
      | toDoc Box = PP.text "__box"
      | toDoc BSet = PP.text "__bset"
      | toDoc DEnv = PP.text "__denv"
      | toDoc EmptyDEnv = PP.text "__emptyDenv"
      | toDoc DGet = PP.text "__dget"
      | toDoc Panic = PP.text "__panic"
end
