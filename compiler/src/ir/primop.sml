structure Primop = struct
    datatype t = Apply
               | IAdd
               | Tuple
               | Close | FnPtr | FnGet
               | Tag | Repr
               | AGet
               | Box | BSet
               | DEnv | EmptyDEnv | DGet
               | Panic

    fun fromString "apply" = Apply
      | fromString "iadd" = IAdd
      | fromString "tuple" = Tuple
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
      | fromString "panic" = Panic

    fun toString Apply = "__apply"
      | toString IAdd = "__iadd"
      | toString Tuple = "__tuple"
      | toString Close = "__close"
      | toString FnPtr = "__fnPtr"
      | toString FnGet = "__fnGet"
      | toString Tag = "__tag"
      | toString Repr = "__repr"
      | toString AGet = "__aget"
      | toString Box = "__box"
      | toString BSet = "__bset"
      | toString DEnv = "__denv"
      | toString EmptyDEnv = "__emptyDenv"
      | toString DGet = "__dget"
      | toString Panic = "__panic"

    val toDoc = PPrint.text o toString
end
