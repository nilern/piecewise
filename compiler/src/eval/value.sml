structure Value :> sig
    datatype t = Int of LargeInt.int
               | Float of LargeReal.real
               | Char of WideChar.char
               | Bool of bool

    val toString : t -> string
    val toDoc : t -> PPrint.doc
end = struct
    datatype t = Int of LargeInt.int
               | Float of LargeReal.real
               | Char of WideChar.char
               | Bool of bool

    val toString =
        fn Int i => LargeInt.toString i
         | Float f => LargeReal.toString f
         | Char c => WideChar.toString c
         | Bool true => "True"
         | Bool false => "False"

    val toDoc = PPrint.text o toString
end
