structure Ast : sig
    datatype expr = Id of string

    val toDoc : expr -> PPrint.doc
end = struct
    structure PP = PPrint

    datatype expr = Id of string

    fun toDoc (Id cs) = PP.text cs
end
