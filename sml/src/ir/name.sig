signature NAME = sig
    eqtype t
    val compare : t * t -> order
    val fromString : string -> t
    val toString : t -> string
    val toDoc : t -> PPrint.doc
end
