structure Name = struct
    structure PP = PPrint

    datatype t = Plain of string
             | Unique of string * int

    local val counter = ref 0
    in
        fun fresh cs =
            let val i = !counter
            in
                counter := i + 1;
                Unique (StringName.toString cs, i)
            end
    end

    fun chars (Plain cs) = cs
      | chars (Unique (cs, _)) = cs

    fun compare (Plain cs, Plain cs') = String.compare (cs, cs')
      | compare (Unique (cs, i), Unique (cs', i')) =
        (case String.compare (cs, cs')
         of EQUAL => Int.compare (i, i')
          | ord => ord)
      | compare (Unique _, Plain _) = GREATER
      | compare (Plain _, Unique _) = LESS

    fun hash (Plain cs) = HashString.hashString cs
      | hash (Unique (cs, i)) =
        Word.orb(HashString.hashString cs, Word.fromInt i)

    fun fromString cs = Plain cs

    fun toString (Plain cs) = cs
      | toString (Unique (cs, i)) = cs ^ Int.toString i

    fun toDoc (Plain cs) = PP.text cs
      | toDoc (Unique (cs, i)) = PP.text (cs ^ Int.toString i)

    datatype prec = Zero | One | Two | Three | Four | Five | Six | Seven

    exception UnprecedentedOp of string

    fun precOf cs =
        case String.sub(cs, 0)
        of #"|" => Zero
         | #"^" => One
         | #"&" => Two
         | #"=" => Three
         | #"!" => Three
         | #"<" => Four
         | #">" => Four
         | #"+" => Five
         | #"-" => Five
         | #"*" => Six
         | #"/" => Six
         | #"%" => Six
         | #"." => Seven
         | _ => raise UnprecedentedOp cs
end
