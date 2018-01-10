structure Value : sig
    type value
    datatype content = Int of int
                     | Float of real
                     | Char of char
                     | Bool of bool
                     | String of string

    datatype expr = Triv of Pos.t * triv
    and triv = Lex of string
             | Dyn of string
             | Const of value

    val wrap : content -> value
    val force : value -> content option

    val toDoc : expr -> PPrint.doc
    val trivToDoc : triv -> PPrint.doc
    val valueToDoc : value -> PPrint.doc
    val contentToDoc : content -> PPrint.doc

end = struct
    structure PP = PPrint
    val op^^ = PPrint.^^

    datatype expr = Triv of Pos.t * triv
    and triv = Lex of string
             | Dyn of string
             | Const of value
    and value = Value of value_state ref
    and value_state = Present of content
                    | Redirect of value
                    | Uninitialized
    and content = Int of int
                | Float of real
                | Char of char
                | Bool of bool
                | String of string

    val wrap = Value o ref o Present

    (* TODO: Cycle detection to avoid infinite loops? *)
    fun force (Value v) =
        case !v
        of Present c => SOME c
         | Redirect v' => force v'
         | Uninitialized => NONE

    val contentToDoc =
        fn Int n      => PP.int n
         | Float n    => PP.real n
         | Char c     => PP.text "'" ^^ PP.char c ^^ PP.text "'"
         | Bool true  => PP.text "True"
         | Bool false => PP.text "False"
         | String cs  => PP.text ("\"" ^ cs ^ "\"")

    fun valueToDoc v =
        case force v
        of SOME c => contentToDoc c
         | NONE => PP.text "#<unbound>"

    val trivToDoc =
        fn Lex cs  => PP.text cs
         | Dyn cs  => PP.text ("$" ^ cs)
         | Const v => valueToDoc v

    fun toDoc (Triv (_, t)) = trivToDoc t

end
