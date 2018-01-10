structure Value :> sig
    type value
    datatype content = Int of int
                     | Float of real
                     | Char of char
                     | Bool of bool
                     | String of string

    datatype expr = Block of Pos.t * stmt vector * expr
                  | Triv of Pos.t * triv
    and stmt = Def of expr * expr option * expr
             | Expr of expr
    and triv = Lex of string
             | Dyn of string
             | Const of value

    val wrap : content -> value
    val uninitialized : unit -> value
    val initialize : value -> value -> unit
    val force : value -> content option

    val lexName : triv -> string option
    val dynName : triv -> string option

    val patBinders : (triv -> string option) -> expr -> string vector
    val stmtBinders : (triv -> string option) -> stmt -> string vector
    val blockBinders : (triv -> string option) -> stmt vector -> string vector

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val trivToDoc : triv -> PPrint.doc
    val valueToDoc : value -> PPrint.doc
    val contentToDoc : content -> PPrint.doc

end = struct
    structure PP = PPrint
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>
    val op<$> = PPrint.<$>

    datatype expr = Block of Pos.t * stmt vector * expr
                  | Triv of Pos.t * triv
    and stmt = Def of expr * expr option * expr
             | Expr of expr
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

    fun uninitialized () = Value (ref Uninitialized)

    (* HACK: error handling *)
    fun initialize (Value cell) value =
        case !cell
        of Uninitialized => cell := Redirect value
         | _ => raise Fail "Reinit"


    (* TODO: Cycle detection to avoid infinite loops? *)
    fun force (Value v) =
        case !v
        of Present c => SOME c
         | Redirect v' => force v'
         | Uninitialized => NONE

    val lexName =
        fn Lex name => SOME name
         | _ => NONE

    val dynName =
        fn Dyn name => SOME name
         | _ => NONE

    fun patBinders f =
        fn Block (_, _, _) => Vector.fromList [] (* actually, illegal pattern *)
         | Triv (_, t) => OptionExt.toVector (f t)

    fun stmtBinders f =
        fn Def (pat, _, _) => patBinders f pat
         | Expr _ => Vector.fromList []

    fun blockBinders f stmts = VectorExt.flatMap (stmtBinders f) stmts

    val exprPos =
        fn Block (pos, _, _) => pos
         | Triv (pos, _) => pos

    val stmtPos =
        fn Def (pat, _, _) => exprPos pat
         | Expr expr => exprPos expr

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

    val rec exprToDoc =
        fn Block (_, stmts, expr) =>
            PP.punctuate (PP.semi ^^ PP.line) (Vector.map stmtToDoc stmts) ^^
                PP.semi <$> exprToDoc expr
         | Triv (_, t) => trivToDoc t
    and stmtToDoc =
        fn Def (pat, SOME guard, expr) =>
            exprToDoc pat <+> PP.text "|" <+> exprToDoc guard <+> PP.text "=" <+> exprToDoc expr
         | Def (pat, NONE, expr) =>
            exprToDoc pat <+> PP.text "=" <+> exprToDoc expr
         | Expr expr => exprToDoc expr
end
