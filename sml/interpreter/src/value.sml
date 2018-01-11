structure Value :> sig
    type value
    datatype content = Int of int
                     | Float of real
                     | Char of char
                     | Bool of bool
                     | String of string

    datatype expr = Fn of Pos.t * method vector
                  | Block of Pos.t * stmt vector * expr
                  | Var of Pos.t * var
                  | Const of Pos.t * value
    and method = Method of expr * expr option * expr
    and stmt = Def of expr * expr option * expr
             | Expr of expr
    and var = Lex of string
             | Dyn of string

    val wrap : content -> value
    val uninitialized : unit -> value
    val initialize : value -> value -> unit
    val force : value -> content option

    val lexName : var -> string option
    val dynName : var -> string option

    val patBinders : (var -> string option) -> expr -> string vector
    val stmtBinders : (var -> string option) -> stmt -> string vector
    val blockBinders : (var -> string option) -> stmt vector -> string vector

    val exprPos : expr -> Pos.t
    val stmtPos : stmt -> Pos.t

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val methodToDoc : method -> PPrint.doc
    val varToDoc : var -> PPrint.doc
    val valueToDoc : value -> PPrint.doc
    val contentToDoc : content -> PPrint.doc

end = struct
    structure PP = PPrint
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>
    val op<$> = PPrint.<$>

    datatype expr = Fn of Pos.t * method vector
                  | Block of Pos.t * stmt vector * expr
                  | Var of Pos.t * var
                  | Const of Pos.t * value
    and method = Method of expr * expr option * expr
    and stmt = Def of expr * expr option * expr
             | Expr of expr
    and var = Lex of string
             | Dyn of string
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
         | Dyn _ => NONE

    val dynName =
        fn Dyn name => SOME name
         | Lex _ => NONE

    fun patBinders f =
        fn Fn _ => Vector.fromList [] (* actually, illegal pattern *)
         | Block _ => Vector.fromList [] (* actually, illegal pattern *)
         | Var (_, v) => OptionExt.toVector (f v)
         | Const _ => Vector.fromList []

    fun stmtBinders f =
        fn Def (pat, _, _) => patBinders f pat
         | Expr _ => Vector.fromList []

    fun blockBinders f stmts = VectorExt.flatMap (stmtBinders f) stmts

    val exprPos =
        fn Fn (pos, _) => pos
         | Block (pos, _, _) => pos
         | Var (pos, _) => pos
         | Const (pos, _) => pos

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

    val varToDoc =
        fn Lex cs  => PP.text cs
         | Dyn cs  => PP.text ("$" ^ cs)

    val rec exprToDoc =
        fn Fn (_, methods) =>
            PP.lBrace ^^
                PP.nest 2 (PP.line ^^ PP.punctuate (PP.semi ^^ PP.line)
                                                   (Vector.map methodToDoc methods)) <$>
                    PP.rBrace
         | Block (_, stmts, expr) =>
            PP.lBrace ^^
                PP.nest 2 (PP.line ^^ PP.punctuate (PP.semi ^^ PP.line)
                                                   (Vector.map stmtToDoc stmts) ^^
                               PP.semi <$> exprToDoc expr) <$>
                        PP.rBrace
         | Var (_, v)   => varToDoc v
         | Const (_, c) => valueToDoc c

    and stmtToDoc =
        fn Def (pat, SOME guard, expr) =>
            exprToDoc pat <+> PP.text "|" <+> exprToDoc guard <+> PP.text "=" <+> exprToDoc expr
         | Def (pat, NONE, expr) =>
            exprToDoc pat <+> PP.text "=" <+> exprToDoc expr
         | Expr expr => exprToDoc expr

    and methodToDoc =
        fn Method (pat, NONE, body) =>
            exprToDoc pat <+> PP.text "=>" <+> exprToDoc body
end
