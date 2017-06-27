(* TODO: Add Var.t variant (or something like that) for fn ptrs *)

(* Like CST0, but alphatized and closure converted. *)
structure FlatCST :> sig
    datatype expr = FixE of (expr, stmt) Expr1.t
    and stmt = FixS of (expr, bind) Stmt0.t
    and bind = Bind of expr * expr option

    type proc = { name: Name.t
                , clovers: Name.t vector
                , cases: (Name.t * bind * expr) vector }

    type 'a program = { procs: proc vector, main: 'a }

    val mapMain : ('a -> 'b) -> 'a program -> 'b program
    val append : ('a -> 'b -> 'c) -> 'a program -> 'b program -> 'c program
    val map : ('a -> 'b program) -> 'a vector -> 'b vector program
    val flatMap : ('a -> 'b vector program) -> 'a vector -> 'b vector program

    val wrapE : (expr, stmt) Expr1.t -> expr
    val wrapS : (expr, bind) Stmt0.t -> stmt

    val unwrapE : expr -> (expr, stmt) Expr1.t
    val unwrapS : stmt -> (expr, bind) Stmt0.t

    val trivial : 'a -> 'a program
    val trivialE : (expr, stmt) Expr1.t -> expr program
    val trivialS : (expr, bind) Stmt0.t -> stmt program

    val exprToDoc : expr -> PPrint.doc
    val stmtToDoc : stmt -> PPrint.doc
    val procToDoc : proc -> PPrint.doc
    val toDoc : stmt vector program -> PPrint.doc
end = struct

structure PP = PPrint
val op^^ = PP.^^
val op<+> = PP.<+>
val op<$> = PP.<$>

datatype expr = FixE of (expr, stmt) Expr1.t
and stmt = FixS of (expr, bind) Stmt0.t
and bind = Bind of expr * expr option

type proc = { name: Name.t
            , clovers: Name.t vector
            , cases: (Name.t * bind * expr) vector }

type 'a program = { procs: proc vector, main: 'a }

fun mapMain f {procs = procs, main = main} = { procs = procs, main = f main}

fun append f {procs = procs, main = main} {procs = procs', main = main'} =
    { procs = Vector.concat [procs, procs']
    , main = f main main' }

fun map (f: 'a -> 'b program) (vs: 'a vector) : 'b vector program =
    let val vs' = Vector.map f vs
    in
        { procs = VectorExt.flatMap #procs vs'
        , main = Vector.map #main vs' }
    end

fun flatMap (f: 'a -> 'b vector program) (vs: 'a vector) : 'b vector program =
    let val vs' = Vector.map f vs
    in
        { procs = VectorExt.flatMap #procs vs'
        , main = VectorExt.flatMap #main vs' }
    end

val wrapE = FixE
val wrapS = FixS

fun unwrapE (FixE expr) = expr
fun unwrapS (FixS stmt) = stmt

fun trivial v = { procs = VectorExt.empty (), main = v }
val trivialE = mapMain wrapE o trivial
val trivialS = mapMain wrapS o trivial

fun exprToDoc expr = Expr1.toDoc exprToDoc stmtToDoc (unwrapE expr)
and stmtToDoc stmt = Stmt0.toDoc exprToDoc bindToDoc (unwrapS stmt)
and bindToDoc (Bind (pat, cond)) =
    exprToDoc pat ^^ (case cond
                      of SOME ce => PP.space ^^ PP.text "|" <+> exprToDoc ce
                       | NONE => PP.empty)

fun stmtsToDoc stmts =
    (case Vector.length stmts
     of 1 => stmtToDoc (Vector.sub (stmts, 0))
      | _ => let fun step (stmt, acc) =
                         acc ^^ PP.semi <$> stmtToDoc stmt
                 val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                 val rstmts = VectorSlice.slice(stmts, 1, NONE)
             in VectorSlice.foldl step stmtDoc rstmts end)

fun procToDoc {name = name, clovers = clovers, cases = cases} =
    let fun caseToDoc (self, Bind (pat, cond), body) =
            let val condDoc = case cond
                              of SOME ce => PP.space ^^ PP.text "|" <+>
                                                exprToDoc ce
                               | NONE => PP.empty
            in Name.toDoc self ^^ condDoc <+> PP.text "=>" <+> exprToDoc body
            end
    in Name.toDoc name ^^
           PP.braces
               (Vector.foldl (fn (cl, acc) => acc <+> Name.toDoc cl)
                             PP.empty clovers) <+>
           PP.text "=" <+>
               (case Vector.length cases
                of 1 => PP.braces (caseToDoc (Vector.sub (cases, 0)))
                 | _ => let fun step (cs, acc) =
                                    acc ^^ PP.semi <$> caseToDoc cs
                            val caseDoc = caseToDoc (Vector.sub (cases, 0))
                            val rcases = VectorSlice.slice(cases, 1, NONE)
                            val caseDocs = VectorSlice.foldl step caseDoc rcases
                        in
                            PP.lBrace ^^
                                PP.nest 4 (PP.line ^^ caseDocs) ^^
                                PP.line ^^ PP.rBrace
                        end)
    end

fun toDoc (prog : (stmt vector) program) =
    let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
    in
        Vector.foldl step PP.empty (#procs prog) <$>
            stmtsToDoc (#main prog)
    end

end (* structure FlatCST0 *)
