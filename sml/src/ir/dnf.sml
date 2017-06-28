structure DNF :> sig

type 'expr t

val always : unit -> 'expr t
val require : 'expr -> 'expr t

val disj : 'expr t vector -> 'expr t
val conj : 'expr t vector-> 'expr t
val neg : 'expr t -> 'expr t

val toDoc : ('expr -> PPrint.doc) -> 'expr t -> PPrint.doc

end = struct

structure PP = PPrint
val op^^ = PP.^^
val op<+> = PP.<+>

structure Atom :> sig
    datatype 'expr t = Require of word * 'expr
                     | Forbid of word * 'expr

    val require : 'expr -> 'expr t

    val neg : 'expr t -> 'expr t

    val toDoc : ('expr -> PPrint.doc) -> 'expr t -> PPrint.doc
end = struct
    datatype 'expr t = Require of word * 'expr
                     | Forbid of word * 'expr

    local
        val counter = ref 0w0
    in
        fun require expr =
            let val i = !counter
            in
                counter := i + 0w1;
                Require (i, expr)
            end
    end

    fun neg (Require ie) = Forbid ie
      | neg (Forbid ie) = Require ie

    fun toDoc exprToDoc (Require (_, expr)) = exprToDoc expr
      | toDoc exprToDoc (Forbid (_, expr)) =
        PP.text "@!" ^^ PP.parens (exprToDoc expr)
end (* structure Atom *)

structure Clause : sig
    type 'expr t

    val return : 'expr Atom.t -> 'expr t

    val always : unit -> 'expr t
    val require : 'expr -> 'expr t

    val toDoc : ('expr -> PPrint.doc) -> 'expr t -> PPrint.doc
end = struct
    type 'expr t = 'expr Atom.t vector

    val return = VectorExt.singleton

    fun always () = VectorExt.empty ()
    fun require expr = VectorExt.singleton (Atom.require expr)

    fun toDoc exprToDoc atoms =
        case Vector.length atoms
        of 0 => PP.text "(@&&)"
         | _ => let val aDoc = Atom.toDoc exprToDoc (Vector.sub (atoms, 0))
                    val ras = VectorSlice.slice (atoms, 1, NONE)
                    fun step (atom, acc) =
                        acc <+> PP.text "@&&" <+> Atom.toDoc exprToDoc atom
                in
                    VectorSlice.foldl step aDoc ras
                end
end (* structure Clause *)

type 'expr t = 'expr Clause.t vector

val return = VectorExt.singleton

fun always () = VectorExt.singleton (Clause.always ())
fun require expr = VectorExt.singleton (Clause.require expr)

fun disj dnfs = VectorExt.flatten dnfs

fun conj dnfs =
    let fun conj2 (dnf, dnf') =
            VectorExt.flatMap
                (fn clause => Vector.map (VectorExt.concat clause) dnf')
                dnf
    in Vector.foldl conj2 (always ()) dnfs
    end

fun neg dnf =
    let val negClause = disj o (Vector.map (return o Clause.return o Atom.neg))
    in conj (Vector.map negClause dnf)
    end

fun toDoc exprToDoc clauses =
    case Vector.length clauses
    of 0 => PP.text "(@||)"
     | _ => let val cDoc = Clause.toDoc exprToDoc (Vector.sub (clauses, 0))
                val rcs = VectorSlice.slice (clauses, 1, NONE)
                fun step (clause, acc) =
                    acc <+> PP.text "@||" <+> Clause.toDoc exprToDoc clause
            in
                VectorSlice.foldl step cDoc rcs
            end
end
