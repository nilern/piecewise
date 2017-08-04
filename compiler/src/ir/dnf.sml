structure DNF :> sig
    structure Atom : sig
        type id
        datatype 'expr t = Require of id * id vector * 'expr
                         | Forbid of id * id vector * 'expr

        val require : 'expr -> id list -> 'expr t * id

        val neg : 'expr t -> 'expr t

        val isNeverWhen : ('e -> bool option) -> 'e t -> bool
        val expr : 'e t -> 'e
        val map : ('e -> 'f) -> 'e t -> 'f t

        val toDoc : ('expr -> PPrint.doc) -> 'expr t -> PPrint.doc
    end

    structure Clause : sig
        type 'expr t

        val return : 'expr Atom.t -> 'expr t

        val always : unit -> 'expr t
        val require : 'expr -> Atom.id list -> 'expr t * Atom.id

        val length : 'e t -> int
        val isAlways : 'e t -> bool
        val isNeverWhen : ('e -> bool option) -> 'e t -> bool
        val exprs : 'e t -> 'e vector
        val first : 'e t -> 'e
        val map : ('e -> 'f) -> 'e t -> 'f t
        val remove : ('e -> bool) -> 'e t -> 'e t

        val toDoc : ('expr -> PPrint.doc) -> 'expr t -> PPrint.doc
    end

    type 'expr t
    type id

    val always : unit -> 'expr t
    val require : 'expr -> id list -> 'expr t * id

    val disj : 'expr t vector -> 'expr t
    val conj : 'expr t vector-> 'expr t
    val neg : 'expr t -> 'expr t

    val isAlways : 'e t -> bool
    val exprs : 'e t -> 'e vector
    val map : ('e -> 'f) -> 'e t -> 'f t

    val toClauses : 'expr t -> 'expr Clause.t vector
    val toDoc : ('expr -> PPrint.doc) -> 'expr t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>

    structure Atom = struct
        type id = word
        datatype 'expr t = Require of id * id vector * 'expr
                         | Forbid of id * id vector * 'expr

        local
            val counter = ref 0w0
        in
            fun require expr deplist =
                let val i = !counter
                in
                    counter := i + 0w1;
                    (Require (i, Vector.fromList deplist, expr), i)
                end
        end

        fun neg (Require ie) = Forbid ie
          | neg (Forbid ie) = Require ie

        fun isNeverWhen info =
            fn Require (_, _, expr) =>
               (case info expr
                of SOME false => true
                 | _ => false)
             | Forbid (_, _, expr) =>
               (case info expr
                of SOME true => true
                 | _ => false)

        fun expr (Require (_, _, e)) = e
          | expr (Forbid (_, _, e)) = e

        fun map f (Require (id, deps, expr)) = Require (id, deps, f expr)
          | map f (Forbid (id, deps, expr)) = Forbid (id, deps, f expr)

        fun toDoc exprToDoc (Require (id, deps, expr)) =
            PP.word id <+> PP.text "<-" <+> PP.punctuate PP.space (Vector.map PP.word deps) <+>
                PP.parens (exprToDoc expr)
          | toDoc exprToDoc (Forbid args) = PP.text "@!" ^^ PP.parens (toDoc exprToDoc (Require args))
    end (* structure Atom *)

    structure Clause = struct
        type 'expr t = 'expr Atom.t vector

        val return = VectorExt.singleton

        fun always () = VectorExt.empty ()
        fun require expr deplist =
            let val (atom, i) = Atom.require expr deplist
            in (VectorExt.singleton atom, i)
            end

        val length = Vector.length

        fun isAlways atoms = Vector.length atoms = 0

        fun isNeverWhen info atoms = Vector.exists (Atom.isNeverWhen info) atoms

        fun exprs atoms = Vector.map Atom.expr atoms

        fun first atoms = Atom.expr (Vector.sub (atoms, 0))

        fun map f atoms = Vector.map (Atom.map f) atoms

        fun remove pred = VectorExt.remove (pred o Atom.expr)

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
    type id = Atom.id

    val return = VectorExt.singleton

    fun always () = VectorExt.singleton (Clause.always ())
    fun require expr deplist =
        let val (clause, i) = Clause.require expr deplist
        in (VectorExt.singleton clause, i)
        end

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

    fun isAlways clauses = Vector.all Clause.isAlways clauses

    fun exprs clauses = VectorExt.flatMap Clause.exprs clauses

    fun map f clauses = Vector.map (Clause.map f) clauses

    fun toClauses clauses = clauses

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
