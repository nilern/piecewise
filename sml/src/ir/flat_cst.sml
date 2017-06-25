(* TODO: Add Var.t variant (or something like that) for fn ptrs *)

(* Like CST0, but alphatized and closure converted. *)
structure FlatCST0 = struct

structure NameSet = BinarySetFn(type ord_key = Name.t
                                val compare = Name.compare)

structure PP = PPrint
val op^^ = PP.^^
val op<+> = PP.<+>
val op<$> = PP.<$>

structure Env :> sig
    type t
    datatype res = Direct of Name.t
                 | Clover of Name.t * int

    val empty : t
    val pushFnFrame : t -> Name.t -> t
    val pushCaseFrame : t -> NameSet.set -> t
    val pushBlockFrame : t -> NameSet.set -> t

    val find : t -> Name.t -> res option
    val self : t -> Name.t option
    val clovers : t -> Name.t vector option

    val toString : t -> string
end = struct
    structure Frame :> sig
        type fn_frame
        val ffName : fn_frame -> Name.t
        val index : fn_frame -> Name.t -> int

        type bindings

        datatype t = Fn of fn_frame
                   | Case of bindings
                   | Block of bindings

        val newFn : Name.t -> t
        val newCase : NameSet.set -> t
        val newBlock : NameSet.set -> t

        val find : t -> Name.t -> Name.t option
        val self : t -> Name.t option
        val clovers : t -> Name.t vector option

        val toString : t -> string
    end = struct
        structure Bindings = BinaryMapFn(type ord_key = Name.t
                                         val compare = Name.compare)
        type bindings = Name.t Bindings.map

        structure ClIndices = HashTableFn(type hash_key = Name.t
                                          val hashVal = Name.hash
                                          val sameKey = op=)
        type cl_indices = int ClIndices.hash_table

        type fn_frame = Name.t * cl_indices * int ref
        datatype t = Fn of fn_frame
                   | Case of bindings
                   | Block of bindings

        val ffName: fn_frame -> Name.t = #1

        fun index (_, clis, counter) key =
            case ClIndices.find clis key
            of SOME i => i
             | NONE => let val i = !counter
                       in
                           counter := i + 1;
                           ClIndices.insert clis (key, i);
                           i
                       end

        local
            fun newBindings names =
                NameSet.foldl (fn (name, bs) =>
                                  let val name' = Name.fresh (Name.chars name)
                                  in Bindings.insert (bs, name, name')
                                  end)
                              Bindings.empty names
        in
            fun newFn self = Fn (self, ClIndices.mkTable (0, Subscript), ref 0)
            fun newCase names = Case (newBindings names)
            fun newBlock names = Block (newBindings names)
        end

        fun find (Fn (self, _, _)) key = if key = self then SOME self else NONE
          | find (Case bs) key = Bindings.find (bs, key)
          | find (Block bs) key = Bindings.find (bs, key)

        fun self (Fn (s, _, _)) = SOME s
          | self (Case _) = NONE
          | self (Block _) = NONE

        fun clovers (Fn (_, cis, _)) =
            let val arr = Array.tabulate (ClIndices.numItems cis,
                                          fn _ => Name.Plain "")
            in
                ClIndices.appi (fn (s, i) => Array.update (arr, i, s)) cis;
                SOME (Array.vector arr)
            end
          | clovers (Case _) = NONE
          | clovers (Block _) = NONE

        fun bindingsToString bs =
            Bindings.foldli (fn (k, v, acc) =>
                                acc ^ Name.toString k ^ ": " ^
                                Name.toString v ^ ", ")
                            "" bs

        fun toString (Fn (self, _, _)) = "Fn " ^ Name.toString self
          | toString (Case bs) = "Case " ^ bindingsToString bs
          | toString (Block bs) = "Block " ^ bindingsToString bs
    end (* structure Frame *)

    type t = Frame.t list
    datatype res = Direct of Name.t
                 | Clover of Name.t * int

    fun resToName (Direct name) = name
      | resToName (Clover (name, _)) = name

    val empty = []
    fun pushFnFrame env self = Frame.newFn self :: env
    fun pushCaseFrame env names = Frame.newCase names :: env
    fun pushBlockFrame env names = Frame.newBlock names :: env

    local
        fun findName (frame :: env') key =
            (case Frame.find frame key
             of SOME name => SOME name
              | NONE => findName env' key)
          | findName [] _ = NONE
        fun findClover caller env key =
            let fun newClover _ =
                    Clover (Frame.ffName caller, Frame.index caller key)
            in Option.map newClover (findName env key)
            end
    in
        fun find _ (key as Name.Unique _) = SOME (Direct key)
          | find (frame :: env') key =
            (case Frame.find frame key
             of SOME name => SOME (Direct name)
              | NONE => (case frame
                         of Frame.Block _ => find env' key
                          | Frame.Case _ => find env' key
                          | Frame.Fn fnFrame => findClover fnFrame env' key))
          | find [] _ = NONE
    end

    fun self (frame :: env') =
        (case Frame.self frame
         of SOME s => SOME s
          | NONE => self env')
      | self [] = NONE

    fun clovers (frame :: env') =
        (case Frame.clovers frame
         of SOME cls => SOME cls
          | NONE => clovers env')
      | clovers [] = NONE

    fun toString frames =
        List.foldl (fn (f, acc) => acc ^ Frame.toString f ^ "\n") "" frames ^
            "\n"
end (* structure Env *)

exception Unbound of Pos.t * Name.t

datatype expr = Block of Pos.t * stmt vector
              | App of Pos.t * expr * expr vector
              | PrimApp of Pos.t * Primop.t * expr vector
              | Var of Pos.t * Var.t
              | Const of Pos.t * Const.t
and stmt = Def of expr * expr
         | AugDef of expr * expr
         | Expr of expr

withtype procCase = expr vector * expr option * expr

type proc = { name: Name.t
            , clovers: Name.t vector
            , cases: procCase vector }

type 'a program = { procs: proc vector, main: 'a }

fun trivial v = { procs = VectorExt.empty (), main = v }

fun mapMain f prog = { procs = #procs prog, main = f (#main prog)}

fun append f (p : 'a program) (q : 'b program) : 'c program =
    { procs = Vector.concat [#procs p, #procs q]
    , main = f (#main p) (#main q) }

fun map f vs =
    let val vs' = Vector.map f vs
    in
        { procs = VectorExt.flatMap #procs vs'
        , main = Vector.map #main vs' }
    end

fun flatMap f vs =
    let val vs' = Vector.map f vs
    in
        { procs = VectorExt.flatMap #procs vs'
        , main = VectorExt.flatMap #main vs' }
    end

fun exprToDoc (Block (_, stmts)) =
    (case Vector.length stmts
     of 1 => PP.braces (stmtsToDoc stmts)
      | _ => PP.lBrace ^^ PP.nest 4 (PP.line ^^ stmtsToDoc stmts) ^^
                 PP.line ^^ PP.rBrace)
  | exprToDoc (App (_, f, args)) =
    let fun step (arg, acc) = acc <+> exprToDoc arg
        val argDocs = Vector.foldl step (exprToDoc f) args
    in
        PP.parens (PP.align argDocs)
    end
  | exprToDoc (PrimApp (_, po, args)) =
    let fun step (arg, acc) = acc <+> exprToDoc arg
        val argDocs = Vector.foldl step (Primop.toDoc po) args
    in
        PP.parens (PP.align argDocs)
    end
  | exprToDoc (Var (_, v)) = Var.toDoc v
  | exprToDoc (Const (_, c)) = Const.toDoc c
and stmtToDoc (Def (pat, expr)) =
    exprToDoc pat <+> PP.text "=" <+> exprToDoc expr
  | stmtToDoc (AugDef (pat, expr)) =
    exprToDoc pat <+> PP.text "+=" <+> exprToDoc expr
  | stmtToDoc (Expr expr) = exprToDoc expr
and stmtsToDoc stmts =
    (case Vector.length stmts
     of 1 => stmtToDoc (Vector.sub (stmts, 0))
      | _ => let fun step (stmt, acc) =
                         acc ^^ PP.semi <$> stmtToDoc stmt
                 val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                 val rstmts = VectorSlice.slice(stmts, 1, NONE)
             in VectorSlice.foldl step stmtDoc rstmts end)

fun procToDoc {name = name, clovers = clovers, cases = cases} =
    let fun caseToDoc ((pats, cond, body): procCase) =
            let fun step (pat, acc) = acc <+> exprToDoc pat
                val patDoc = exprToDoc (Vector.sub (pats, 0))
                val rpats = VectorSlice.slice(pats, 1, NONE)
                val patsDoc = VectorSlice.foldl step patDoc rpats
                val condDoc = case cond
                              of SOME ce =>
                                     PP.space ^^ PP.text "|" <+>
                                         exprToDoc ce
                               | NONE => PP.empty
                val bodyDoc = exprToDoc body
            in
                (patsDoc ^^ condDoc) <+> PP.text "=>" <+> bodyDoc
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

fun patBindings (CST0.Const _) = NameSet.empty
  | patBindings (CST0.Var (_, Var.Lex name)) = NameSet.singleton name

fun stmtBindings (Stmt0.Def (pat, _)) = patBindings pat
  | stmtBindings (Stmt0.AugDef (pat, _)) = patBindings pat
  | stmtBindings (Stmt0.Expr _) = NameSet.empty

fun stmtVecBindings stmts =
    Vector.foldl (fn (stmt, acc) => NameSet.union (acc, stmtBindings stmt))
                 NameSet.empty stmts

fun elabPat env (CST0.Const (pos, c)) = trivial (Const (pos, c))
  | elabPat env (CST0.Var (pos, var as Var.Lex name)) =
    trivial (case Env.find env name
             of SOME (Env.Direct name) => Var (pos, Var.Lex name)
              | SOME (Env.Clover (self, i)) =>
                    PrimApp (pos, Primop.FnGet,
                             Vector.fromList [
                                 Var (pos, Var.Lex self),
                                 Const (pos, Const.Int (Int.toString i))])
              | NONE => raise Unbound (pos, name))

and elabExpr env (e as CST0.Fn (pos, cases)) =
    let val name = Name.fresh "f"
        val env' = Env.pushFnFrame env name
        val cprogs = Vector.map (elabCase env') cases
        val cprocs = VectorExt.flatMap #procs cprogs
        val cases' = Vector.map #main cprogs
        val clovers = Option.valOf (Env.clovers env')
        val procs = VectorExt.conj cprocs { name = name
                                          , clovers = clovers
                                          , cases = cases' }
        val cexprs = Vector.map (fn name => CST0.Var (pos, Var.Lex name)) clovers
        val close = elabExpr env (CST0.PrimApp (pos, Primop.Close, cexprs))
        val close' = case #main close (* HACK *)
                     of PrimApp (pos, Primop.Close, cexprs) =>
                            PrimApp (pos, Primop.Close,
                                     VectorExt.prepend cexprs
                                                       (Var (pos, Var.Lex name)))
                      | _ => raise Fail "unreachable"
    in { procs = procs , main = close' }
    end
  | elabExpr env (CST0.Block (pos, stmts)) =
    mapMain (fn stmts => Block (pos, stmts)) (elabStmts env stmts)
  | elabExpr env (CST0.App (pos, f, args)) =
    let fun makeApp f args = App (pos, PrimApp (pos, Primop.FnPtr,
                                                VectorExt.singleton f),
                                  VectorExt.prepend args f)
    in append makeApp (elabExpr env f) (map (elabExpr env) args)
    end
  | elabExpr env (CST0.PrimApp (pos, po, args)) =
    let fun makePrimApp po args = PrimApp (pos, po, args)
    in append makePrimApp (trivial po) (map (elabExpr env) args)
    end
  | elabExpr _ (CST0.Const (pos, c)) = trivial (Const (pos, c))
  | elabExpr env (CST0.Var (pos, var as Var.Lex name)) =
    trivial (case Env.find env name
             of SOME (Env.Direct name) => Var (pos, Var.Lex name)
              | SOME (Env.Clover (self, i)) =>
                    PrimApp (pos, Primop.FnGet,
                             Vector.fromList [
                                 Var (pos, Var.Lex self),
                                 Const (pos, Const.Int (Int.toString i))])
              | NONE => raise Unbound (pos, name))

and elabCase env (pats, cond, body) =
    let val self = Option.valOf (Env.self env)
        fun step (pat, acc) = NameSet.union (acc, patBindings pat)
        val names = Vector.foldl step (NameSet.singleton self) pats
        val env' = Env.pushCaseFrame env names
        val pprogs = Vector.map (elabPat env') pats
        val pprocs = VectorExt.flatMap #procs pprogs
        val pats' =
            VectorExt.prepend (Vector.map #main pprogs)
                              (Var (Pos.def, Var.Lex self))
        val bodyProg = elabExpr env' body
    in
        case cond
        of SOME c =>
           let val cp = elabExpr env' c
           in
               { procs = Vector.concat [pprocs, #procs cp, #procs bodyProg]
               , main = (pats', SOME (#main cp), #main bodyProg) }
           end
         | NONE =>
           { procs = Vector.concat [pprocs, #procs bodyProg]
           , main = (pats', NONE, #main bodyProg) }
    end

and elabStmt env (Stmt0.Def (pat, expr)) =
    let fun makeMain pat expr = Vector.fromList [Def (pat, expr)]
    in append makeMain (elabPat env pat) (elabExpr env expr)
    end
  | elabStmt env (Stmt0.AugDef (pat, expr)) =
    let fun makeMain pat expr = Vector.fromList [AugDef (pat, expr)]
    in append makeMain (elabPat env pat) (elabExpr env expr)
    end
  | elabStmt env (Stmt0.Expr expr) =
    mapMain (VectorExt.singleton o Expr) (elabExpr env expr)
and elabStmts env stmts =
    let val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
    in flatMap (elabStmt env') stmts
    end

fun fromCST0 stmts = elabStmts Env.empty stmts

end (* structure FlatCST0 *)
