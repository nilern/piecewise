structure LexFlatten :> sig
exception Unbound of Pos.t * Name.t

val flatten : CST0.stmt vector -> FlatCST.stmt vector FlatCST.program
end = struct

structure NameSet = BinarySetFn(type ord_key = Name.t
                                val compare = Name.compare)
val wrapE = FlatCST.wrapE
val wrapS = FlatCST.wrapS
val trivial = FlatCST.trivial
val trivialE = FlatCST.trivialE
val mapMain = FlatCST.mapMain
val append = FlatCST.append
val map = FlatCST.map
val flatMap = FlatCST.flatMap

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

fun patBindings (CST0.FixE (Expr0.Const _)) = NameSet.empty
  | patBindings (CST0.FixE (Expr0.Var (_, Var.Lex name))) =
    NameSet.singleton name

fun stmtBindings (CST0.FixS (Stmt0.Def (pat, _))) = patBindings pat
  | stmtBindings (CST0.FixS (Stmt0.AugDef (pat, _))) = patBindings pat
  | stmtBindings (CST0.FixS (Stmt0.Expr _)) = NameSet.empty

fun stmtVecBindings stmts =
    Vector.foldl (fn (stmt, acc) => NameSet.union (acc, stmtBindings stmt))
                 NameSet.empty stmts

fun elabPat env (CST0.FixE expr) =
    case expr
    of Expr0.Const (pos, c) => trivialE (Expr1.Const (pos, c))
     | Expr0.Var (pos, var as Var.Lex name) =>
       trivialE (case Env.find env name
                 of SOME (Env.Direct name) => Expr1.Var (pos, Var.Lex name)
                  | SOME (Env.Clover (self, i)) =>
                       Expr1.PrimApp (pos, Primop.FnGet,
                            Vector.fromList [
                                    wrapE (Expr1.Var (pos, Var.Lex self)),
                                    wrapE (Expr1.Const (pos, Const.Int (Int.toString i)))])
                 | NONE => raise Unbound (pos, name))

and elabExpr env (CST0.FixE expr) =
    case expr
    of Expr0.Fn (pos, cases) =>
       let val name = Name.fresh "f"
           val env' = Env.pushFnFrame env name
           val cprogs = Vector.map (elabCase env') cases
           val cprocs = VectorExt.flatMap #procs cprogs
           val cases' = Vector.map #main cprogs
           val clovers = Option.valOf (Env.clovers env')
           val procs = VectorExt.conj cprocs { name = name
                                             , clovers = clovers
                                             , cases = cases' }
           val cexprs =
               Vector.map (fn name => CST0.wrapE (Expr0.Var (pos,
                                                            Var.Lex name)))
                          clovers
           val close =
               elabExpr env (CST0.wrapE (Expr0.PrimApp (pos, Primop.Close,
                                                       cexprs)))
           val close' = case #main close (* HACK *)
                        of FlatCST.FixE (Expr1.PrimApp (pos, Primop.Close, cexprs)) =>
                           wrapE (Expr1.PrimApp (pos, Primop.Close,
                                    VectorExt.prepend
                                             cexprs (wrapE (Expr1.Var (pos, Var.Lex name)))))
                         | _ => raise Fail "unreachable"
       in { procs = procs , main = close' }
       end
     | Expr0.Block (pos, stmts) =>
       mapMain (fn stmts => wrapE (Expr1.Block (pos, stmts))) (elabStmts env stmts)
     | Expr0.App (pos, f, args) =>
       let fun makeApp f args = wrapE (Expr1.App (pos, wrapE (Expr1.PrimApp (pos, Primop.FnPtr,
                                     VectorExt.singleton f)),
                                     VectorExt.prepend args f))
       in append makeApp (elabExpr env f) (map (elabExpr env) args)
       end
     | Expr0.PrimApp (pos, po, args) =>
       let fun makePrimApp po args = wrapE (Expr1.PrimApp (pos, po, args))
       in append makePrimApp (trivial po) (map (elabExpr env) args)
       end
     | Expr0.Const (pos, c) => trivialE (Expr1.Const (pos, c))
     | Expr0.Var (pos, var as Var.Lex name) =>
       trivialE (case Env.find env name
                of SOME (Env.Direct name) => Expr1.Var (pos, Var.Lex name)
                 | SOME (Env.Clover (self, i)) =>
                   Expr1.PrimApp (pos, Primop.FnGet,
                            Vector.fromList [
                                wrapE (Expr1.Var (pos, Var.Lex self)),
                                wrapE (Expr1.Const (pos, Const.Int (Int.toString i)))])
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
                              (wrapE (Expr1.Var (Pos.def, Var.Lex self)))
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

and elabStmt env (CST0.FixS stmt) =
    case stmt
    of Stmt0.Def (pat, expr) =>
       let fun makeMain pat expr =
               Vector.fromList [wrapS (Stmt0.Def (pat, expr))]
       in append makeMain (elabPat env pat) (elabExpr env expr)
       end
     | Stmt0.AugDef (pat, expr) =>
       let fun makeMain pat expr =
               Vector.fromList [wrapS (Stmt0.AugDef (pat, expr))]
       in append makeMain (elabPat env pat) (elabExpr env expr)
       end
     | Stmt0.Expr expr =>
       mapMain (VectorExt.singleton o wrapS o Stmt0.Expr) (elabExpr env expr)
and elabStmts env stmts =
    let val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
    in flatMap (elabStmt env') stmts
    end

fun flatten stmts = elabStmts Env.empty stmts

end (* structure LexFlatten *)
