structure FlattenScope :> sig
exception Unbound of Pos.t * Name.t

val flatten : CST0.stmt vector -> FlatCST.stmt vector FlatCST.program
end = struct

structure Name0Set = BinarySetFn(type ord_key = Name.t
                                 val compare = Name.compare)
structure Var = Expr1.Var
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
    val pushCaseFrame : t -> Name0Set.set -> t
    val pushBlockFrame : t -> Name0Set.set -> t

    val find : t -> Name.t -> res option
    val self : t -> Name.t option
    val self0 : t -> Name.t option
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
        val newCase : Name0Set.set -> t
        val newBlock : Name0Set.set -> t

        val find : t -> Name.t -> Name.t option
        val self : t -> Name.t option
        val self0 : t -> Name.t option
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

        type fn_frame = Name.t * Name.t * cl_indices * int ref
        datatype t = Fn of fn_frame
                   | Case of bindings
                   | Block of bindings

        val ffName: fn_frame -> Name.t = #2

        fun index (_, _, clis, counter) key =
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
                Name0Set.foldl (fn (name, bs) =>
                                    Bindings.insert (bs, name, Name.fresh name))
                                Bindings.empty names
        in
            fun newFn self = Fn ( self
                                , Name.fresh self
                                , ClIndices.mkTable (0, Subscript)
                                , ref 0 )
            fun newCase names = Case (newBindings names)
            fun newBlock names = Block (newBindings names)
        end

        fun find (Fn (selfStr, self, _, _)) key =
            if key = selfStr then SOME self else NONE
          | find (Case bs) key = Bindings.find (bs, key)
          | find (Block bs) key = Bindings.find (bs, key)

        fun self (Fn (_, s, _, _)) = SOME s
          | self (Case _) = NONE
          | self (Block _) = NONE

        fun self0 (Fn (s, _, _, _)) = SOME s
          | self0 (Case _) = NONE
          | self0 (Block _) = NONE

        fun clovers (Fn (_, _, cis, _)) =
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

        fun toString (Fn (_, self, _, _)) = "Fn " ^ Name.toString self
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
            let fun newClover name =
                    Clover (Frame.ffName caller, Frame.index caller name)
            in Option.map newClover (findName env key)
            end
    in
        fun find (frame :: env') key =
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

    fun self0 (frame :: env') =
        (case Frame.self0 frame
         of SOME s => SOME s
          | NONE => self0 env')
      | self0 [] = NONE

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

fun patBindings (CST0.FixE (Expr0.Const _)) = Name0Set.empty
  | patBindings (CST0.FixE (Expr0.Var (_, Expr0.Var.Lex name))) =
    Name0Set.singleton name

fun stmtBindings stmt =
    case stmt
    of CST0.FixS (Stmt0.Def (CST0.Bind (pat, _), _)) => patBindings pat
     | CST0.FixS (Stmt0.AugDef (CST0.Bind (pat, _), _)) => patBindings pat
     | CST0.FixS (Stmt0.Expr _) => Name0Set.empty

fun stmtVecBindings stmts =
    Vector.foldl (fn (stmt, acc) => Name0Set.union (acc, stmtBindings stmt))
                 Name0Set.empty stmts

fun elabPat env (CST0.FixE expr) =
    case expr
    of Expr0.Const (pos, c) => trivialE (Expr1.Const (pos, c))
     | Expr0.Var (pos, var as Expr0.Var.Lex name) =>
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
    of Expr0.Fn (pos, _, cases) =>
       let val env' = Env.pushFnFrame env (Name.fromString "f")
           val name = Option.valOf (Env.self env')
           val cprogs = Vector.map (elabCase env') cases
           val cprocs = VectorExt.flatMap #procs cprogs
           val cases' = Vector.map #main cprogs
           val clovers = Option.valOf (Env.clovers env')
           val procs = VectorExt.conj cprocs { name = name
                                             , clovers = clovers
                                             , cases = cases' }
           val cexprs = (* HACK *)
               Vector.map (fn name =>
                              CST0.wrapE
                                  (Expr0.Var (pos,
                                              Expr0.Var.Lex
                                                  (Name.fromString
                                                      (Name.chars name)))))
                          clovers
           val close =
               elabExpr env (CST0.wrapE (Expr0.PrimApp (pos, Primop.Close,
                                                       cexprs)))
           val close' = case #main close (* HACK *)
                        of FlatCST.FixE (Expr1.PrimApp (pos, Primop.Close, cexprs)) =>
                           wrapE (Expr1.PrimApp (pos, Primop.Close,
                                    VectorExt.prepend
                                             cexprs (wrapE (Expr1.Var (pos, Expr1.Var.Lex name)))))
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
     | Expr0.Var (pos, var as Expr0.Var.Lex name) =>
       trivialE (case Env.find env name
                of SOME (Env.Direct name) => Expr1.Var (pos, Var.Lex name)
                 | SOME (Env.Clover (self, i)) =>
                   Expr1.PrimApp (pos, Primop.FnGet,
                            Vector.fromList [
                                wrapE (Expr1.Var (pos, Var.Lex self)),
                                wrapE (Expr1.Const (pos, Const.Int (Int.toString i)))])
                 | NONE => raise Unbound (pos, name))

and elabCase env (CST0.Bind (pat, cond), body) =
    let val self = Option.valOf (Env.self env)
        val self0 = Option.valOf (Env.self0 env)
        val names = Name0Set.union (Name0Set.singleton self0, patBindings pat)
        val env' = Env.pushCaseFrame env names
        val pprog = elabPat env' pat
        val bodyProg = elabExpr env' body
    in
        case cond
        of SOME c =>
           let val cp = elabExpr env' c
           in
               { procs =
                   Vector.concat [#procs pprog, #procs cp, #procs bodyProg]
               , main =
                   (self, FlatCST.Bind (#main pprog, SOME (#main cp)),
                    #main bodyProg) }
           end
         | NONE =>
           { procs = Vector.concat [#procs pprog, #procs bodyProg]
           , main = (self, FlatCST.Bind (#main pprog, NONE), #main bodyProg) }
    end

and elabStmt env (CST0.FixS stmt) =
    case stmt
    of Stmt0.Def (CST0.Bind (pat, SOME cond), expr) =>
       let val pprog = elabPat env pat
           val cprog = elabExpr env cond
           val eprog = elabExpr env expr
       in
           { procs = Vector.concat [#procs pprog, #procs cprog, #procs eprog]
           , main = Stmt0.Def (FlatCST.Bind (#main pprog, SOME (#main cprog)), #main eprog) }
       end
     | Stmt0.Def (CST0.Bind (pat, NONE), expr) =>
       let val pprog = elabPat env pat
           val eprog = elabExpr env expr
       in
           { procs = Vector.concat [#procs pprog, #procs eprog]
           , main = Stmt0.Def (FlatCST.Bind (#main pprog, NONE), #main eprog) }
       end
     | Stmt0.AugDef (CST0.Bind (pat, SOME cond), expr) =>
        let val pprog = elabPat env pat
            val cprog = elabExpr env cond
            val eprog = elabExpr env expr
        in
            { procs = Vector.concat [#procs pprog, #procs cprog, #procs eprog]
            , main = Stmt0.AugDef (FlatCST.Bind (#main pprog, SOME (#main cprog)), #main eprog) }
        end
      | Stmt0.AugDef (CST0.Bind (pat, NONE), expr) =>
        let val pprog = elabPat env pat
            val eprog = elabExpr env expr
        in
            { procs = Vector.concat [#procs pprog, #procs eprog]
            , main = Stmt0.AugDef (FlatCST.Bind (#main pprog, NONE), #main eprog) }
        end
     | Stmt0.Expr expr =>
       mapMain (Stmt0.Expr) (elabExpr env expr)
and elabStmts env stmts =
    let val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
    in flatMap (mapMain (VectorExt.singleton o wrapS) o elabStmt env') stmts
    end

fun flatten stmts = elabStmts Env.empty stmts

end (* structure LexFlatten *)
