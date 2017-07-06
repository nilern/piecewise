(* TODO: also make dynamic env explicit *)

structure FlattenScope :> sig
exception Unbound of Pos.t * Name.t

val flatten : AuglessAst.stmt vector -> FlatAst.stmt vector FlatAst.program
end = struct

structure NameSet = BinarySetFn(type ord_key = Name.t
                                val compare = Name.compare)
val FixE = FlatAst.FixE
val FixS = FlatAst.FixS
val FixBS = FlatAst.FixBS
val trivial = FlatAst.trivial
val trivialE = FlatAst.trivialE
val mapMain = FlatAst.mapMain
val append = FlatAst.append
val map = FlatAst.map
val flatMap = FlatAst.flatMap

structure Env :> sig
    type t
    datatype res = Direct of Name.t
                 | Clover of Name.t * int

    val empty : t
    val pushFnFrame : t -> Name.t -> Name.t -> t
    val pushCaseFrame : t -> NameSet.set -> t
    val pushBlockFrame : t -> NameSet.set -> t

    val find : t -> Name.t -> res option
    val self : t -> Name.t option
    val self0 : t -> Name.t option
    val formals : t -> Name.t option
    val formals0 : t -> Name.t option
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

        val newFn : Name.t -> Name.t -> t
        val newCase : NameSet.set -> t
        val newBlock : NameSet.set -> t

        val find : t -> Name.t -> Name.t option
        val self : t -> Name.t option
        val self0 : t -> Name.t option
        val formals : t -> Name.t option
        val formals0 : t -> Name.t option
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

        type fn_frame = Name.t * Name.t * Name.t * Name.t * cl_indices * int ref
        datatype t = Fn of fn_frame
                   | Case of bindings
                   | Block of bindings

        val ffName: fn_frame -> Name.t = #2

        fun index (_, _, _, _, clis, counter) key =
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
                                    Bindings.insert (bs, name, Name.fresh name))
                                Bindings.empty names
        in
            fun newFn self formals = Fn ( self
                                        , Name.fresh self
                                        , formals
                                        , Name.fresh formals
                                        , ClIndices.mkTable (0, Subscript)
                                        , ref 0 )
            fun newCase names = Case (newBindings names)
            fun newBlock names = Block (newBindings names)
        end

        fun find (Fn (selfStr, self, _, _, _, _)) key =
            if key = selfStr then SOME self else NONE
          | find (Fn (_, _, formals, formals', _, _)) key =
            if key = formals then SOME formals' else NONE
          | find (Case bs) key = Bindings.find (bs, key)
          | find (Block bs) key = Bindings.find (bs, key)

        fun self (Fn (_, s, _, _, _, _)) = SOME s
          | self (Case _) = NONE
          | self (Block _) = NONE

        fun self0 (Fn (s, _, _, _, _, _)) = SOME s
          | self0 (Case _) = NONE
          | self0 (Block _) = NONE

        fun formals (Fn (_, _, _, fs, _, _)) = SOME fs
          | formals (Case _) = NONE
          | formals (Block _) = NONE

        fun formals0 (Fn (_, _, fs, _, _, _)) = SOME fs
          | formals0 (Case _) = NONE
          | formals0 (Block _) = NONE

        fun clovers (Fn (_, _, _, _, cis, _)) =
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

        fun toString (Fn (self, self', formals, formals', _, _)) =
            "Fn " ^ Name.toString self ^ ": " ^  Name.toString self' ^ ", "
                  ^ Name.toString formals ^ ": " ^  Name.toString formals'
          | toString (Case bs) = "Case " ^ bindingsToString bs
          | toString (Block bs) = "Block " ^ bindingsToString bs
    end (* structure Frame *)

    type t = Frame.t list
    datatype res = Direct of Name.t
                 | Clover of Name.t * int

    fun resToName (Direct name) = name
      | resToName (Clover (name, _)) = name

    val empty = []
    fun pushFnFrame env self formals = Frame.newFn self formals :: env
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

    fun formals (frame :: env') =
        (case Frame.formals frame
         of SOME s => SOME s
          | NONE => formals env')
      | formals [] = NONE

    fun formals0 (frame :: env') =
        (case Frame.formals0 frame
         of SOME s => SOME s
          | NONE => formals0 env')
      | formals0 [] = NONE

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

fun bindBindings (AuglessAst.Bind (_, _, bstmts), names) =
    let fun bstmtBindings (AuglessAst.FixBS bstmt, names) =
            case bstmt
            of BindStmt1.Def (Var.Lex name, _) =>
               NameSet.add (names, name)
             | BindStmt1.Expr _ => names
    in Vector.foldl bstmtBindings names bstmts
    end

fun stmtVecBindings stmts =
    let fun stmtBindings (AuglessAst.FixS stmt, names) =
            case stmt
            of AuglessStmt.Def (temp, bind, _) =>
               NameSet.add (bindBindings (bind, names), temp)
             | AuglessStmt.Expr _ => names
    in Vector.foldl stmtBindings NameSet.empty stmts
    end

fun elabExpr env (AuglessAst.FixE expr) =
    case expr
    of Expr.Fn (pos, formals, cases) =>
       let fun elabCase env (bind, body) =
               let val self = Option.valOf (Env.self env)
                   val oldnames = NameSet.fromList [ valOf (Env.self0 env)
                                                   , valOf (Env.formals0 env) ]
                   val names = bindBindings (bind, oldnames)
                   val env' = Env.pushCaseFrame env names
               in
                   FlatAst.append (fn bind' => fn body' =>
                                      ( self
                                      , valOf (Env.formals env')
                                      , bind'
                                      , body' ))
                                  (elabBind env' bind)
                                  (elabExpr env' body)
               end
           val env' = Env.pushFnFrame env (Name.fromString "f") formals
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
                              AuglessAst.FixE
                                  (Expr.Var (pos,
                                              Var.Lex
                                                  (Name.fromString
                                                      (Name.chars name)))))
                          clovers
           val close =
               elabExpr env (AuglessAst.FixE (Expr.PrimApp (pos, Primop.Close,
                                                       cexprs)))
           val close' = case #main close (* HACK *)
                        of FlatAst.FixE (FlatExpr.PrimApp (pos, Primop.Close, cexprs)) =>
                           FixE (FlatExpr.PrimApp (pos, Primop.Close,
                                    VectorExt.prepend
                                             cexprs (FixE (FlatExpr.Var (pos, Var.Lex name)))))
                         | _ => raise Fail "unreachable"
       in { procs = procs , main = close' }
       end
     | Expr.Block (pos, stmts) =>
       mapMain (fn stmts => FixE (FlatExpr.Block (pos, stmts))) (elabStmts env stmts)
     | Expr.App (pos, f, args) =>
       let fun makeApp f args = FixE (FlatExpr.App (pos, FixE (FlatExpr.PrimApp (pos, Primop.FnPtr,
                                     VectorExt.singleton f)),
                                     VectorExt.prepend args f))
       in append makeApp (elabExpr env f) (map (elabExpr env) args)
       end
     | Expr.PrimApp (pos, po, args) =>
       let fun makePrimApp po args = FixE (FlatExpr.PrimApp (pos, po, args))
       in append makePrimApp (trivial po) (map (elabExpr env) args)
       end
     | Expr.Const (pos, c) => trivialE (FlatExpr.Const (pos, c))
     | Expr.Var (pos, var as Var.Lex name) =>
       trivialE (case Env.find env name
                 of SOME (Env.Direct name) => FlatExpr.Var (pos, Var.Lex name)
                  | SOME (Env.Clover (self, i)) =>
                    FlatExpr.PrimApp (pos, Primop.FnGet,
                             Vector.fromList [
                                 FixE (FlatExpr.Var (pos, Var.Lex self)),
                                 FixE (FlatExpr.Const (pos, Const.Int (Int.toString i)))])
                  | NONE => raise Unbound (pos, name))

and elabStmt env (AuglessAst.FixS stmt) =
    case stmt
    of AuglessStmt.Def (temp, bind, expr) =>
       FlatAst.append (fn bind' => fn expr' =>
                          FixS (AuglessStmt.Def (temp, bind', expr')))
                      (elabBind env bind)
                      (elabExpr env expr)
     | AuglessStmt.Expr expr =>
       FlatAst.mapMain (FixS o AuglessStmt.Expr) (elabExpr env expr)

and elabStmts env stmts =
    let val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
    in flatMap (mapMain VectorExt.singleton o elabStmt env') stmts
    end

and elabBind env (AuglessAst.Bind (pos, dnf, bind)) =
    let fun elabBStmt env (AuglessAst.FixBS bstmt) =
            case bstmt
            of BindStmt1.Def (Var.Lex name, expr) =>
               let val SOME (Env.Direct name') = Env.find env name
               in
                   FlatAst.mapMain
                       (fn expr' =>
                           FixBS (BindStmt1.Def (Var.Lex name', expr')))
                       (elabExpr env expr)
               end
             | BindStmt1.Expr expr =>
               FlatAst.mapMain (FixBS o BindStmt1.Expr) (elabExpr env expr)
        val dnfProg = DNF.map (elabExpr env) dnf
        val bindProgs = Vector.map (elabBStmt env) bind
    in
        { procs = Vector.concat [ VectorExt.flatMap #procs (DNF.exprs dnfProg)
                                , VectorExt.flatMap #procs bindProgs ]
        , main = FlatAst.Bind ( pos
                              , DNF.map #main dnfProg
                              , Vector.map #main bindProgs) }
    end

fun flatten stmts = elabStmts Env.empty stmts

end (* structure LexFlatten *)
