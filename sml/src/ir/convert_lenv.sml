structure StringHashTable = HashTableFn(type hash_key = string
                                        val hashVal = HashString.hashString
                                        val sameKey = op=)
structure FlatAst0 = FlatAst(Var)

(* TODO: Call StraightenScope from inside this since it just works on one block at a time AND would
         produce better code if it had access to free variable information. So LexEnv.t should also
         keep track of which variables are used before they are initialized. *)
structure ConvertLEnv :> sig
    exception Unbound of Pos.t * Name.t

    val convert : AuglessAst.stmt vector -> FlatAst0.program
end = struct
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

    val FixE = FlatAst0.FixE
    val FixS = FlatAst0.FixS
    val FixBS = FlatAst0.FixBS
    val Block = FlatAst0.Expr.Block
    val App = FlatAst0.Expr.App
    val PrimApp = FlatAst0.Expr.PrimApp
    val Var = FlatAst0.Expr.Var
    val Const = FlatAst0.Expr.Const
    val Def = AuglessStmt.Def
    val Expr = AuglessStmt.Expr

    exception Unbound of Pos.t * Name.t

    fun bindBindings (AuglessAst.Bind (_, _, bstmts), names) =
        let fun bstmtBindings (AuglessAst.FixBS bstmt, names) =
                case bstmt
                of BindStmt1.Def (Var.Lex name, _) => NameSet.add (names, name)
                 | BindStmt1.Def (Var.Dyn _, _) => names
                 | BindStmt1.Expr _ => names
        in Vector.foldl bstmtBindings names bstmts
        end

    fun stmtVecBindings stmts =
        let fun stmtBindings (AuglessAst.FixS stmt, names) =
                case stmt
                of AuglessStmt.Def (temp, bind, _) => NameSet.add (bindBindings (bind, names), temp)
                 | AuglessStmt.Expr _ => names
        in Vector.foldl stmtBindings NameSet.empty stmts
        end

    fun convert stmts =
        let val procs = StringHashTable.mkTable (0, Subscript)
            fun elabStmts env stmts =
                let fun elabExpr env (AuglessAst.FixE expr) =
                        FixE (case expr
                              of Expr.Fn (pos, formals, cases) =>
                                 let fun elabCase env (bind, body) =
                                         let val self = Option.valOf (Env.self env)
                                             val oldnames =
                                                 NameSet.fromList [ valOf (Env.self0 env)
                                                                  , valOf (Env.formals0 env) ]
                                             val names = bindBindings (bind, oldnames)
                                             val env' = Env.pushCaseFrame env names
                                         in
                                             ( self
                                             , valOf (Env.formals env')
                                             , Name.freshFromString "denv"
                                             , elabBind env' bind
                                             , elabExpr env' body )
                                         end
                                     val env' = Env.pushFnFrame env (Name.fromString "f") formals
                                     val cases' = Vector.map (elabCase env') cases
                                     val name = Option.valOf (Env.self env')
                                     val clovers = Option.valOf (Env.clovers env')
                                     val proc = { name = name
                                                , clovers = clovers
                                                , cases = cases' }
                                     val _ =
                                         StringHashTable.insert procs (Name.toString name, proc)
                                     val cexprs = (* HACK *)
                                         Vector.map (fn name =>
                                                        AuglessAst.FixE
                                                            (Expr.Var (pos,
                                                                        Var.Lex
                                                                            (Name.fromString
                                                                               (Name.chars name)))))
                                                     clovers
                                     val close = elabExpr env
                                                          (AuglessAst.FixE
                                                              (Expr.PrimApp (pos, Primop.Close,
                                                                             cexprs)))
                                 in
                                     case close (* HACK *)
                                     of FlatAst0.FixE
                                            (FlatAst0.Expr.PrimApp (pos, Primop.Close, cexprs)) =>
                                        let val fnPtr = FixE (Var (pos, Var.Lex name))
                                        in PrimApp (pos, Primop.Close,
                                                    VectorExt.prepend cexprs fnPtr)
                                        end
                                      | _ => raise Fail "unreachable"
                                 end
                               | Expr.Block (pos, stmts) =>
                                 Block (pos, elabStmts env stmts)
                               | Expr.App (pos, f, args) => (* MAYBE: add the self-closure arg *)
                                 App (pos, elabExpr env f, Vector.map (elabExpr env) args)
                               | Expr.PrimApp (pos, po, args) =>
                                 PrimApp (pos, po, Vector.map (elabExpr env) args)
                               | Expr.Var (pos, Var.Lex name) =>
                                 (case Env.find env name
                                  of SOME (Env.Direct name) => Var (pos, Var.Lex name)
                                   | SOME (Env.Clover (self, i)) =>
                                     let val index = Const.Int (Int.toString i)
                                     in PrimApp ( pos
                                                , Primop.FnGet
                                                , Vector.fromList [ FixE (Var (pos, Var.Lex self)),
                                                                    FixE (Const (pos, index)) ] )
                                     end
                                   | NONE => raise Unbound (pos, name))
                              | Expr.Var (pos, var as Var.Dyn _) => Var (pos, var)
                              (* TODO: Var.Upper* *)
                              | Expr.Const (pos, c) => Const (pos, c))
                    and elabBindStmt env (AuglessAst.FixBS bstmt) =
                        FixBS (case bstmt
                               of BindStmt1.Def (Var.Lex name, expr) =>
                                  let val SOME (Env.Direct name') = Env.find env name
                                  in BindStmt1.Def (Var.Lex name', elabExpr env expr)
                                  end
                                | BindStmt1.Def (var as Var.Dyn _, expr) =>
                                  BindStmt1.Def (var, elabExpr env expr)
                                | BindStmt1.Expr expr => BindStmt1.Expr (elabExpr env expr))
                    and elabBind env (AuglessAst.Bind (pos, dnf, bind)) =
                        FlatAst0.Bind ( pos
                                      , DNF.map (elabExpr env) dnf
                                      , Vector.map (elabBindStmt env) bind )
                    fun elabStmt env (AuglessAst.FixS stmt) =
                        FixS (case stmt
                              of AuglessStmt.Def (temp, bind, expr) =>
                                 let val SOME (Env.Direct temp') = Env.find env temp
                                 in Def (temp', elabBind env bind, elabExpr env expr)
                                 end
                               | AuglessStmt.Expr expr => Expr (elabExpr env expr))
                    val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
                in Vector.map (elabStmt env') stmts
                end
            val stmts' = elabStmts Env.empty stmts
        in
            { procs = Vector.fromList (StringHashTable.listItems procs)
            , main = stmts' }
        end
end (* structure ConvertLEnv *)
