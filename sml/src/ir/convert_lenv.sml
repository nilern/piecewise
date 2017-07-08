structure NameSet = BinarySetFn(type ord_key = Name.t
                                val compare = Name.compare)
structure StringHashTable = HashTableFn(type hash_key = string
                                        val hashVal = HashString.hashString
                                        val sameKey = op=)
structure NameHashTable = HashTableFn(type hash_key = Name.t
                                      val hashVal = Name.hash
                                      val sameKey = op=)
structure FlatAst0 = FlatAst(Var)

structure ConvertLEnv :> sig
    exception Unbound of Pos.t * Name.t

    val convert : AuglessAst.stmt vector -> FlatAst0.program
end = struct
    structure Env :> sig
        type t

        datatype status = Pristine | Used | DefdBeforeUse | UsedBeforeDef
        datatype 'a res = Direct of 'a
                        | Clover of 'a * int

        val empty : t
        val pushFnFrame : t -> Name.t -> Name.t -> t
        val pushCaseFrame : t -> NameSet.set -> t
        val pushBlockFrame : t -> NameSet.set -> t

        val use : t -> Name.t -> Name.t res option
        val define : t -> Name.t -> (Name.t * status) res option
        val self : t -> Name.t option
        val self0 : t -> Name.t option
        val formals : t -> Name.t option
        val formals0 : t -> Name.t option
        val clovers : t -> Name.t vector option
        val locals : t -> (Name.t * status) vector

        val toString : t -> string
    end = struct (* FIXME: DRY *)
        datatype status = Pristine | Used | DefdBeforeUse | UsedBeforeDef

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

            val findAndTransition : (status -> status) -> t -> Name.t -> (Name.t * status) option
            val self : t -> Name.t option
            val self0 : t -> Name.t option
            val formals : t -> Name.t option
            val formals0 : t -> Name.t option
            val clovers : t -> Name.t vector option
            val locals : t -> (Name.t * status) list

            val toString : t -> string
        end = struct
            type bindings = (Name.t * status) NameHashTable.hash_table

            type cl_indices = int NameHashTable.hash_table

            (* TODO: use a record: *)
            type fn_frame = Name.t * Name.t * Name.t * Name.t * cl_indices * int ref
            datatype t = Fn of fn_frame
                       | Case of bindings
                       | Block of bindings

            val ffName: fn_frame -> Name.t = #2

            fun index (_, _, _, _, clis, counter) key =
                case NameHashTable.find clis key
                of SOME i => i
                 | NONE => let val i = !counter
                           in
                               counter := i + 1;
                               NameHashTable.insert clis (key, i);
                               i
                           end

            local
                fun newBindings names =
                    let val bindings = NameHashTable.mkTable (0, Subscript)
                    in
                        NameSet.app (fn name =>
                                        NameHashTable.insert bindings (name, ( Name.fresh name
                                                                             , Pristine)))
                                    names;
                        bindings
                    end
            in
                fun newFn self formals = Fn ( self
                                            , Name.fresh self
                                            , formals
                                            , Name.fresh formals
                                            , NameHashTable.mkTable (0, Subscript)
                                            , ref 0 )
                fun newCase names = Case (newBindings names)
                fun newBlock names = Block (newBindings names)
            end

            local
                fun performTransition transition bs key (name, status) =
                    let val entry' = (name, transition status)
                    in
                        NameHashTable.insert bs (key, entry');
                        entry'
                    end
            in
                fun findAndTransition transition (Fn (selfStr, self, formals, formals', _, _)) key =
                    if key = selfStr then SOME (self, DefdBeforeUse)
                    else if key = formals then SOME (formals', DefdBeforeUse)
                    else NONE
                  | findAndTransition transition (Case bs) key =
                    Option.map (performTransition transition bs key) (NameHashTable.find bs key)
                  | findAndTransition transition (Block bs) key =
                    Option.map (performTransition transition bs key) (NameHashTable.find bs key)
            end

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
                let val arr = Array.tabulate (NameHashTable.numItems cis,
                                              let val v = Name.Plain "" in fn _ => v end)
                in
                    NameHashTable.appi (fn (s, i) => Array.update (arr, i, s)) cis;
                    SOME (Array.vector arr)
                end
              | clovers (Case _) = NONE
              | clovers (Block _) = NONE

            fun locals (Fn (_, self, _, formals, _, _)) =
                [(self, DefdBeforeUse), (formals, DefdBeforeUse)]
              | locals (Case bs) = NameHashTable.listItems bs
              | locals (Block bs) = NameHashTable.listItems bs

            fun statusToString Pristine = "Pristine"
              | statusToString Used = "Used"
              | statusToString Defined = "Defined"

            fun bindingsToString bs =
                NameHashTable.foldi (fn (k, (name, status), acc) =>
                                        acc ^ Name.toString k ^ ": " ^
                                            statusToString status ^ " " ^ Name.toString name ^ ", ")
                                    "" bs

            fun toString (Fn (self, self', formals, formals', _, _)) =
                "Fn " ^ Name.toString self ^ ": " ^  Name.toString self' ^ ", "
                      ^ Name.toString formals ^ ": " ^  Name.toString formals'
              | toString (Case bs) = "Case " ^ bindingsToString bs
              | toString (Block bs) = "Block " ^ bindingsToString bs
        end (* structure Frame *)

        type t = Frame.t list
        datatype 'a res = Direct of 'a
                        | Clover of 'a * int

        val empty = []
        fun pushFnFrame env self formals = Frame.newFn self formals :: env
        fun pushCaseFrame env names = Frame.newCase names :: env
        fun pushBlockFrame env names = Frame.newBlock names :: env

        local
            fun findNameAndTransition transition (frame :: env') key =
                (case Frame.findAndTransition transition frame key
                 of SOME name => SOME name
                  | NONE => findNameAndTransition transition env' key)
              | findNameAndTransition _ [] _ = NONE
            fun findCloverAndTransition transition caller env key =
                let fun newClover (name, status) =
                        Clover ((Frame.ffName caller, status), Frame.index caller name)
                in Option.map newClover (findNameAndTransition transition env key)
                end
        in
            fun findAndTransition transition (frame :: env') key =
                (case Frame.findAndTransition transition frame key
                 of SOME name => SOME (Direct name)
                  | NONE => (case frame
                             of Frame.Block _ => findAndTransition transition env' key
                              | Frame.Case _ => findAndTransition transition env' key
                              | Frame.Fn fnFrame =>
                                findCloverAndTransition transition fnFrame env' key))
              | findAndTransition _ [] _ = NONE


            fun use env name =
                let val transition = fn Pristine => Used
                                      | Used => Used
                                      | DefdBeforeUse => DefdBeforeUse
                                      | UsedBeforeDef => UsedBeforeDef
                in
                    Option.map (fn Direct (name, _) => Direct name
                                 | Clover ((self, _), i) => Clover (self, i))
                               (findAndTransition transition env name)
                end
            val define = findAndTransition (fn Pristine => DefdBeforeUse
                                             | Used => UsedBeforeDef
                                             | DefdBeforeUse => DefdBeforeUse
                                             | UsedBeforeDef => UsedBeforeDef)
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

        val locals = fn frame :: _ => Vector.fromList (Frame.locals frame)
                      | [] => VectorExt.empty ()

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
                                 (case Env.use env name
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
                               of BindStmt1.Def (var as Var.Lex name, expr) =>
                                  (* TODO: use status *)
                                  let val expr' = elabExpr env expr
                                      val SOME (Env.Direct (name', status')) = Env.define env name
                                  in
                                      case status'
                                      of Env.UsedBeforeDef =>
                                         let val pos = FlatAst0.Expr.pos (FlatAst0.unwrapE expr')
                                             val varExpr = FixE (Var (pos, Var.Lex name'))
                                             val assign =
                                                 FixE (PrimApp (pos, Primop.BSet,
                                                                Vector.fromList [varExpr, expr']))
                                         in BindStmt1.Expr assign
                                         end
                                       | Env.DefdBeforeUse => BindStmt1.Def (Var.Lex name', expr')
                                       | _ => raise Fail "unreachable"
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
                                 let val SOME (Env.Direct (temp', DefdBeforeUse)) =
                                         Env.define env temp
                                 in Def (temp', elabBind env bind, elabExpr env expr)
                                 end
                               | AuglessStmt.Expr expr => Expr (elabExpr env expr))
                    val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
                    val stmts' = Vector.map (elabStmt env') stmts
                    val pos = AuglessAst.stmtPos (Vector.sub (stmts, 0))
                    val boxAlloc = FixE (PrimApp (pos, Primop.Box, VectorExt.empty ()))
                    fun newBoxDef name =
                        let val temp = Name.freshFromString "box"
                            val tExpr = FixE (Var (pos, Var.Lex temp))
                            val bind =
                                FlatAst0.Bind
                                    (pos, DNF.always (),
                                     VectorExt.singleton
                                         (FixBS (BindStmt1.Def (Var.Lex name, tExpr))))
                        in FixS (Def (temp, bind, boxAlloc))
                        end
                    val boxDefs =
                        VectorExt.flatMap (fn (name, Env.UsedBeforeDef) =>
                                              VectorExt.singleton (newBoxDef name)
                                            | (_, Env.DefdBeforeUse) => VectorExt.empty ()
                                            | (_, _) => raise Fail "unreachable")
                                          (Env.locals env')
                in VectorExt.concat boxDefs stmts'
                end
            val stmts' = elabStmts Env.empty stmts
        in
            { procs = Vector.fromList (StringHashTable.listItems procs)
            , main = stmts' }
        end
end (* structure ConvertLEnv *)
