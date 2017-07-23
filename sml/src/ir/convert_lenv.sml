structure NameSet = BinarySetFn(type ord_key = Name.t
                                val compare = Name.compare)
structure StringHashTable = HashTableFn(type hash_key = string
                                        val hashVal = HashString.hashString
                                        val sameKey = op=)
structure NameHashTable = HashTableFn(type hash_key = Name.t
                                      val hashVal = Name.hash
                                      val sameKey = op=)

(* OPTIMIZE: self recursion doesn't require a cyclic closure *)
structure ConvertLEnv :> sig
    exception Unbound of Pos.t * Name.t

    val convert : (AuglessAst.expr, AuglessAst.stmt) Block.t -> FlatAst0.program
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
    end = struct
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

            local
                fun fnProp f = fn Fn fn_frame => SOME (f fn_frame)
                                | Case _ => NONE
                                | Block _ => NONE
            in
                val self = fnProp #2
                val self0 = fnProp #1
                val formals = fnProp #4
                val formals0 = fnProp #3
                val clovers =
                    fnProp (fn (_, _, _, _, cis, _) =>
                               let val arr =
                                       Array.tabulate (NameHashTable.numItems cis,
                                                       let val v = Name.Plain "" in fn _ => v end)
                               in
                                   NameHashTable.appi (fn (s, i) => Array.update (arr, i, s)) cis;
                                   Array.vector arr
                               end)
            end

            fun locals (Fn (_, self, _, formals, _, _)) =
                [(self, DefdBeforeUse), (formals, DefdBeforeUse)]
              | locals (Case bs) = NameHashTable.listItems bs
              | locals (Block bs) = NameHashTable.listItems bs

            val statusToString = fn Pristine => "Pristine"
                                  | Used => "Used"
                                  | UsedBeforeDef => "UsedBeforeDef"
                                  | DefdBeforeUse => "DefdBeforeUse"

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

        val self = ListExt.some Frame.self
        val self0 = ListExt.some Frame.self0
        val formals = ListExt.some Frame.formals
        val formals0 = ListExt.some Frame.formals0
        val clovers = ListExt.some Frame.clovers

        val locals = fn frame :: _ => Vector.fromList (Frame.locals frame)
                      | [] => VectorExt.empty ()

        fun toString frames =
            List.foldl (fn (f, acc) => acc ^ Frame.toString f ^ "\n") "" frames ^ "\n"
    end (* structure Env *)

    val FixE = FlatAst0.FixE
    val FixS = FlatAst0.FixS
    val Block = FlatAst0.Expr.Block
    val PrimApp = FlatAst0.Expr.PrimApp
    val Triv = FlatAst0.Expr.Triv
    val Def = FlatAst0.Stmt.Def
    val Guard = FlatAst0.Stmt.Guard
    val Expr = FlatAst0.Stmt.Expr
    val Var = FlatTriv0.Var
    val Const = FlatTriv0.Const

    exception Unbound of Pos.t * Name.t

    (* MAYBE: bindBindings = foldBindStmts (fn ...)
              stmtVecBindings = foldStmtVecBindStmts (fn ...) *)

    fun stmtVecBindings stmts =
        let fun stmtBindings (AuglessAst.FixS stmt, names) =
                case stmt
                of AuglessStmt.Def (_, (CTag.Lex, name), _) => NameSet.add (names, name)
                 | AuglessStmt.Def (_, (CTag.Dyn, _), _) => names
                 | AuglessStmt.Guard _ => names
                 | AuglessStmt.Expr _ => names
        in Vector.foldl stmtBindings NameSet.empty stmts
        end

    fun elabExpr procs env (AuglessAst.FixE expr) =
        FixE (case expr
              of Expr.Fn (pos, name, formals, cases) =>
                 let fun elabCase env (cas as (prologue, _)) =
                         let val env' = Env.pushCaseFrame env (stmtVecBindings prologue)
                             val (prologue', body') = elabBlock procs env' cas
                         in
                             ( Option.valOf (Env.self env)
                             , valOf (Env.formals env')
                             , Name.freshFromString "denv"
                             , prologue'
                             , body' )
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
                     val cexprs = (* HACK: *)
                         Vector.map (fn name =>
                                        AuglessAst.FixE
                                            (Expr.Triv (pos,
                                                   ATriv.Var (ATag.Lex,
                                                              Name.fromString
                                                               (Name.chars name)))))
                                     clovers
                     val close = elabExpr procs env
                                          (AuglessAst.FixE
                                              (Expr.PrimApp (pos, Primop.Close,
                                                             cexprs)))
                 in
                     case close (* HACK *)
                     of FlatAst0.FixE
                            (FlatAst0.Expr.PrimApp (pos, Primop.Close, cexprs)) =>
                        let val fnPtr =
                                FixE (Triv (pos, Var (FlatTag0.Label, name)))
                        in PrimApp (pos, Primop.Close,
                                    VectorExt.prepend cexprs fnPtr)
                        end
                      | _ => raise Fail "unreachable"
                 end
               | Expr.Block (pos, block as (stmts, _)) =>
                 let val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
                 in Block (pos, elabBlock procs env' block)
                 end
               | Expr.PrimApp (pos, po, args) =>
                 (* MAYBE: add the self-closure arg to App:s *)
                 PrimApp (pos, po, Vector.map (elabExpr procs env) args)
               | Expr.Triv (pos, ATriv.Var (ATag.Lex, name)) =>
                 (case Env.use env name
                  of SOME (Env.Direct name) => Triv (pos, Var (FlatTag0.Lex, name))
                   | SOME (Env.Clover (self, i)) =>
                     let val selfVar = Var (FlatTag0.Lex, self)
                         val index = Const (Const.Int (Int.toString i))
                     in PrimApp ( pos
                                , Primop.FnGet
                                , Vector.fromList [ FixE (Triv (pos, selfVar))
                                                  , FixE (Triv (pos, index)) ] )
                     end
                   | NONE => raise Unbound (pos, name))
              | Expr.Triv (pos, ATriv.Var (var as (ATag.Dyn, _))) =>
                Triv (pos, Var (valOf (FlatVar0.fromAVar var)))
              (* TODO: ATag.Upper* *)
              | Expr.Triv (pos, ATriv.Const c) => Triv (pos, Const c))

    and elabStmt procs env (AuglessAst.FixS stmt) =
        FixS (case stmt
              of AuglessStmt.Def (pos, var as (CTag.Lex, name), expr) =>
                 let val expr' = elabExpr procs env expr
                     val SOME (Env.Direct (name', status')) = Env.define env name
                 in
                     case status'
                     of Env.UsedBeforeDef =>
                        let val pos = FlatAst0.Expr.pos (FlatAst0.unwrapE expr')
                            val varExpr =
                                FixE (Triv (pos, Var (FlatTag0.Lex, name')))
                            val assign =
                                FixE (PrimApp (pos, Primop.BSet,
                                               Vector.fromList [varExpr, expr']))
                        in Expr assign
                        end
                      | Env.DefdBeforeUse =>
                        Def (pos, (CTag.Lex, name'), expr')
                      | _ => raise Fail "unreachable"
                 end
               | AuglessStmt.Def (pos, var as (CTag.Dyn, name), expr) =>
                 Def (pos, (CTag.Dyn, name), elabExpr procs env expr)
               | AuglessStmt.Guard (pos, dnf) =>
                 Guard (pos, DNF.map (elabExpr procs env) dnf)
               | AuglessStmt.Expr expr => Expr (elabExpr procs env expr))

    and elabBlock procs env (block as (stmts, expr)) =
        let val stmts' = Vector.map (elabStmt procs env) stmts
            val pos = AuglessAst.blockPos block
            val boxAlloc = FixE (PrimApp (pos, Primop.Box, VectorExt.empty ()))
            fun newBoxDef name =
                FixS (Def (pos, (CTag.Lex, name), boxAlloc))
            val boxDefs =
                VectorExt.flatMap (fn (name, Env.UsedBeforeDef) =>
                                      VectorExt.singleton (newBoxDef name)
                                    | (_, Env.DefdBeforeUse) => VectorExt.empty ()
                                    | (_, _) => raise Fail "unreachable")
                                  (Env.locals env)
            val expr' = elabExpr procs env expr
        in (VectorExt.concat boxDefs stmts', expr')
        end

    fun convert (block as (stmts, _)) =
        let val procs = StringHashTable.mkTable (0, Subscript)
            val env = Env.pushBlockFrame Env.empty (stmtVecBindings stmts)
            val block' = elabBlock procs env block
        in
            { procs = Vector.fromList (StringHashTable.listItems procs)
            , main = block' }
        end
end (* structure ConvertLEnv *)
