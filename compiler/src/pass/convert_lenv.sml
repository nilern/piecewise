structure NameSet = BinarySetFn(type ord_key = Name.t
                                val compare = Name.compare)
structure NameHashTable = HashTableFn(type hash_key = Name.t
                                      val hashVal = Name.hash
                                      val sameKey = op=)

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

        val use : int -> t -> Name.t -> Name.t res option
        val define : t -> Name.t -> (Name.t * status) res option
        val self : t -> Name.t option
        val self0 : t -> Name.t option
        val params : t -> Name.t option
        val params0 : t -> Name.t option
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

            val contains : t -> Name.t -> bool
            val findAndTransition : (status -> status) -> t -> Name.t -> (Name.t * status) option
            val self : t -> Name.t option
            val self0 : t -> Name.t option
            val params : t -> Name.t option
            val params0 : t -> Name.t option
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
                fun newFn self params = Fn ( self
                                            , Name.fresh self
                                            , params
                                            , Name.fresh params
                                            , NameHashTable.mkTable (0, Subscript)
                                            , ref 0 )
                fun newCase names = Case (newBindings names)
                fun newBlock names = Block (newBindings names)
            end

            fun contains (Fn (self, _, params, _, _, _)) key =
                if key = self then true
                else if key = params then true
                else false
              | contains (Case bs) key = NameHashTable.inDomain bs key
              | contains (Block bs) key = NameHashTable.inDomain bs key

            local
                fun performTransition transition bs key (name, status) =
                    let val entry' = (name, transition status)
                    in
                        NameHashTable.insert bs (key, entry');
                        entry'
                    end
            in
                fun findAndTransition transition (Fn (selfStr, self, params, params', _, _)) key =
                    if key = selfStr then SOME (self, DefdBeforeUse)
                    else if key = params then SOME (params', DefdBeforeUse)
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
                val params = fnProp #4
                val params0 = fnProp #3
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

            fun locals (Fn (_, self, _, params, _, _)) =
                [(self, DefdBeforeUse), (params, DefdBeforeUse)]
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

            fun toString (Fn (self, self', params, params', _, _)) =
                "Fn " ^ Name.toString self ^ ": " ^  Name.toString self' ^ ", "
                      ^ Name.toString params ^ ": " ^  Name.toString params'
              | toString (Case bs) = "Case " ^ bindingsToString bs
              | toString (Block bs) = "Block " ^ bindingsToString bs
        end (* structure Frame *)

        type t = Frame.t list
        datatype 'a res = Direct of 'a
                        | Clover of 'a * int

        val empty = []
        fun pushFnFrame env self params = Frame.newFn self params :: env
        fun pushCaseFrame env names = Frame.newCase names :: env
        fun pushBlockFrame env names = Frame.newBlock names :: env

        local
            fun findNameAndTransition skip transition (frame :: env') key =
                if Frame.contains frame key
                then if skip = 0
                     then Frame.findAndTransition transition frame key
                     else findNameAndTransition (skip - 1) transition env' key
                else findNameAndTransition skip transition env' key
              | findNameAndTransition _ _ [] _ = NONE
            fun findCloverAndTransition skip transition caller env key =
                let fun newClover (name, status) =
                        Clover ((Frame.ffName caller, status), Frame.index caller name)
                in Option.map newClover (findNameAndTransition skip transition env key)
                end
        in
            fun findAndTransition skip transition (frame :: env') key =
                if Frame.contains frame key
                then if skip = 0
                     then Option.map Direct (Frame.findAndTransition transition frame key)
                     else findAndTransition (skip - 1) transition env' key
                else (case frame
                           of Frame.Block _ => findAndTransition skip transition env' key
                            | Frame.Case _ => findAndTransition skip transition env' key
                            | Frame.Fn fnFrame =>
                              findCloverAndTransition skip transition fnFrame env' key)
              | findAndTransition _ _ [] _ = NONE

            fun use skip env name =
                let val transition = fn Pristine => Used
                                      | Used => Used
                                      | DefdBeforeUse => DefdBeforeUse
                                      | UsedBeforeDef => UsedBeforeDef
                in
                    Option.map (fn Direct (name, _) => Direct name
                                 | Clover ((self, _), i) => Clover (self, i))
                               (findAndTransition skip transition env name)
                end

            val define = findAndTransition 0 (fn Pristine => DefdBeforeUse
                                               | Used => UsedBeforeDef
                                               | DefdBeforeUse => DefdBeforeUse
                                               | UsedBeforeDef => UsedBeforeDef)
        end

        val self = ListExt.some Frame.self
        val self0 = ListExt.some Frame.self0
        val params = ListExt.some Frame.params
        val params0 = ListExt.some Frame.params0
        val clovers = ListExt.some Frame.clovers

        val locals = fn frame :: _ => Vector.fromList (Frame.locals frame)
                      | [] => VectorExt.empty ()

        fun toString frames =
            List.foldl (fn (f, acc) => acc ^ Frame.toString f ^ "\n") "" frames ^ "\n"
    end (* structure Env *)

    structure Expr = FlatAst0.Expr
    structure Stmt = FlatAst0.Stmt
    structure Triv = Expr.Triv
    structure Var = FlatVar0

    structure AAExpr = AuglessAst.Expr
    structure AAStmt = AuglessAst.Stmt
    structure AATriv = AAExpr.Triv

    val FixE = FlatAst0.FixE
    val FixS = FlatAst0.FixS
    val Block = Expr.Block
    val Call = Expr.Call
    val PrimCall = Expr.PrimCall
    val Triv = Expr.Triv
    val Def = Stmt.Def
    val Guard = Stmt.Guard
    val Expr = Stmt.Expr
    val Var = Triv.Var
    val Const = Triv.Const

    exception Unbound of Pos.t * Name.t

    (* MAYBE: bindBindings = foldBindStmts (fn ...)
              stmtVecBindings = foldStmtVecBindStmts (fn ...) *)

    fun stmtVecBindings stmts =
        let fun stmtBindings (AuglessAst.FixS stmt, names) =
                case stmt
                of AAStmt.Def (_, BaseVar.Lex name, _) => NameSet.add (names, name)
                 | AAStmt.Def (_, BaseVar.Dyn _, _) => names
                 | AAStmt.Guard _ => names
                 | AAStmt.Expr _ => names
        in Vector.foldl stmtBindings NameSet.empty stmts
        end

    fun elabExpr procs env (AuglessAst.FixE expr) =
        FixE (case expr
              of AAExpr.Fn (pos, name, params, cases) =>
                 let fun elabCase env (AuglessAst.Prolog (cond, bindStmts), body) =
                         let val env' = Env.pushCaseFrame env (stmtVecBindings bindStmts)
                             val cond' = DNF.map (elabExpr procs env') cond
                             val (bindStmts', body') = elabBlock procs env' (bindStmts, body)
                         in ((cond', bindStmts'), body')
                         end
                     val name = OptionExt.mapOrElse (fn () => Name.fromString "f")
                                                    (Name.fromString o RVar.toString) name
                     val env' = Env.pushFnFrame env name params
                     val name = valOf (Env.self env')
                     val cases' = Vector.map (elabCase env') cases
                     val clovers = Option.valOf (Env.clovers env')
                     val proc = { pos = pos
                                , name = name
                                , clovers = clovers
                                , args = { names = Vector.fromList [ valOf (Env.self env')
                                                                   , Name.freshFromString "m"
                                                                   , valOf (Env.params env') ]
                                         , types = Vector.fromList [ Type.Fn
                                                                   , Type.Int
                                                                   , Type.Any ] }
                                , cases = cases' }
                     val _ = NameHashTable.insert procs (name, proc)
                     val cexprs = (* HACK: *)
                         Vector.map (fn name =>
                                        AuglessAst.FixE
                                            (AAExpr.Triv (pos,
                                                   AATriv.Var (RVar.Current (BaseVar.Lex
                                                               (Name.fromString
                                                                (Name.chars name)))))))
                                     clovers
                     val close = elabExpr procs env
                                          (AuglessAst.FixE
                                              (AAExpr.PrimCall (pos, Primop.Close, cexprs)))
                 in
                     case close (* HACK *)
                     of FlatAst0.FixE (Expr.PrimCall (pos, Primop.Close, cexprs)) =>
                        let val fnPtr = FixE (Triv (pos, Var (Var.Label name)))
                        in PrimCall (pos, Primop.Close, VectorExt.prepend cexprs fnPtr)
                        end
                      | _ => raise Fail "unreachable"
                 end
               | AAExpr.Block (pos, block as (stmts, _)) =>
                 let val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
                 in Block (pos, elabBlock procs env' block)
                 end
               | AAExpr.Call (pos, f, args) =>
                 let val f' = elabExpr procs env f
                     val m = FixE (Triv (pos, Const (Const.Int (Int.toLarge 0))))
                     val fnPtr =
                         FixE (PrimCall (AuglessAst.exprPos f, Primop.FnPtr,
                                         Vector.fromList [f', m]))
                     val args' =
                         VectorExt.concat
                             (Vector.fromList [f'
                                              , FixE (Triv (pos,
                                                            Const (Const.Int (Int.toLarge 0))))])
                             (Vector.map (elabExpr procs env) args)
                 in Call (pos, fnPtr, args')
                 end
               | AAExpr.PrimCall (pos, po, args) =>
                 PrimCall (pos, po, Vector.map (elabExpr procs env) args)
               | AAExpr.Triv (pos, AATriv.Var (var as (RVar.Current (BaseVar.Dyn _)))) =>
                 Triv (pos, Var (FlatVar0.Data var))
               | AAExpr.Triv (pos, AATriv.Var (var as (RVar.Upper (BaseVar.Dyn _)))) =>
                 Triv (pos, Var (FlatVar0.Data var))
               | AAExpr.Triv (pos, AATriv.Var var) =>
                 let val (name, skip) = case var
                                        of RVar.Current (BaseVar.Lex name) => (name, 0)
                                         | RVar.Upper (BaseVar.Lex name) => (name, 1)
                                         | _ => raise Fail "unreachable"
                 in case Env.use skip env name
                    of SOME (Env.Direct name) =>
                       Triv (pos, Var (FlatVar0.Data (RVar.Current (BaseVar.Lex name))))
                     | SOME (Env.Clover (self, i)) =>
                       let val selfVar = Var (FlatVar0.Data (RVar.Current (BaseVar.Lex self)))
                           val index = Const (Const.Int (Int.toLarge i))
                       in PrimCall ( pos
                                  , Primop.FnGet
                                  , Vector.fromList [ FixE (Triv (pos, selfVar))
                                                    , FixE (Triv (pos, index)) ] )
                       end
                     | NONE => raise Unbound (pos, name)
                end
              | AAExpr.Triv (pos, AATriv.Const c) => Triv (pos, Const c))

    and elabStmt procs env (AuglessAst.FixS stmt) =
        FixS (case stmt
              of AAStmt.Def (pos, var as (BaseVar.Lex name), expr) =>
                 let val expr' = elabExpr procs env expr
                     val SOME (Env.Direct (name', status')) = Env.define env name
                 in
                     case status'
                     of Env.UsedBeforeDef =>
                        let val pos = FlatAst0.Expr.pos (FlatAst0.unwrapE expr')
                            val varExpr =
                                FixE (Triv (pos,
                                            Var (FlatVar0.Data (RVar.Current (BaseVar.Lex name')))))
                            val assign =
                                FixE (PrimCall (pos, Primop.BSet,
                                               Vector.fromList [varExpr, expr']))
                        in Expr assign
                        end
                      | Env.DefdBeforeUse =>
                        Def (pos, BaseVar.Lex name', expr')
                      | _ => raise Fail "unreachable"
                 end
               | AAStmt.Def (pos, var as (BaseVar.Dyn name), expr) =>
                 Def (pos, BaseVar.Dyn name, elabExpr procs env expr)
               | AAStmt.Guard (pos, dnf) =>
                 Guard (pos, DNF.map (elabExpr procs env) dnf)
               | AAStmt.Expr expr => Expr (elabExpr procs env expr))

    and elabBlock procs env (block as (stmts, expr)) =
        let val stmts' = Vector.map (elabStmt procs env) stmts
            val pos = AuglessAst.blockPos block
            val boxAlloc = FixE (PrimCall (pos, Primop.Box, VectorExt.empty ()))
            fun newBoxDef name =
                FixS (Def (pos, BaseVar.Lex name, boxAlloc))
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
        let val procs = NameHashTable.mkTable (0, Subscript)
            val env = Env.pushBlockFrame Env.empty (stmtVecBindings stmts)
            val block' = elabBlock procs env block
        in
            { procs = NameHashTable.foldi (fn (name, proc, map) => NameMap.insert (map, name, proc))
                                          NameMap.empty procs
            , main = block' }
        end
end (* structure ConvertLEnv *)
