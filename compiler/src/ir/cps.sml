structure NameHashSet = HashSetFn(type hash_key = Name.t
                                  val hashVal = Name.hash
                                  val sameKey = op=)

structure Cps : sig
    structure Expr : ANF_EXPR
    structure Stmt : ANF_STMT

    structure Transfer : sig
        datatype t = Continue of Pos.t * Name.t * Expr.Triv.t vector
                   | Call of Pos.t * Name.t * Expr.Triv.t * Expr.Triv.t vector
                   | Branch of Pos.t * Expr.Triv.t * Name.t * Name.t
                   | Halt of Pos.t * Expr.Triv.t

        val successors : t -> Name.t vector

        val toDoc : t -> PPrint.doc
    end

    structure Cont : sig
        type t = { args: Argv.t
                 , block: (Transfer.t, Stmt.t) Block.t }

        val successors : t -> Name.t vector

        val toDoc : t -> PPrint.doc
    end

    structure Cfg : sig
        type t = { entry: Name.t
                 , conts: Cont.t NameMap.map }

        val controlEdges : t -> (Name.t * Name.t) vector
        val parentsOf : t -> Name.t -> Name.t vector
        val reversePostorder : t -> (Name.t * int vector) vector

        val toDoc : t -> PPrint.doc

        structure Builder : sig
            type builder

            val empty : unit -> builder
            val contains : builder -> Name.t -> bool
            val insert : builder * Name.t * Cont.t -> unit
            val assocArgs : builder * Name.t * Argv.t -> unit
            val prependStmts : builder * Name.t * Stmt.t vector -> unit
            val genCaseFail : builder -> Pos.t -> Name.t
            val genGuardFail : builder -> Pos.t -> Name.t
            val build : builder -> Name.t -> t
        end
    end

    structure DominatorTree : sig
        datatype t = Branch of Name.t * t vector
                   | Leaf of Name.t

        val ofCfg : Cfg.t -> t
        val toDoc : t -> PPrint.doc
    end

    type proc = { pos: Pos.t
                , name: Name.t
                , clovers: Name.t vector
                , cfg: Cfg.t }

    type program = { procs: proc NameMap.map
                   , main: Cfg.t }

    val toDoc : program -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = Anf.Expr
    structure Stmt = Anf.Stmt

    structure Transfer = struct
        datatype t = Continue of Pos.t * Name.t * Expr.Triv.t vector
                   | Call of Pos.t * Name.t * Expr.Triv.t * Expr.Triv.t vector
                   | Branch of Pos.t * Expr.Triv.t * Name.t * Name.t
                   | Halt of Pos.t * Expr.Triv.t

        val successors =
            fn Continue (_, succ, _) => Vector.fromList [succ]
             | Call (_, succ, _, _) => Vector.fromList [succ]
             | Branch (_, _, succ1, succ2) => Vector.fromList [succ1, succ2]
             | Halt _ => Vector.fromList []

        val toDoc =
            fn Continue (pos, label, args) =>
               Name.toDoc label ^^
                   PP.parens (PP.punctuate (PP.text "," ^^ PP.space)
                                           (Vector.map Expr.Triv.toDoc args))
             | Call (pos, label, f, args) =>
               let val lExpr = Expr.Triv.Var (FlatVar1.Label label)
               in Expr.toDoc (Expr.Call (pos, f, VectorExt.prepend args lExpr))
               end
             | Branch (_, cond, conseq, alt) =>
               PP.parens (Expr.Triv.toDoc cond <+> PP.text "?" <+>
                              Name.toDoc conseq <+> PP.text "|" <+> Name.toDoc alt) ^^
                   PP.parens (PP.empty)
             | Halt (_, v) => PP.text "__halt" ^^ PP.parens (Expr.Triv.toDoc v)
    end

    structure Cont = struct
        type t = { args: Argv.t
                 , block: (Transfer.t, Stmt.t) Block.t }

        fun successors { args = _, block = (_, transfer) } = Transfer.successors transfer

        fun toDoc { args = args, block = block } =
            Argv.toDoc args <+> PP.lBrace ^^
                PP.nest 4 (PP.line ^^ Block.toDoc Transfer.toDoc Stmt.toDoc block) <$> PP.rBrace
    end

    structure Cfg = struct
        type t = { entry: Name.t
                 , conts: Cont.t NameMap.map }

        fun controlEdges { entry = _, conts = conts } =
            let val contVec = Vector.fromList (NameMap.listItemsi conts)
                fun succEdges (label, cont) =
                    Vector.map (fn succ => (label, succ)) (Cont.successors cont)
            in VectorExt.flatMap succEdges contVec
            end

        fun parentsOf cfg label =
            Vector.map #2 (VectorExt.filter (fn (src, dest) => dest = label) (controlEdges cfg))

        fun reversePostorder { entry = entry, conts = conts } =
            let val res = ref []

                val parents = NameHashTable.mkTable (0, Subscript)
                val isVisited = NameHashTable.inDomain parents
                fun insertParents clabel plabels' =
                    let val plabels = case NameHashTable.find parents clabel
                                      of SOME plabels => plabels' @ plabels
                                       | NONE => plabels'
                    in NameHashTable.insert parents (clabel, plabels)
                    end

                fun walk parents label =
                    let val _ = insertParents label parents
                        fun visitChild parents label =
                            if isVisited label
                            then insertParents label parents
                            else walk parents label
                    in case NameMap.find (conts, label)
                       of SOME cont => Vector.app (visitChild [label]) (Cont.successors cont)
                        | NONE => ()
                     ; res := label :: !res
                    end

                val _ = walk [] entry
                val labels = Vector.fromList (!res)
                fun indexOf label = #1 (valOf (Vector.findi (fn (_, l) => l = label) labels))
            in Vector.map (fn label =>
                              (label,
                               Vector.map indexOf
                                          (Vector.fromList (NameHashTable.lookup parents label))))
                          labels
            end

        fun toDoc (cfg as { entry = entry, conts = conts }) =
            let fun pairToDoc (label, cont) =
                    PP.line ^^ Name.toDoc label <+> Cont.toDoc cont
                val contsDoc =
                    PP.punctuate PP.line
                        (Vector.map pairToDoc (Vector.fromList (NameMap.listItemsi conts)))
            in Name.toDoc entry <+> PP.lBrace ^^ PP.nest 4 contsDoc <$> PP.rBrace
            end

        structure Builder = struct
            type builder = (Cont.t NameMap.map ref * Name.t option ref)

            fun empty () = (ref NameMap.empty, ref NONE)

            fun contains (builder : builder) label = NameMap.inDomain (!(#1 builder), label)

            fun insert (builder : builder, label, cont) =
                #1 builder := NameMap.insert (!(#1 builder), label, cont)

            fun assocArgs (builder: builder, label, args) =
                let val block = #block (NameMap.lookup (!(#1 builder), label))
                    val cont' = { args = args, block = block }
                in #1 builder := NameMap.insert (!(#1 builder), label, cont')
                end

            fun prependStmts (builder : builder, label, stmts') =
                let val { args = args, block = (stmts, transfer) } =
                        NameMap.lookup (!(#1 builder), label)
                    val cont' = { args = args
                                , block = (VectorExt.concat stmts' stmts, transfer) }
                in #1 builder := NameMap.insert (!(#1 builder), label, cont')
                end

            fun genCaseFail pos builder = Name.freshFromString "fail" (* FIXME *)

            fun genGuardFail pos builder = Name.freshFromString "fail" (* FIXME *)

            fun build (builder : builder) entry = { entry = entry, conts = ! (#1 builder) }
        end
    end

    structure DominatorTree = struct
        datatype t = Branch of Name.t * t vector
                   | Leaf of Name.t

        structure Builder = struct
            type builder = (Name.t * int vector * int list) vector

            val rootIndex = 0

            val new = Vector.mapi (fn (0, (label, parents)) => (label, parents, [rootIndex])
                                    | (i, (label, parents)) => (label, parents, []))

            fun childIdxs builder ni =
                let val cidxs = VectorExt.Builder.empty ()
                    fun loop i =
                        if i < Vector.length builder
                        then ( if hd (#3 (Vector.sub (builder, i))) = ni
                               then VectorExt.Builder.append cidxs i
                               else ()
                             ; loop (i + 1) )
                        else VectorExt.Builder.build cidxs
                in loop (ni + 1)
                end

            fun updateOne (builder : builder) changed (label, pidxs, doms) =
                let fun intersect (xs as x :: xtail) (ys as y :: ytail) =
                        case Int.compare (x, y)
                        of LESS => intersect xtail ys
                         | GREATER => intersect xs ytail
                         | EQUAL => xs
                    fun sameSet (x :: _) (y :: _) = x = y
                      | sameSet [] [] = true
                      | sameSet _ _ = false
                    fun step (pi, acc) =
                        case (#3 (Vector.sub (builder, pi)), acc)
                        of (tail, NONE) => SOME (pi :: tail)
                         | ([], acc) => acc
                         | (tail, SOME doms) => SOME (intersect (pi :: tail) doms)
                    val doms' = valOf (Vector.foldl step NONE pidxs)
                in if not (sameSet doms doms') then changed := true else ()
                 ; (label, pidxs, doms')
                end

            fun update builder =
                let val changed = ref false
                    val builder' =
                        Vector.mapi (fn (i, entry) =>
                                        if i <> rootIndex
                                        then updateOne builder changed entry
                                        else entry)
                                    builder
                in (!changed, builder')
                end

            fun build (builder : builder) =
                let fun buildNode ni =
                        let val name = #1 (Vector.sub (builder, ni))
                            val children = Vector.map buildNode (childIdxs builder ni)
                        in if Vector.length children = 0
                           then Leaf name
                           else Branch (name, children)
                        end
                in buildNode rootIndex
                end
        end

        fun ofCfg (cfg as { entry = entry, conts = conts }) =
            let val rpo = Cfg.reversePostorder cfg
                fun iterate (changed, builder) =
                    if changed
                    then iterate (Builder.update builder)
                    else builder
            in Builder.build (iterate (true, Builder.new rpo))
            end

        val rec toDoc =
            fn Branch (name, children) =>
               PP.parens (Name.toDoc name <+> (PP.punctuate PP.space (Vector.map toDoc children)))
             | Leaf name => Name.toDoc name
    end

    type proc = { pos: Pos.t
                , name: Name.t
                , clovers: Name.t vector
                , cfg: Cfg.t }

    type program = { procs: proc NameMap.map
                   , main: Cfg.t }

    fun procToDoc { pos = _, name = name, clovers = clovers, cfg = cfg } =
        let val nameDoc = Name.toDoc name
            val cloversDoc = PP.braces (PP.punctuate (PP.text ", ") (Vector.map Name.toDoc clovers))
            val cfgDoc = Cfg.toDoc cfg
            val rpo = Cfg.reversePostorder cfg
        in nameDoc ^^ cloversDoc <+> cfgDoc
        end

    fun toDoc { procs = procs, main = main } =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in NameMap.foldl step PP.empty procs <$> Cfg.toDoc main
        end
end
