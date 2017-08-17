structure Cps : sig
    structure Expr : ANF_EXPR
    structure Stmt : ANF_STMT

    structure Transfer : sig
        datatype t = Continue of Pos.t * Name.t * Expr.Triv.t vector
                   | Call of Pos.t * Name.t * Expr.Triv.t * Expr.Triv.t vector
                   | Branch of Pos.t * Expr.Triv.t * Name.t * Name.t

        val toDoc : t -> PPrint.doc
    end

    structure Cont : sig
        type t = { args: Argv.t
                 , block: (Transfer.t, Stmt.t) Block.t }

        val toDoc : t -> PPrint.doc
    end

    structure Cfg : sig
        type t = { entry: Name.t
                 , conts: Cont.t NameMap.map }

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
    end

    structure Cont = struct
        type t = { args: Argv.t
                 , block: (Transfer.t, Stmt.t) Block.t }

        fun toDoc { args = args, block = block } =
            Argv.toDoc args <+> PP.lBrace ^^
                PP.nest 4 (PP.line ^^ Block.toDoc Transfer.toDoc Stmt.toDoc block) <$> PP.rBrace
    end

    structure Cfg = struct
        type t = { entry: Name.t
                 , conts: Cont.t NameMap.map }

        fun toDoc { entry = entry, conts = conts } =
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
        in nameDoc ^^ cloversDoc <+> cfgDoc
        end

    fun toDoc { procs = procs, main = main } =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in NameMap.foldl step PP.empty procs <$> Cfg.toDoc main
        end
end
