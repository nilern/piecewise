structure Anf : sig
    structure Expr : ANF_EXPR
    structure Stmt : ANF_STMT
    structure Argv : TO_DOC

    structure ValExpr : sig
        datatype t = Triv of Pos.t * Expr.Triv.t
                   | Guard of Pos.t * Label.t DNF.t * Label.t

        val toDoc : t -> PPrint.doc
    end

    type block = (ValExpr.t, Stmt.t) Block.t

    structure Cfg : sig
        type t = { entry: Label.t
                 , blocks: block LabelMap.map }

        structure Builder : sig
            type builder

            val empty : Label.t -> builder
            val insert : builder * Label.t * block -> unit
            val build : builder -> t
        end
    end

    type procCase = { cond: Label.t DNF.t
                    , cfg: Cfg.t }

    type proc = { name: Name.t
                , clovers: Name.t vector
                , args: Argv.t
                , cases: procCase vector }

    type program = { procs: proc NameMap.map
                   , main: Cfg.t }

    val toDoc : program -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Expr = AnfExpr
    structure Stmt = AnfStmt

    structure Argv = Argv1

    structure ValExpr = struct
        datatype t = Triv of Pos.t * Expr.Triv.t
                   | Guard of Pos.t * Label.t DNF.t * Label.t

        val toDoc =
            fn Triv (_, t) => Expr.Triv.toDoc t
             | Guard (_, dnf, dest) =>
               PP.text "@guard" <+> DNF.toDoc Label.toDoc dnf <+> PP.text "->" <+> Label.toDoc dest
    end

    type block = (ValExpr.t, Stmt.t) Block.t

    structure Cfg = struct
        type t = { entry: Label.t
                 , blocks: block LabelMap.map }

        structure Builder = struct
            type builder = Label.t * block LabelMap.map ref

            fun empty entry = (entry, ref LabelMap.empty)

            fun insert ((_, blocks), label, block) =
                blocks := LabelMap.insert (!blocks, label, block)

            fun build (entry, blocks) = { entry = entry, blocks = !blocks }
        end
    end

    type procCase = { cond: Label.t DNF.t
                    , cfg: Cfg.t }

    type proc = { name: Name.t
                , clovers: Name.t vector
                , args: Argv.t
                , cases: procCase vector }

    type program = { procs: proc NameMap.map
                   , main: Cfg.t }

    fun cfgToDoc { entry = entry, blocks = blocks } =
        let fun pairToDoc (label, block) =
                (if label = entry
                 then PP.text "-> " ^^ Label.toDoc label
                 else Label.toDoc label) ^^ PP.text ": " ^^
                    PP.align (Block.toDoc ValExpr.toDoc Stmt.toDoc block)
        in PP.punctuate (PP.line ^^ PP.line)
                        (Vector.map pairToDoc (Vector.fromList (LabelMap.listItemsi blocks)))
        end

    fun caseToDoc { cond = cond, cfg = cfg } =
        PP.text "case" <+> DNF.toDoc Label.toDoc cond ^^ PP.text ":" <$> cfgToDoc cfg

    fun procToDoc { name = name, clovers = clovers, args = args, cases = cases } =
        let val nameDoc = Name.toDoc name
            val cloversDoc = PP.braces (PP.punctuate (PP.text ", ") (Vector.map Name.toDoc clovers))
            val argsDoc = Argv.toDoc args
            val casesDoc = PP.punctuate (PP.semi ^^ PP.line) (Vector.map caseToDoc cases)
        in
            nameDoc ^^ cloversDoc ^^ argsDoc <+> PP.text "=" <+> PP.lBrace <$>
                PP.nest 4 casesDoc <$> PP.rBrace
        end

    fun toDoc { procs = procs, main = main } =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in NameMap.foldl step PP.empty procs <$> cfgToDoc main
        end
end
