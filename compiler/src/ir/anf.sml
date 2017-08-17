signature ANF_EXPR = sig
    structure Triv : TRIV

    datatype t = Call of Pos.t * Triv.t * Triv.t vector
               | PrimCall of Pos.t * Primop.t * Triv.t vector
               | Triv of Pos.t * Triv.t

    val pos : t -> Pos.t
    val toDoc : t -> PPrint.doc
end

signature ANF_STMT = sig
    structure Expr : ANF_EXPR

    datatype t = Def of Pos.t * Name.t * Type.t * Expr.t
               | Expr of Expr.t

    val pos : t -> Pos.t
    val toDoc : t -> PPrint.doc
end

structure Anf : sig
    structure Expr : ANF_EXPR
    structure Stmt : ANF_STMT

    structure ValExpr : sig
        datatype t = Triv of Pos.t * Expr.Triv.t
                   | Call of Pos.t * Expr.Triv.t * Expr.Triv.t vector
                   | Guard of Pos.t * Name.t DNF.t * Name.t

        val pos : t -> Pos.t
        val toDoc : t -> PPrint.doc
    end

    type block = (ValExpr.t, Stmt.t) Block.t

    val blockPos : block -> Pos.t

    structure Cfg : sig
        type t = { entry: Name.t
                 , blocks: block NameMap.map }

        structure Builder : sig
            type builder

            val empty : Name.t -> builder
            val insert : builder * Name.t * block -> unit
            val build : builder -> t
        end
    end

    type procCase = { cond: Name.t DNF.t
                    , cfg: Cfg.t }

    type proc = { pos: Pos.t
                , name: Name.t
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

    structure Expr = struct
        structure Triv = FlatAst1.Expr.Triv

        datatype t = Call of Pos.t * Triv.t * Triv.t vector
                   | PrimCall of Pos.t * Primop.t * Triv.t vector
                   | Triv of Pos.t * Triv.t

        val pos = fn Call (pos, _, _) => pos
                   | PrimCall (pos, _, _) => pos
                   | Triv (pos, _) => pos

        val toDoc =
            fn Call (_, f, args) =>
               Triv.toDoc f ^^ PP.parens (PP.punctuate (PP.text "," ^^ PP.space)
                                                       (Vector.map Triv.toDoc args))
             | PrimCall (_, po, args) =>
               Primop.toDoc po ^^ PP.parens (PP.punctuate (PP.text "," ^^ PP.space)
                                                          (Vector.map Triv.toDoc args))
             | Triv (_, t) => Triv.toDoc t
    end

    structure Stmt = struct
        structure Expr = Expr

        datatype t = Def of Pos.t * Name.t * Type.t * Expr.t
                   | Expr of Expr.t

        val pos =
            fn Def (pos, _, _, _) => pos
             | Expr expr => Expr.pos expr

        val toDoc =
            fn Def (_, name, ty, expr) =>
               Name.toDoc name ^^ PP.text ":" <+> Type.toDoc ty <+> PP.text "=" <+>
                   Expr.toDoc expr
             | Expr expr => Expr.toDoc expr
    end

    structure ValExpr = struct
        datatype t = Triv of Pos.t * Expr.Triv.t
                   | Call of Pos.t * Expr.Triv.t * Expr.Triv.t vector
                   | Guard of Pos.t * Name.t DNF.t * Name.t

        val pos = fn Triv (pos, _) => pos
                   | Call (pos, _, _) => pos
                   | Guard (pos, _, _) => pos

        val toDoc =
            fn Triv (_, t) => Expr.Triv.toDoc t
             | Call fields => Expr.toDoc (Expr.Call fields)
             | Guard (_, dnf, dest) =>
               PP.text "@guard" <+> DNF.toDoc Name.toDoc dnf <+> PP.text "->" <+> Name.toDoc dest
    end

    type block = (ValExpr.t, Stmt.t) Block.t

    val blockPos = Block.pos ValExpr.pos Stmt.pos

    structure Cfg = struct
        type t = { entry: Name.t
                 , blocks: block NameMap.map }

        structure Builder = struct
            type builder = Name.t * block NameMap.map ref

            fun empty entry = (entry, ref NameMap.empty)

            fun insert ((_, blocks), label, block) =
                blocks := NameMap.insert (!blocks, label, block)

            fun build (entry, blocks) = { entry = entry, blocks = !blocks }
        end
    end

    type procCase = { cond: Name.t DNF.t
                    , cfg: Cfg.t }

    type proc = { pos: Pos.t
                , name: Name.t
                , clovers: Name.t vector
                , args: Argv.t
                , cases: procCase vector }

    type program = { procs: proc NameMap.map
                   , main: Cfg.t }

    fun cfgToDoc { entry = entry, blocks = blocks } =
        let fun pairToDoc (label, block) =
                (if label = entry
                 then PP.text "-> " ^^ Name.toDoc label
                 else Name.toDoc label) ^^ PP.text ": " ^^
                    PP.align (Block.toDoc ValExpr.toDoc Stmt.toDoc block)
        in PP.punctuate (PP.line ^^ PP.line)
                        (Vector.map pairToDoc (Vector.fromList (NameMap.listItemsi blocks)))
        end

    fun caseToDoc { cond = cond, cfg = cfg } =
        PP.text "case" <+> DNF.toDoc Name.toDoc cond ^^ PP.text ":" <$> cfgToDoc cfg

    fun procToDoc { pos = _, name = name, clovers = clovers, args = args, cases = cases } =
        let val nameDoc = Name.toDoc name
            val cloversDoc = PP.braces (PP.punctuate (PP.text ", ") (Vector.map Name.toDoc clovers))
            val argsDoc = Argv.toDoc args
            val casesDoc = PP.punctuate (PP.semi ^^ PP.line) (Vector.map caseToDoc cases)
        in
            nameDoc ^^ cloversDoc ^^ argsDoc <+> PP.text "=" <+> PP.lBrace <$>
                PP.nest 4 (PP.line ^^ casesDoc) <$> PP.rBrace
        end

    fun toDoc { procs = procs, main = main } =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in NameMap.foldl step PP.empty procs <$> cfgToDoc main
        end
end
