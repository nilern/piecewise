signature CONT = sig
    structure Expr : CPS_EXPR
    structure ContRef : TO_DOC

    type t = { args: Name.t vector
             , expr: Expr.t
             , succs: ContRef.t vector }

    val pos : t -> Pos.t
    val toDoc : t -> PPrint.doc
end

functor ContFn(structure E : CPS_EXPR structure CR: TO_DOC)
:> CONT where type Expr.t = E.t and type ContRef.t = CR.t = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>

    structure Expr = E
    structure ContRef = CR

    type t = { args: Name.t vector
             , expr: Expr.t
             , succs: ContRef.t vector }

    fun pos (k : t) = Expr.pos (#expr k)

    fun toDoc { args = args, expr = expr, succs = succs } =
        PP.punctuate PP.space (Vector.map Name.toDoc args) ^^ PP.text ":" <+>
            Expr.toDoc expr <+> PP.text "->" <+>
                PP.punctuate (PP.text " | ") (Vector.map ContRef.toDoc succs)
end

structure Cont0 = ContFn(structure E = CpsExpr0 structure CR = ContRef0)

signature CONT_MAP = sig
    type t

    val toDoc : int * t -> PPrint.doc
end

functor ContMapFn(K : CONT) = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>

    structure Map = BinaryMapFn(type ord_key = int val compare = Int.compare)

    type t = K.t Map.map

    val empty = Map.empty
    val insert = Map.insert

    fun toDoc (entry, map) =
        let fun pairToDoc (k, cont) =
                (if k = entry then PP.text "=> " else PP.text "   ") ^^
                    PP.text "k" ^^ PP.brackets (PP.text (Int.toString k)) <+>
                        K.toDoc cont
        in PPrint.punctuate PPrint.line (Vector.map pairToDoc
                                                    (Vector.fromList (Map.listItemsi map)))
        end
end

structure ContMap0 = ContMapFn(Cont0)

structure CpsArgv0 = struct
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>

    type t = {self: Name.t, params: Name.t, denv: Name.t, cont: int}

    fun toDoc {self = self, params = params, denv = denv, cont = cont} =
        PPrint.parens (PPrint.text "self =" <+> Name.toDoc self ^^ PPrint.text "," <+>
                           PPrint.text "params =" <+> Name.toDoc params ^^ PPrint.text "," <+>
                               PPrint.text "denv =" <+> Name.toDoc denv ^^ PPrint.text "," <+>
                                   PPrint.text "cont =" <+> PPrint.text (Int.toString cont))
end

functor CpsFn(structure CM : CONT_MAP
              structure A: TO_DOC) = struct
    structure PP = PPrint
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>
    val op<$> = PPrint.<$>

    structure ContMap = CM
    structure Argv = A

    type proc = { name: Name.t
                , clovers: Name.t vector
                , args: Argv.t
                , cases: (int * ContMap.t) vector }

    type program = { procs: proc NameMap.map
                   , main: int * ContMap.t }

    fun procToDoc {name = name, clovers = clovers, args = args, cases = cases} =
        let val nameDoc = Name.toDoc name
            val cloversDoc = PP.braces (PP.punctuate (PP.text ", ") (Vector.map Name.toDoc clovers))
            val argsDoc = Argv.toDoc args
            val casesDoc = PP.punctuate (PP.semi ^^ PP.line) (Vector.map ContMap.toDoc cases)
        in
            nameDoc ^^ cloversDoc ^^ argsDoc <+> PP.text "=" <+> PP.lBrace <$>
                PP.nest 4 casesDoc <$> PP.rBrace
        end

    fun toDoc { procs = procs, main = main } =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in NameMap.foldl step PP.empty procs <$> ContMap.toDoc main
        end
end

structure Cps0 = CpsFn(structure CM = ContMap0
                       structure A = CpsArgv0)
