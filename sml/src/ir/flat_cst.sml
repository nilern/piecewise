structure FlatCST = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Env = struct
        structure Bindings = BinaryMapFn(type ord_key = string
                                         val compare = String.compare)
        type bindings = Name.t Bindings.map

        datatype t = Fn of bindings
                   | Block of bindings

        datatype res = Direct of Name.t
                     | Clover of Name.t

        val empty = []
        fun pushFnFrame env bindings = Fn bindings :: env
        fun pushBlockFrame env bindings = Block bindings :: env

        fun lookupWith f (Block bs :: env') key =
            (case Bindings.find (bs, key)
             of SOME name => SOME (f name)
              | NONE => lookupWith f env' key)
          | lookupWith f (Fn bs :: env') key =
            (case Bindings.find (bs, key)
             of SOME name => SOME (f name)
              | NONE => lookupWith Clover env' key)
          | lookupWith _ [] _ = NONE
        val lookup = lookupWith Direct
    end

    exception Unbound of Var.t

    datatype expr = Block of Pos.t * stmt vector
                  | App of Pos.t * expr * expr vector
                  | PrimApp of Pos.t * Primop.t * expr vector
                  | Var of Pos.t * Var.t
                  | Clo of Pos.t * Name.t
                  | Const of Pos.t * Const.t
    and stmt = Def of expr * expr
             | AugDef of expr * expr
             | Expr of expr

    withtype procCase = expr vector * expr option * expr

    type proc = { name: Name.t
                , cases: procCase vector }

    type 'a program = { procs: proc vector, main: 'a }

    fun trivial v = { procs = VectorExt.empty (), main = v }

    fun mapMain f prog = { procs = #procs prog, main = f (#main prog)}

    fun append f (p : 'a program) (q : 'b program) : 'c program =
        { procs = Vector.concat [#procs p, #procs q]
        , main = f (#main p) (#main q) }

    fun map f vs =
        let val vs' = Vector.map f vs
        in
            { procs = VectorExt.flatMap #procs vs'
            , main = Vector.map #main vs' }
        end

    fun flatMap f vs =
        let val vs' = Vector.map f vs
        in
            { procs = VectorExt.flatMap #procs vs'
            , main = VectorExt.flatMap #main vs' }
        end

    fun exprToDoc (Block (_, stmts)) =
        (case Vector.length stmts
         of 1 => PP.braces (stmtsToDoc stmts)
          | _ => PP.lBrace ^^ PP.nest 4 (PP.line ^^ stmtsToDoc stmts) ^^
                     PP.line ^^ PP.rBrace)
      | exprToDoc (App (_, f, args)) =
        let fun step (arg, acc) = acc <+> exprToDoc arg
            val argDocs = Vector.foldl step (exprToDoc f) args
        in
            PP.parens (PP.align argDocs)
        end
      | exprToDoc (PrimApp (_, po, args)) =
        let fun step (arg, acc) = acc <+> exprToDoc arg
            val argDocs = Vector.foldl step (Primop.toDoc po) args
        in
            PP.parens (PP.align argDocs)
        end
      | exprToDoc (Var (_, v)) = Var.toDoc v
      | exprToDoc (Clo (_, name)) = PP.text "^" ^^ Name.toDoc name
      | exprToDoc (Const (_, c)) = Const.toDoc c
    and stmtToDoc (Def (pat, expr)) =
        exprToDoc pat <+> PP.text "=" <+> exprToDoc expr
      | stmtToDoc (AugDef (pat, expr)) =
        exprToDoc pat <+> PP.text "+=" <+> exprToDoc expr
      | stmtToDoc (Expr expr) = exprToDoc expr
    and stmtsToDoc stmts =
        (case Vector.length stmts
         of 1 => stmtToDoc (Vector.sub (stmts, 0))
          | _ => let fun step (stmt, acc) =
                             acc ^^ PP.semi <$> stmtToDoc stmt
                     val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                     val rstmts = VectorSlice.slice(stmts, 1, NONE)
                 in VectorSlice.foldl step stmtDoc rstmts end)

    fun procToDoc {name = name, cases = cases} =
        let fun caseToDoc ((pats, cond, body): procCase) =
                let fun step (pat, acc) = acc <+> exprToDoc pat
                    val patDoc = exprToDoc (Vector.sub (pats, 0))
                    val rpats = VectorSlice.slice(pats, 1, NONE)
                    val patsDoc = VectorSlice.foldl step patDoc rpats
                    val condDoc = case cond
                                  of SOME ce =>
                                         PP.space ^^ PP.text "|" <+>
                                             exprToDoc ce
                                   | NONE => PP.empty
                    val bodyDoc = exprToDoc body
                in
                    (patsDoc ^^ condDoc) <+> PP.text "=>" <+> bodyDoc
                end
        in case Vector.length cases
            of 1 => PP.braces (caseToDoc (Vector.sub (cases, 0)))
             | _ => let fun step (cs, acc) =
                                acc ^^ PP.semi <$> caseToDoc cs
                        val caseDoc = caseToDoc (Vector.sub (cases, 0))
                        val rcases = VectorSlice.slice(cases, 1, NONE)
                        val caseDocs = VectorSlice.foldl step caseDoc rcases
                    in
                        Name.toDoc name <+> PP.text "=" <+> PP.lBrace ^^
                            PP.nest 4 (PP.line ^^ caseDocs) ^^
                                PP.line ^^ PP.rBrace
                    end
        end

    fun toDoc (prog : (stmt vector) program) =
        let fun step (proc, acc) = procToDoc proc ^^ PP.line <$> acc
        in
            Vector.foldl step PP.empty (#procs prog) <$>
                stmtsToDoc (#main prog)
        end

    fun patBindings (CST.Const _) = Env.Bindings.empty
      | patBindings (CST.Var (_, Var.Lex name)) =
        let val cs = Name.toString name
        in Env.Bindings.insert(Env.Bindings.empty, cs, Name.fresh cs)
        end

    fun stmtBindings (CST.Def (pat, _)) = patBindings pat
      | stmtBindings (CST.AugDef (pat, _)) = patBindings pat
      | stmtBindings (CST.Expr _) = Env.Bindings.empty

    fun stmtVecBindings stmts =
        let fun step (stmt, acc) =
                Env.Bindings.unionWith #2 (acc, stmtBindings stmt)
        in Vector.foldl step Env.Bindings.empty stmts
        end

    fun elabPat env (CST.Const (pos, c)) = trivial (Const (pos, c))
      | elabPat env (CST.Var (pos, var as Var.Lex name)) =
        trivial (case Env.lookup env (Name.toString name)
                 of SOME (Env.Direct name) => Var (pos, Var.Lex name)
                  | SOME (Env.Clover name) => Clo (pos, name)
                  | NONE => raise Unbound var)

    and elabExpr env (CST.Fn (pos, cases)) =
        let val name = Name.fresh "f"
            val cprogs = Vector.map (elabCase env) cases
            val cprocs = VectorExt.flatMap #procs cprogs
            val cases' = Vector.map #main cprogs
            val procs = VectorExt.conj cprocs { name = name, cases = cases' }
        in
            { procs = procs
            , main = PrimApp (pos, Primop.Close, VectorExt.empty ()) }
        end
      | elabExpr env (CST.Block (pos, stmts)) =
        mapMain (fn stmts => Block (pos, stmts)) (elabStmts env stmts)
      | elabExpr env (CST.App (pos, f, args)) =
        let fun makeApp f args = App (pos, f, args)
        in append makeApp (elabExpr env f) (map (elabExpr env) args)
        end
      | elabExpr env (CST.PrimApp (pos, po, args)) =
        let fun makePrimApp po args = PrimApp (pos, po, args)
        in append makePrimApp (trivial po) (map (elabExpr env) args)
        end
      | elabExpr _ (CST.Const (pos, c)) = trivial (Const (pos, c))
      | elabExpr env (CST.Var (pos, var as Var.Lex name)) =
        trivial (case Env.lookup env (Name.toString name)
                 of SOME (Env.Direct name) => Var (pos, Var.Lex name)
                  | SOME (Env.Clover name) => Clo (pos, name)
                  | NONE => raise Unbound var)

    and elabCase env (pats, cond, body) =
        let fun step (pat, acc) =
                Env.Bindings.unionWith #2 (acc, patBindings pat)
            val env' = Env.pushFnFrame env
                           (Vector.foldl step Env.Bindings.empty pats)
            val pprogs = Vector.map (elabPat env') pats
            val pprocs = VectorExt.flatMap #procs pprogs
            val pats' = Vector.map #main pprogs
            val bodyProg = elabExpr env' body
        in
            case cond
            of SOME c =>
               let val cp = elabExpr env' c
               in
                   { procs = Vector.concat [pprocs, #procs cp, #procs bodyProg]
                   , main = (pats', SOME (#main cp), #main bodyProg) }
               end
             | NONE =>
               { procs = Vector.concat [pprocs, #procs bodyProg]
               , main = (pats', NONE, #main bodyProg) }
        end

    and elabStmt env (CST.Def (pat, expr)) =
        let fun makeMain pat expr = Vector.fromList [Def (pat, expr)]
        in append makeMain (elabPat env pat) (elabExpr env expr)
        end
      | elabStmt env (CST.AugDef (pat, expr)) =
        let fun makeMain pat expr = Vector.fromList [AugDef (pat, expr)]
        in append makeMain (elabPat env pat) (elabExpr env expr)
        end
      | elabStmt env (CST.Expr expr) =
        mapMain (VectorExt.singleton o Expr) (elabExpr env expr)
    and elabStmts env stmts =
        let val env' = Env.pushBlockFrame env (stmtVecBindings stmts)
        in flatMap (elabStmt env') stmts
        end

    fun fromCST stmts = elabStmts Env.empty stmts
end
