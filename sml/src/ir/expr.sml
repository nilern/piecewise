structure Expr :> sig
    datatype ('expr, 'stmt, 'bind, 'prologue) t = Fn of Pos.t * Name.t * ('prologue * 'expr) vector
                                                | Block of Pos.t * 'stmt vector
                                                | App of Pos.t * 'expr * 'expr vector
                                                | PrimApp of Pos.t * Primop.t * 'expr vector
                                                | Var of Pos.t * Var.t
                                                | Const of Pos.t * Const.t

    val pos : ('e, 's, 'b, 'p) t -> Pos.t

    val stmtsToDoc : ('s -> PPrint.doc) -> 's vector -> PPrint.doc
    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('b -> PPrint.doc) -> ('p -> PPrint.doc)
              -> ('e, 's, 'b, 'p) t -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    datatype ('expr, 'stmt, 'bind, 'prologue) t = Fn of Pos.t * Name.t * ('prologue * 'expr) vector
                                                | Block of Pos.t * 'stmt vector
                                                | App of Pos.t * 'expr * 'expr vector
                                                | PrimApp of Pos.t * Primop.t * 'expr vector
                                                | Var of Pos.t * Var.t
                                                | Const of Pos.t * Const.t

    fun pos (Fn (pos, _, _)) = pos
      | pos (Block (pos, _)) = pos
      | pos (App (pos, _, _)) = pos
      | pos (PrimApp (pos, _, _)) = pos
      | pos (Var (pos, _)) = pos
      | pos (Const (pos, _)) = pos

    fun stmtsToDoc stmtToDoc stmts =
        case Vector.length stmts
         of 1 => PP.braces (stmtToDoc (Vector.sub (stmts, 0)))
          | _ => let fun step (stmt, acc) = acc ^^ PP.semi <$> stmtToDoc stmt
                     val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                     val rstmts = VectorSlice.slice(stmts, 1, NONE)
                     val stmtDocs = VectorSlice.foldl step stmtDoc rstmts
                 in
                     PP.lBrace ^^
                         PP.nest 4 (PP.line ^^ stmtDocs) ^^
                             PP.line ^^ PP.rBrace
                 end

    fun toDoc toDoc' _ bindToDoc prologueToDoc (Fn (_, formals, cases)) =
        let fun caseToDoc (prologue, body) =
                prologueToDoc prologue <+> PP.text "=>" <+> toDoc' body
            val c = Vector.sub (cases, 0)
            val rcs = VectorSlice.slice(cases, 1, NONE)
            fun step (cs, acc) = acc ^^ PP.semi <$> caseToDoc cs
        in
            PP.braces
                (PP.align
                    (PP.text "|" ^^ Name.toDoc formals ^^ PP.text "|" <$>
                        VectorSlice.foldl step (caseToDoc c) rcs))
        end
      | toDoc _ stmtToDoc _ _ (Block (_, stmts)) = stmtsToDoc stmtToDoc stmts
      | toDoc toDoc' _ _ _ (App (_, f, args)) =
        let fun step (arg, acc) = acc <+> toDoc' arg
            val argDocs = Vector.foldl step (toDoc' f) args
        in PP.parens (PP.align argDocs)
        end
      | toDoc toDoc' _ _ _ (PrimApp (_, po, args)) =
        let fun step (arg, acc) = acc <+> toDoc' arg
            val argDocs = Vector.foldl step (Primop.toDoc po) args
        in PP.parens (PP.align argDocs)
        end
      | toDoc _ _ _ _ (Var (_, v)) = Var.toDoc v
      | toDoc _ _ _ _ (Const (_, c)) = Const.toDoc c
end
