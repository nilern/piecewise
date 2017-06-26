structure Expr0 :> sig
    type 'expr fnCase = 'expr vector * 'expr option * 'expr
    datatype ('expr, 'stmt) t = Fn of Pos.t * 'expr fnCase vector
                              | Block of Pos.t * 'stmt vector
                              | App of Pos.t * 'expr * 'expr vector
                              | PrimApp of Pos.t * Primop.t * 'expr vector
                              | Var of Pos.t * Var.t
                              | Const of Pos.t * Const.t

    val pos : ('expr, 'stmt) t -> Pos.t

    val toString : ('e -> string) -> ('s -> string) -> ('e, 's) t -> string

    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('e, 's) t
              -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    type 'expr fnCase = 'expr vector * 'expr option * 'expr
    datatype ('expr, 'stmt) t = Fn of Pos.t * 'expr fnCase vector
                              | Block of Pos.t * 'stmt vector
                              | App of Pos.t * 'expr * 'expr vector
                              | PrimApp of Pos.t * Primop.t * 'expr vector
                              | Var of Pos.t * Var.t
                              | Const of Pos.t * Const.t

    fun pos (Fn (pos, _)) = pos
      | pos (Block (pos, _)) = pos
      | pos (App (pos, _, _)) = pos
      | pos (PrimApp (pos, _, _)) = pos
      | pos (Var (pos, _)) = pos
      | pos (Const (pos, _)) = pos

    fun toString toString' _ (Fn (_, cases)) =
        let fun caseToString (pats, SOME cond, body) =
                Vector.foldl (fn (pat, acc) => acc ^ " " ^ toString' pat)
                             "" pats ^ " | " ^ toString' cond ^
                             " => " ^ toString' body
              | caseToString (pats, NONE, body) =
                Vector.foldl (fn (pat, acc) => acc ^ " " ^ toString' pat)
                             "" pats ^ " => " ^ toString' body
        in
            Vector.foldl (fn (c, acc) => acc ^ ";\n" ^ caseToString c)
                         "{" cases ^ "}"
        end
      | toString _ stmtToString (Block (_, stmts)) =
        Vector.foldl (fn (c, acc) => acc ^ ";\n" ^ stmtToString c)
                     "{" stmts ^ "}"
      | toString toString' _ (App (_, f, args)) =
        "(" ^
            Vector.foldl (fn (arg, acc) => acc ^ " " ^ toString' arg)
                         (toString' f) args ^
            ")"
      | toString toString' _ (PrimApp (_, opp, args)) =
        "(" ^
            Vector.foldl (fn (arg, acc) => acc ^ " " ^ toString' arg)
                         (Primop.toString opp) args ^
            ")"
      | toString _ _ (Var (_, v)) = Var.toString v
      | toString _ _ (Const (_, c)) = Const.toString c

    fun toDoc toDoc' _ (Fn (_, cases)) =
        let fun caseToDoc (pats, cond, body) =
                let fun step (pat, acc) = acc <+> toDoc' pat
                    val patDoc = toDoc' (Vector.sub (pats, 0))
                    val rpats = VectorSlice.slice(pats, 1, NONE)
                    val patsDoc = VectorSlice.foldl step patDoc rpats
                    val condDoc = case cond
                                  of SOME ce =>
                                         PP.space ^^ PP.text "|" <+>
                                             toDoc' ce
                                   | NONE => PP.empty
                    val bodyDoc = toDoc' body
                in
                    (patsDoc ^^ condDoc) <+> PP.text "=>" <+> bodyDoc
                end
        in case Vector.length cases
            of 1 => PP.braces (caseToDoc (Vector.sub (cases, 0)))
             | _ => let fun step (cs, acc) = acc ^^ PP.semi <$> caseToDoc cs
                        val caseDoc = caseToDoc (Vector.sub (cases, 0))
                        val rcases = VectorSlice.slice(cases, 1, NONE)
                        val caseDocs = VectorSlice.foldl step caseDoc rcases
                    in
                        PP.lBrace ^^ PP.align caseDocs ^^ PP.rBrace
                    end
        end
      | toDoc _ stmtToDoc (Block (_, stmts)) =
        (case Vector.length stmts
         of 1 => PP.braces (stmtToDoc (Vector.sub (stmts, 0)))
          | _ => let fun step (stmt, acc) = acc ^^ PP.semi <$> stmtToDoc stmt
                     val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                     val rstmts = VectorSlice.slice(stmts, 1, NONE)
                     val stmtDocs = VectorSlice.foldl step stmtDoc rstmts
                 in
                     PP.lBrace ^^
                         PP.nest 4 (PP.line ^^ stmtDocs) ^^
                             PP.line ^^ PP.rBrace
                 end)
      | toDoc toDoc' _ (App (_, f, args)) =
        let fun step (arg, acc) = acc <+> toDoc' arg
            val argDocs = Vector.foldl step (toDoc' f) args
        in PP.parens (PP.align argDocs)
        end
      | toDoc toDoc' _ (PrimApp (_, po, args)) =
        let fun step (arg, acc) = acc <+> toDoc' arg
            val argDocs = Vector.foldl step (Primop.toDoc po) args
        in PP.parens (PP.align argDocs)
        end
      | toDoc _ _ (Var (_, v)) = Var.toDoc v
      | toDoc _ _ (Const (_, c)) = Const.toDoc c
end
