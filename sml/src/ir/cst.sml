(* TODO: use plain strings instead of Name.t:s *)

structure CST = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    datatype expr = Fn of Pos.t * fnCase vector
                  | Block of Pos.t * stmt vector
                  | App of Pos.t * expr * expr vector
                  | PrimApp of Pos.t * Primop.t * expr vector
                  | Var of Pos.t * Var.t
                  | Const of Pos.t * Const.t

    and stmt = Def of expr * expr
             | AugDef of expr * expr
             | Expr of expr

    withtype fnCase = expr vector * expr option * expr

    fun exprPos (Fn (pos, _)) = pos
      | exprPos (Block (pos, _)) = pos
      | exprPos (App (pos, _, _)) = pos
      | exprPos (PrimApp (pos, _, _)) = pos
      | exprPos (Var (pos, _)) = pos
      | exprPos (Const (pos, _)) = pos

    fun stmtPos (Def (pat, _)) = exprPos pat
      | stmtPos (AugDef (pat, _)) = exprPos pat
      | stmtPos (Expr expr) = exprPos expr

    fun exprToString (Fn (_, cases)) =
            Vector.foldl (fn (c, acc) => acc ^ ";\n" ^ caseToString c)
                         "{" cases ^ "}"
      | exprToString (Block (_, stmts)) =
              Vector.foldl (fn (c, acc) => acc ^ ";\n" ^ stmtToString c)
                           "{" stmts ^ "}"
      | exprToString (App (_, f, args)) =
            "(" ^
                Vector.foldl (fn (arg, acc) => acc ^ " " ^ exprToString arg)
                             (exprToString f) args ^
                ")"
      | exprToString (PrimApp (_, opp, args)) =
            "(" ^
                Vector.foldl (fn (arg, acc) => acc ^ " " ^ exprToString arg)
                             (Primop.toString opp) args ^
                ")"
      | exprToString (Var (_, v)) = Var.toString v
      | exprToString (Const (_, c)) = Const.toString c

    and stmtToString (Def (pat, expr)) =
          exprToString pat ^ " = " ^ exprToString expr
      | stmtToString (AugDef (pat, expr)) =
            exprToString pat ^ " += " ^ exprToString expr
      | stmtToString (Expr expr) = exprToString expr

    and caseToString (pats, SOME cond, body) =
            Vector.foldl (fn (pat, acc) => acc ^ " " ^ exprToString pat)
                         "" pats ^ " | " ^ exprToString cond ^ " => " ^
                         exprToString body
      | caseToString (pats, NONE, body) =
              Vector.foldl (fn (pat, acc) => acc ^ " " ^ exprToString pat)
                           "" pats ^ " => " ^ exprToString body

    fun exprToDoc (Fn (_, cases)) =
            let fun caseToDoc ((pats, cond, body): fnCase) =
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
                            PP.lBrace ^^ PP.align caseDocs ^^ PP.rBrace
                        end
            end
      | exprToDoc (Block (_, stmts)) =
              (case Vector.length stmts
               of 1 => PP.braces (stmtToDoc (Vector.sub (stmts, 0)))
                | _ => let fun step (stmt, acc) =
                                   acc ^^ PP.semi <$> stmtToDoc stmt
                           val stmtDoc = stmtToDoc (Vector.sub (stmts, 0))
                           val rstmts = VectorSlice.slice(stmts, 1, NONE)
                           val stmtDocs = VectorSlice.foldl step stmtDoc rstmts
                       in
                           PP.lBrace ^^
                               PP.nest 4 (PP.line ^^ stmtDocs) ^^
                                   PP.line ^^ PP.rBrace
                       end)
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
      | exprToDoc (Const (_, c)) = Const.toDoc c
    and stmtToDoc (Def (pat, expr)) =
            exprToDoc pat <+> PP.text "=" <+> exprToDoc expr
      | stmtToDoc (AugDef (pat, expr)) =
            exprToDoc pat <+> PP.text "+=" <+> exprToDoc expr
      | stmtToDoc (Expr expr) = exprToDoc expr
end
