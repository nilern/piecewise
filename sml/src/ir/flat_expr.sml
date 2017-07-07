signature FLAT_EXPR = sig
    structure Var : TO_DOC

    type 'expr fnCase = 'expr vector * 'expr option * 'expr
    datatype ('expr, 'stmt) t = Block of Pos.t * 'stmt vector
                              | App of Pos.t * 'expr * 'expr vector
                              | PrimApp of Pos.t * Primop.t * 'expr vector
                              | Var of Pos.t * Var.t
                              | Const of Pos.t * Const.t

    val pos : ('expr, 'stmt) t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('e, 's) t
              -> PPrint.doc
end

functor FlatExpr(V : TO_DOC) :> FLAT_EXPR where type Var.t = V.t = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Var = V

    type 'expr fnCase = 'expr vector * 'expr option * 'expr
    datatype ('expr, 'stmt) t = Block of Pos.t * 'stmt vector
                              | App of Pos.t * 'expr * 'expr vector
                              | PrimApp of Pos.t * Primop.t * 'expr vector
                              | Var of Pos.t * Var.t
                              | Const of Pos.t * Const.t

    fun pos (Block (pos, _)) = pos
      | pos (App (pos, _, _)) = pos
      | pos (PrimApp (pos, _, _)) = pos
      | pos (Var (pos, _)) = pos
      | pos (Const (pos, _)) = pos

    fun toDoc _ stmtToDoc (Block (_, stmts)) =
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
