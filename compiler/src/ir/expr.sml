structure Expr :> sig
    datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * AVar.t option * Name.t
                                                       * ('prologue * 'expr) vector
                                         | Block of Pos.t * ('expr, 'stmt) Block.t
                                         | PrimApp of Pos.t * Primop.t * 'expr vector
                                         | Triv of Pos.t * ATriv.t

    val pos : ('e, 's, 'p) t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('p -> PPrint.doc) -> ('e, 's, 'p) t
              -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * AVar.t option * Name.t
                                                       * ('prologue * 'expr) vector
                                         | Block of Pos.t * ('expr, 'stmt) Block.t
                                         | PrimApp of Pos.t * Primop.t * 'expr vector
                                         | Triv of Pos.t * ATriv.t

    fun app fixExpr (pos, f, args) =
        let val argsExpr = fixExpr (PrimApp (pos, Primop.Tuple, args))
            val metaArgs = fixExpr (PrimApp (pos, Primop.Tuple, Vector.fromList [f, argsExpr]))
        in PrimApp (pos, Primop.Call, metaArgs)
        end

    fun pos (Fn (pos, _, _, _)) = pos
      | pos (Block (pos, _)) = pos
      | pos (PrimApp (pos, _, _)) = pos
      | pos (Triv (pos, _)) = pos

    fun toDoc exprToDoc stmtToDoc prologueToDoc =
        fn Fn (_, name, params, cases) =>
           let fun caseToDoc (prologue, body) =
                   prologueToDoc prologue <+> PP.text "=>" <+> exprToDoc body
           in PP.braces (PP.align (OptionExt.toDoc AVar.toDoc name <+> Name.toDoc params <$>
                                   (PP.punctuate (PP.semi ^^ PP.line)
                                                 (Vector.map caseToDoc cases))))
           end
         | Block (_, block) => PP.braces (PP.nest 4 (Block.toDoc exprToDoc stmtToDoc block))
         | PrimApp (_, po, args) =>
           let fun step (arg, acc) = acc <+> exprToDoc arg
           in PP.parens (PP.align (Vector.foldl step (Primop.toDoc po) args))
           end
         | Triv (_, t) => ATriv.toDoc t
end

signature FLAT_EXPR = sig
    structure Triv : TRIV

    datatype ('expr, 'stmt) t = Block of Pos.t * ('expr, 'stmt) Block.t
                              | PrimApp of Pos.t * Primop.t * 'expr vector
                              | Triv of Pos.t * Triv.t

    val pos : ('expr, 'stmt) t -> Pos.t

    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('e, 's) t -> PPrint.doc
end

functor FlatExprFn(T : TRIV) :> FLAT_EXPR where type Triv.t = T.t = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Triv = T

    datatype ('expr, 'stmt) t = Block of Pos.t * ('expr, 'stmt) Block.t
                              | PrimApp of Pos.t * Primop.t * 'expr vector
                              | Triv of Pos.t * Triv.t

    fun pos (Block (pos, _)) = pos
      | pos (PrimApp (pos, _, _)) = pos
      | pos (Triv (pos, _)) = pos

    fun toDoc exprToDoc stmtToDoc =
        fn Block (_, block) => PP.braces (PP.nest 4 (Block.toDoc exprToDoc stmtToDoc block))
         | PrimApp (_, po, args) =>
           let fun step (arg, acc) = acc <+> exprToDoc arg
           in PP.parens (PP.align (Vector.foldl step (Primop.toDoc po) args))
           end
         | Triv (_, t) => Triv.toDoc t
end

structure FlatExpr0 = FlatExprFn(FlatTriv0)
structure FlatExpr1 = FlatExprFn(FlatTriv1)
