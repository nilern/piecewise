signature HIGHER_ORDER_EXPR = sig
    structure Triv : TRIV

    datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * Triv.Var.t option * Name.t
                                               * ('prologue * 'expr) vector
                                         | Block of Pos.t * ('expr, 'stmt) Block.t
                                         | Call of Pos.t * 'expr * 'expr vector
                                         | PrimCall of Pos.t * Primop.t * 'expr vector
                                         | Triv of Pos.t * Triv.t

    val pos : ('e, 's, 'p) t -> Pos.t
    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('p -> PPrint.doc) -> ('e, 's, 'p) t
              -> PPrint.doc
end

(* TODO: parameterize over Argv, make sure params and args are consistent via `'name Argv.t` *)
functor HigherOrderExpr(RV : TO_DOC) : HIGHER_ORDER_EXPR = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    structure Triv = TrivFn(structure V = RV
                            structure C = Const)

    datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * Triv.Var.t option * Name.t
                                               * ('prologue * 'expr) vector
                                         | Block of Pos.t * ('expr, 'stmt) Block.t
                                         | Call of Pos.t * 'expr * 'expr vector
                                         | PrimCall of Pos.t * Primop.t * 'expr vector
                                         | Triv of Pos.t * Triv.t

    val pos = fn Fn (pos, _, _, _) => pos
               | Block (pos, _) => pos
               | Call (pos, _, _) => pos
               | PrimCall (pos, _, _) => pos
               | Triv (pos, _) => pos

    fun toDoc exprToDoc stmtToDoc prologueToDoc =
        fn Fn (_, name, params, cases) =>
           let fun caseToDoc (prologue, body) =
                   prologueToDoc prologue <+> PP.text "=>" <+> exprToDoc body
           in PP.braces (PP.align (OptionExt.toDoc Triv.Var.toDoc name <+> Name.toDoc params <$>
                                   (PP.punctuate (PP.semi ^^ PP.line)
                                                 (Vector.map caseToDoc cases))))
           end
         | Block (_, block) => PP.braces (PP.nest 4 (Block.toDoc exprToDoc stmtToDoc block))
         | Call (_, f, args) =>
           let fun step (arg, acc) = acc <+> exprToDoc arg
           in PP.parens (PP.align (Vector.foldl step (exprToDoc f) args))
           end
         | PrimCall (_, po, args) =>
           let fun step (arg, acc) = acc <+> exprToDoc arg
           in PP.parens (PP.align (Vector.foldl step (Primop.toDoc po) args))
           end
         | Triv (_, t) => Triv.toDoc t
end
