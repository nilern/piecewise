structure CExpr :> sig
    datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * CVar.t option * ('prologue * 'expr) vector
                                         | Block of Pos.t * ('expr, 'stmt) Block.t
                                         | App of Pos.t * 'expr * 'expr vector
                                         | PrimApp of Pos.t * Primop.t * 'expr vector
                                         | Triv of Pos.t * CTriv.t

    val pos : ('e, 's, 'p) t -> Pos.t
    val toDoc : ('e -> PPrint.doc) -> ('s -> PPrint.doc) -> ('p -> PPrint.doc) -> ('e, 's, 'p) t
              -> PPrint.doc
end = struct
    structure PP = PPrint
    val op^^ = PP.^^
    val op<+> = PP.<+>
    val op<$> = PP.<$>

    datatype ('expr, 'stmt, 'prologue) t = Fn of Pos.t * CVar.t option * ('prologue * 'expr) vector
                                         | Block of Pos.t * ('expr, 'stmt) Block.t
                                         | App of Pos.t * 'expr * 'expr vector
                                         | PrimApp of Pos.t * Primop.t * 'expr vector
                                         | Triv of Pos.t * CTriv.t

    val pos = fn Fn (pos, _, _) => pos
               | Block (pos, _) => pos
               | App (pos, _, _) => pos
               | PrimApp (pos, _, _) => pos
               | Triv (pos, _) => pos

    fun toDoc exprToDoc stmtToDoc prologueToDoc =
        fn Fn (_, name, cases) =>
           let fun caseToDoc (prologue, body) =
                   prologueToDoc prologue <+> PP.text "=>" <+> exprToDoc body
           in PP.braces (PP.align (OptionExt.toDoc CVar.toDoc name <$>
                                   (PP.punctuate (PP.semi ^^ PP.line)
                                                 (Vector.map caseToDoc cases))))
           end
         | Block (_, block) => PP.braces (PP.nest 4 (Block.toDoc exprToDoc stmtToDoc block))
         | App (_, f, args) =>
           let fun step (arg, acc) = acc <+> exprToDoc arg
           in PP.parens (PP.align (Vector.foldl step (exprToDoc f) args))
           end
         | PrimApp (_, po, args) =>
           let fun step (arg, acc) = acc <+> exprToDoc arg
           in PP.parens (PP.align (Vector.foldl step (Primop.toDoc po) args))
           end
         | Triv (_, t) => CTriv.toDoc t
end

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

signature CPS_EXPR = sig
    structure Triv : TRIV

    datatype t = PrimApp of Pos.t * Primop.t * Triv.t vector
               | Guard of Pos.t * int DNF.t
               | Triv of Pos.t * Triv.t

    val pos : t -> Pos.t
    val toDoc : t -> PPrint.doc
end

functor CpsExprFn(T : TRIV)
:> CPS_EXPR where type Triv.t = T.t
                  and type Triv.Var.t = T.Var.t
                  and type Triv.Const.t = T.Const.t = struct
    structure PP = PPrint
    val op<+> = PP.<+>

    structure Triv = T

    datatype t = PrimApp of Pos.t * Primop.t * Triv.t vector
               | Guard of Pos.t * int DNF.t
               | Triv of Pos.t * Triv.t

    val pos = fn PrimApp (pos, _, _) => pos
               | Guard (pos, _) => pos
               | Triv (pos, _) => pos

    val toDoc =
        fn PrimApp (_, po, args) =>
           PP.parens (PP.punctuate PP.space
                      (VectorExt.prepend (Vector.map Triv.toDoc args) (Primop.toDoc po)))
         | Guard (_, dnf) => PP.text "@guard" <+> PP.parens (DNF.toDoc PP.int dnf)
         | Triv (_, t) => Triv.toDoc t
end

structure CpsExpr0 = CpsExprFn(FlatTriv1)
