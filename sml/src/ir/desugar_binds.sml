(* TODO: formals is guaranteed to be a tuple so matching it can be optimized *)

structure DesugarBinds :> sig
    val expand : Cst.stmt vector -> Ast.stmt vector

    exception Pattern of Pos.t * Cst.expr
end = struct
    val Def = AuglessStmt.Def
    val Expr = AuglessStmt.Expr
    val FixE = Ast.FixE
    val FixS = Ast.FixS
    val FixBS = Ast.FixBS

    exception Pattern of Pos.t * Cst.expr

    fun expandPat newBinding pat access parentId (cond, binds) =
        case Cst.unwrapE pat
        of Expr.Fn _ => raise Pattern (Cst.exprPos pat, pat)
         | Expr.Block _ => raise Pattern (Cst.exprPos pat, pat)
         | Expr.App (pos, f, args) =>
           let val unapply =
                   FixE (Expr.Var (pos, Var.Lex (Name.fromString "unapply")))
               val eq =
                   FixE (Expr.Var (pos, Var.Lex (Name.fromString "==")))
               val f' = expandExpr f
               val access' =
                   FixE (Expr.App (pos, unapply, Vector.fromList [f', access]))
               val tag =
                   FixE (Expr.PrimApp (pos, Primop.Tag, VectorExt.singleton access'))
               val some = FixE (Expr.Const (pos, Const.Symbol "Some"))
               val newCondExpr =
                   FixE (Expr.App (pos, eq, Vector.fromList [tag, some]))
               val (newCond, parentId') =
                   DNF.require newCondExpr (OptionExt.toList parentId)
               val cond' = DNF.conj (Vector.fromList [cond, newCond])
               val access'' =
                   FixE (Expr.PrimApp (pos, Primop.Repr,
                                        Vector.fromList [access']))
               fun expandArg (i, arg) =
                   let val pos = Cst.exprPos arg
                       val ie =
                           FixE (Expr.Const (pos, Const.Int (Int.toString i)))
                       val argAccess =
                           FixE (Expr.PrimApp (pos, Primop.AGet,
                                                Vector.fromList [access'', ie]))
                   in
                       expandPat newBinding arg argAccess (SOME parentId')
                                 (cond', VectorExt.empty ())
                   end
               val argCbs = Vector.mapi expandArg args
               val argConds = Vector.map #1 argCbs
               val cond'' = DNF.conj argConds
               fun step ((_, bs), acc) = VectorExt.concat acc bs
               val binds' = Vector.foldl step binds argCbs
           in
               (cond'', binds')
           end
         | Expr.PrimApp (_, po, args) => raise Fail "unimplemented"
         | Expr.Var (_, var) =>
           (cond, VectorExt.conj binds (newBinding (var, access)))
         | Expr.Const (pos, c) =>
           let val eq = FixE (Expr.Var (pos, Var.Lex (Name.fromString "==")))
               val c' = FixE (Expr.Const (pos, c))
               val newCondExpr =
                   FixE (Expr.App (pos, eq, Vector.fromList [access, c']))
               val (newCond, _) =
                   DNF.require newCondExpr (OptionExt.toList parentId)
           in
               ( DNF.conj (Vector.fromList [cond, newCond])
               , binds )
           end

    and expandBind newBinding (Cst.Bind (pat, cond)) access =
        let val initial = (DNF.always (), VectorExt.empty ())
            val (cond, binds) = expandPat newBinding pat access NONE initial
        in
            Ast.Bind (Cst.exprPos pat, cond, binds)
        end

    and expandExpr (Cst.FixE expr) =
        FixE (case expr
              of Expr.Fn (pos, formal, cases) =>
                 let val faccess = FixE (Expr.Var (pos, Var.Lex formal))
                     fun expandCase (bind, body) =
                         (expandBind (FixBS o CStmt.Def) bind faccess,
                          expandExpr body)
                 in
                     Expr.Fn (pos, formal, Vector.map expandCase cases)
                 end
               | Expr.Block (pos, stmts) =>
                 Expr.Block (pos, Vector.map expandStmt stmts)
               | Expr.App (pos, f, args) =>
                 Expr.App (pos, expandExpr f, Vector.map expandExpr args)
               | Expr.PrimApp (pos, po, args) =>
                 Expr.PrimApp (pos, po, Vector.map expandExpr args)
               | Expr.Var v => Expr.Var v
               | Expr.Const c => Expr.Const c)

    and expandStmt (Cst.FixS stmt) =
        let fun expandDef newDef bind expr =
                case bind
                of Cst.Bind (Cst.FixE (Expr.App (pos, f, args)), cond) =>
                   let val tuple =
                           Cst.FixE
                               (Expr.Var
                                   (pos,
                                    Var.Lex (Name.freshFromString "tuple")))
                       val ftup = Cst.FixE (Expr.App (pos, tuple, args))
                       val cs = (Cst.Bind (ftup, cond), expr)
                   in
                       expandDef newDef (Cst.Bind (f, NONE))
                           (Cst.FixE
                               (Expr.Fn (pos, Name.freshFromString "formals",
                                         VectorExt.singleton cs)))
                   end
                 | _ =>
                   let val triv = Name.freshFromString "v"
                       val taccess =
                           FixE (Expr.Var (Cst.exprPos expr, Var.Lex triv))
                   in
                       Def (triv, expandBind (FixBS o newDef) bind taccess,
                            expandExpr expr)
                   end
        in
            FixS
                (case stmt
                 of CStmt.Def (bind, expr) =>
                    expandDef CStmt.Def bind expr
                  | CStmt.AugDef (bind, expr) =>
                    expandDef CStmt.AugDef bind expr
                  | CStmt.Expr e => Expr (expandExpr e))
        end

    val expand = Vector.map expandStmt
end
