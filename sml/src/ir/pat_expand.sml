(* TODO: formals is guaranteed to be a tuple so matching it can be optimized *)

structure PatExpand :> sig
    val expand : CST0.stmt vector -> DnfCst.stmt vector

    exception Pattern of Pos.t * CST0.expr
end = struct
    val Def = Stmt1.Def
    val Expr = Stmt1.Expr
    val FixE = DnfCst.FixE
    val FixS = DnfCst.FixS
    val FixBS = DnfCst.FixBS

    exception Pattern of Pos.t * CST0.expr

    fun expandPat newBinding pat access parentId (cond, binds) =
        case CST0.unwrapE pat
        of Expr0.Fn _ => raise Pattern (CST0.exprPos pat, pat)
         | Expr0.Block _ => raise Pattern (CST0.exprPos pat, pat)
         | Expr0.App (pos, f, args) =>
           let val unapply =
                   FixE (Expr0.Var (pos, Var.Lex (Name.fromString "unapply")))
               val eq =
                   FixE (Expr0.Var (pos, Var.Lex (Name.fromString "==")))
               val f' = expandExpr f
               val access' =
                   FixE (Expr0.App (pos, unapply, Vector.fromList [f', access]))
               val tag =
                   FixE (Expr0.PrimApp (pos, Primop.Tag, VectorExt.singleton access'))
               val some = FixE (Expr0.Const (pos, Const.Symbol "Some"))
               val newCondExpr =
                   FixE (Expr0.App (pos, eq, Vector.fromList [tag, some]))
               val (newCond, parentId') =
                   DNF.require newCondExpr (OptionExt.toList parentId)
               val cond' = DNF.conj (Vector.fromList [cond, newCond])
               val access'' =
                   FixE (Expr0.PrimApp (pos, Primop.Repr,
                                        Vector.fromList [access']))
               fun expandArg (i, arg) =
                   let val pos = CST0.exprPos arg
                       val ie =
                           FixE (Expr0.Const (pos, Const.Int (Int.toString i)))
                       val argAccess =
                           FixE (Expr0.PrimApp (pos, Primop.AGet,
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
         | Expr0.PrimApp (_, po, args) => raise Fail "unimplemented"
         | Expr0.Var (_, var) =>
           (cond, VectorExt.conj binds (newBinding (var, access)))
         | Expr0.Const (pos, c) =>
           let val eq = FixE (Expr0.Var (pos, Var.Lex (Name.fromString "==")))
               val c' = FixE (Expr0.Const (pos, c))
               val newCondExpr =
                   FixE (Expr0.App (pos, eq, Vector.fromList [access, c']))
               val (newCond, _) =
                   DNF.require newCondExpr (OptionExt.toList parentId)
           in
               ( DNF.conj (Vector.fromList [cond, newCond])
               , binds )
           end

    and expandBind newBinding (CST0.Bind (pat, cond)) access =
        let val initial = (DNF.always (), VectorExt.empty ())
            val (cond, binds) = expandPat newBinding pat access NONE initial
        in
            DnfCst.Bind (CST0.exprPos pat, cond, binds)
        end

    and expandExpr (CST0.FixE expr) =
        FixE (case expr
              of Expr0.Fn (pos, formal, cases) =>
                 let val faccess = FixE (Expr0.Var (pos, Var.Lex formal))
                     fun expandCase (bind, body) =
                         (expandBind (FixBS o Stmt0.Def) bind faccess,
                          expandExpr body)
                 in
                     Expr0.Fn (pos, formal, Vector.map expandCase cases)
                 end
               | Expr0.Block (pos, stmts) =>
                 Expr0.Block (pos, Vector.map expandStmt stmts)
               | Expr0.App (pos, f, args) =>
                 Expr0.App (pos, expandExpr f, Vector.map expandExpr args)
               | Expr0.PrimApp (pos, po, args) =>
                 Expr0.PrimApp (pos, po, Vector.map expandExpr args)
               | Expr0.Var v => Expr0.Var v
               | Expr0.Const c => Expr0.Const c)

    and expandStmt (CST0.FixS stmt) =
        let fun expandDef newDef bind expr =
                case bind
                of CST0.Bind (CST0.FixE (Expr0.App (pos, f, args)), cond) =>
                   let val tuple =
                           CST0.FixE
                               (Expr0.Var
                                   (pos,
                                    Var.Lex (Name.freshFromString "tuple")))
                       val ftup = CST0.FixE (Expr0.App (pos, tuple, args))
                       val cs = (CST0.Bind (ftup, cond), expr)
                   in
                       expandDef newDef (CST0.Bind (f, NONE))
                           (CST0.FixE
                               (Expr0.Fn (pos, Name.freshFromString "formals",
                                         VectorExt.singleton cs)))
                   end
                 | _ =>
                   let val triv = Name.freshFromString "v"
                       val taccess =
                           FixE (Expr0.Var (CST0.exprPos expr, Var.Lex triv))
                   in
                       Def (triv, expandBind (FixBS o newDef) bind taccess,
                            expandExpr expr)
                   end
        in
            FixS
                (case stmt
                 of Stmt0.Def (bind, expr) =>
                    expandDef Stmt0.Def bind expr
                  | Stmt0.AugDef (bind, expr) =>
                    expandDef Stmt0.AugDef bind expr
                  | Stmt0.Expr e => Expr (expandExpr e))
        end

    val expand = Vector.map expandStmt
end
